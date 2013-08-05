{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE QuasiQuotes       #-}

-- |
-- Module      : Network.AWS.Internal.TH
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.TH
    ( deriveTmpl
    , deriveQS
    , deriveQS'
    ) where

import           Control.Applicative
import           Data.Aeson.TH
import qualified Data.ByteString.Char8       as BS
import           Data.Char                   (toLower)
import           Data.Monoid
import           Language.Haskell.TH
import           Language.Haskell.TH.Syntax
import           Network.AWS.Internal.String
import           Network.AWS.Internal.Types
import           Paths_aws_haskell           (getDataFileName)

deriveTmpl :: (String -> String) -> Name -> Q [Dec]
deriveTmpl f name = concat <$> sequence
    [ deriveToJSON f name
    , embedTemplate name
    ]

deriveQS :: Name -> Q [Dec]
deriveQS name = deriveQS' (dropPrefix $ toLower x : xs) name
  where
    (x : xs) = nameBase name

deriveQS' :: (String -> String) -> Name -> Q [Dec]
deriveQS' f name = do
    ref <- reify name

    case ref of
        TyConI (DataD _ _ _ [RecC _ fields] _) -> do
            let names   = map (\(n, _, _) -> n) fields
                field n = [| queryParam s . $(global n) |]
                  where
                    s = BS.pack . f $ nameBase n
                query   = listE $ map field names

            [d|instance QueryString $(conT name) where
                   queryString x = concatMap ($ x) $query|]
        _ ->
            [d|instance QueryString $(conT name) where
                   queryString _ = []|]

--
-- Internal
--

instance Lift BS.ByteString where
    lift = return . LitE . StringL . BS.unpack

-- | template/<NameOfModule>/<Type>
embedTemplate :: Name -> Q [Dec]
embedTemplate name =
    [d|instance Template $(conT name) where
           readTemplate _ = $(readTemplate' (suffix $ show name))|]

readTemplate' :: FilePath -> Q Exp
readTemplate' name =
    (runIO $ getDataFileName ("template/" <> name) >>= BS.readFile) >>= bsExp
  where
    bsExp bs = do
        pack <- [| BS.pack |]
        return $! AppE pack $! LitE $! StringL $! BS.unpack bs
