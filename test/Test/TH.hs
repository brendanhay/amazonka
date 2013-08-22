{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

-- |
-- Module      : Test.TH
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Test.TH
    (
    -- * Specific Derivations
      deriveResponse

    -- * Individual Derivations
    , deriveJSON
    , deriveArbitrary
    , deriveTemplate
    ) where

import           Control.Monad
import qualified Data.Aeson.TH         as Aeson
import qualified Data.ByteString.Char8 as BS
import qualified Data.DeriveTH         as Derive
import           Data.List
import           Language.Haskell.TH
import           Network.AWS.Internal
import           Paths_aws_haskell     (getDataFileName)

deriveResponse :: FilePath -> [Name] -> Q [Dec]
deriveResponse path names = liftM concat $ mapM ($ names)
     [ deriveJSON
     , deriveArbitrary
     , deriveTemplate path
     ]

deriveJSON :: [Name] -> Q [Dec]
deriveJSON = liftM concat . mapM (Aeson.deriveToJSON id)

deriveArbitrary :: [Name] -> Q [Dec]
deriveArbitrary = Derive.derives [Derive.makeArbitrary]

deriveTemplate :: FilePath -> [Name] -> Q [Dec]
deriveTemplate path = liftM concat . mapM derive
  where
    derive n = embedTemplate (dropSuffix "/" path ++ "/" ++ nameBase n) n

--
-- Internal
--

embedTemplate :: FilePath -> Name -> Q [Dec]
embedTemplate path name =
    [d|instance Template $(conT name) where
           readTemplate _ = $(template >>= embed)|]
  where
    template = runIO $
        getDataFileName (path ++ ".tmpl") >>= BS.readFile

    embed bstr = do
        pack <- [| BS.pack |]
        return $! AppE pack $! LitE $! StringL $! BS.unpack bstr

suffix :: String -> String
suffix str = map rep $ drop idx str
  where
    idx = (+ 1) $ reverse (elemIndices '.' str) !! 2

    rep '.' = '/'
    rep  c  = c
