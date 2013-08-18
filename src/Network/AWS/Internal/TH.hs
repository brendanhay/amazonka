{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
b
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
    (
    -- * Template Instances
      embedTemplate
    ) where

import qualified Data.ByteString.Char8      as BS
import           Data.List
import           Data.Monoid
import           Language.Haskell.TH
import           Network.AWS.Internal.Types
import           Paths_aws_haskell          (getDataFileName)

embedTemplate :: Name -> Q [Dec]
embedTemplate name =
    [d|instance Template $(conT name) where
           readTemplate _ = $(template >>= embed)|]
  where
    template = runIO $ getDataFileName
        ("template/" <> suffix (show name) <> ".tmpl") >>= BS.readFile

    embed bstr = do
        pack <- [| BS.pack |]
        return $! AppE pack $! LitE $! StringL $! BS.unpack bstr

-- deriveQS :: Name -> Q [Dec]
-- deriveQS = deriveQS' dropLower

-- deriveQS' :: (String -> String) -> Name -> Q [Dec]
-- deriveQS' f name = reify name >>= derive
--   where
--     derive :: Info -> DecsQ

--     derive (TyConI (DataD _ _ _ [RecC _ fields] _)) = do
--         let field n = [| \k -> queryString (k <> s) . $(global n) |]
--               where s = toBS . f $ nameBase n
--             names = map (\(n, _, _) -> n) fields
--             query = listE $ map field names
--         [d|instance QueryString $(conT name) where
--                queryString k v = concatMap (($ v) . ($ k)) $query|]

--     derive (TyConI (DataD _ _ _ _ _)) = do
--         [d|instance QueryString $(conT name) where
--                queryString _ _ = []|]

--     derive (TyConI (NewtypeD _ _ _ (NormalC ctor [(_, AppT ListT (ConT typ))]) _)) = (:[]) `fmap` do
--         instanceD (cxt []) (conT ''QueryString `appT` conT name)
--             [ funD 'queryString . (:[]) $ do
--                  k  <- newName "k"
--                  vs <- newName "vs"
--                  clause [varP k, conP ctor [varP vs]]
--                      (normalB [|
--                          let z n = queryString ($(global k) <> key <> "." <> toBS n <> ".")
--                          in concat (zipWith z ([1..] :: [Integer]) $(global vs))
--                      |]) []
--             ]
--       where
--         key = toBS . f $ nameBase ctor

--     derive (TyConI (NewtypeD _ _ _ (NormalC ctor [field]) _)) = (:[]) `fmap` do
--        instanceD (cxt []) (conT ''QueryString `appT` conT name)
--            [ funD 'queryString . (:[]) $ do
--                 k <- newName "k"
--                 v <- newName "v"
--                 clause [varP k, conP ctor [varP v]]
--                     (normalB [|queryString ($(global k) <> key) $(global v)|]) []
--            ]
--      where
--        key = toBS . f $ nameBase ctor

--     derive (TyConI (TySynD _ _ (AppT ListT (ConT typ)))) = do
--         [d|instance QueryString [$(conT typ)] where
--                queryString k = concat . zipWith z ([1..] :: [Integer])
--                  where
--                    z n = queryString (k <> key <> "." <> toBS n <> ".")|]
--       where
--         key = toBS . f $ nameBase name

--     derive err = error $ "Cannot derive QueryString instance for: " ++ show err

-- deriveXML :: Name -> Q [Dec]
-- deriveXML name = liftM2 (++)
--     (deriveJSON fieldOptions name)
--     ([d|instance FromXML $(conT name)|])

-- options, fieldOptions, loweredFieldOptions, underscoredFieldOptions :: Options
-- options                 = defaultOptions
-- fieldOptions            = options { fieldLabelModifier = dropLower }
-- loweredFieldOptions     = options { fieldLabelModifier = lowerAll . dropLower }
-- underscoredFieldOptions = options { fieldLabelModifier = underscore . dropLower }

--
-- Internal
--

suffix :: String -> String
suffix str = map rep $ drop idx str
  where
    idx = (+ 1) $ reverse (elemIndices '.' str) !! 2

    rep '.' = '/'
    rep  c  = c
