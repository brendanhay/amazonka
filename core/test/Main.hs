{-# LANGUAGE TemplateHaskell #-}

-- |
-- Module      : Main
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Main (main) where

import qualified Test.AWS.Data.Base64  as Base64
import qualified Test.AWS.Data.List    as List
import qualified Test.AWS.Data.Maybe   as Maybe
import qualified Test.AWS.Data.Numeric as Numeric
import qualified Test.AWS.Data.Path    as Path
import qualified Test.AWS.Data.Time    as Time
import qualified Test.AWS.Error        as Error
import qualified Test.AWS.Sign.V4      as V4
import           Test.Tasty

main :: IO ()
main = defaultMain $
    testGroup "amazonka-core"
        [ testGroup "primitives"
            [ Numeric.tests
            , Time.tests
            , Base64.tests
            , Maybe.tests
            ]

        , testGroup "paths"
            [ Path.tests
            ]

        , testGroup "collections"
            [ List.tests
            ]

        , testGroup "signing"
            [ V4.tests
            ]

        , Error.tests
        ]
