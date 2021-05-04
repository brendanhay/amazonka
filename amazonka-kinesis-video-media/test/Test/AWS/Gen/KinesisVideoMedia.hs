{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.KinesisVideoMedia
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.KinesisVideoMedia where

import Data.Proxy
import Network.AWS.KinesisVideoMedia
import Test.AWS.Fixture
import Test.AWS.KinesisVideoMedia.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetMedia $
--             newGetMedia
--
--           ]

--     , testGroup "response"
--         [ responseGetMedia $
--             newGetMediaResponse
--
--           ]
--     ]

-- Requests

requestGetMedia :: GetMedia -> TestTree
requestGetMedia =
  req
    "GetMedia"
    "fixture/GetMedia.yaml"

-- Responses
