{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.FinSpaceData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.FinSpaceData where

import Data.Proxy
import Network.AWS.FinSpaceData
import Test.AWS.FinSpaceData.Internal
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateChangeset $
--             newCreateChangeset
--
--         , requestGetProgrammaticAccessCredentials $
--             newGetProgrammaticAccessCredentials
--
--         , requestGetWorkingLocation $
--             newGetWorkingLocation
--
--           ]

--     , testGroup "response"
--         [ responseCreateChangeset $
--             newCreateChangesetResponse
--
--         , responseGetProgrammaticAccessCredentials $
--             newGetProgrammaticAccessCredentialsResponse
--
--         , responseGetWorkingLocation $
--             newGetWorkingLocationResponse
--
--           ]
--     ]

-- Requests

requestCreateChangeset :: CreateChangeset -> TestTree
requestCreateChangeset =
  req
    "CreateChangeset"
    "fixture/CreateChangeset.yaml"

requestGetProgrammaticAccessCredentials :: GetProgrammaticAccessCredentials -> TestTree
requestGetProgrammaticAccessCredentials =
  req
    "GetProgrammaticAccessCredentials"
    "fixture/GetProgrammaticAccessCredentials.yaml"

requestGetWorkingLocation :: GetWorkingLocation -> TestTree
requestGetWorkingLocation =
  req
    "GetWorkingLocation"
    "fixture/GetWorkingLocation.yaml"

-- Responses

responseCreateChangeset :: CreateChangesetResponse -> TestTree
responseCreateChangeset =
  res
    "CreateChangesetResponse"
    "fixture/CreateChangesetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateChangeset)

responseGetProgrammaticAccessCredentials :: GetProgrammaticAccessCredentialsResponse -> TestTree
responseGetProgrammaticAccessCredentials =
  res
    "GetProgrammaticAccessCredentialsResponse"
    "fixture/GetProgrammaticAccessCredentialsResponse.proto"
    defaultService
    (Proxy :: Proxy GetProgrammaticAccessCredentials)

responseGetWorkingLocation :: GetWorkingLocationResponse -> TestTree
responseGetWorkingLocation =
  res
    "GetWorkingLocationResponse"
    "fixture/GetWorkingLocationResponse.proto"
    defaultService
    (Proxy :: Proxy GetWorkingLocation)
