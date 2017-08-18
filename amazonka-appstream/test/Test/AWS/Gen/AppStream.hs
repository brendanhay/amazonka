{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AppStream
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.AppStream where

import Data.Proxy
import Test.AWS.Fixture
import Test.AWS.Prelude
import Test.Tasty
import Network.AWS.AppStream
import Test.AWS.AppStream.Internal

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestDisassociateFleet $
--             disassociateFleet
--
--         , requestListAssociatedFleets $
--             listAssociatedFleets
--
--         , requestDeleteStack $
--             deleteStack
--
--         , requestUpdateStack $
--             updateStack
--
--         , requestCreateDirectoryConfig $
--             createDirectoryConfig
--
--         , requestListAssociatedStacks $
--             listAssociatedStacks
--
--         , requestDeleteFleet $
--             deleteFleet
--
--         , requestUpdateFleet $
--             updateFleet
--
--         , requestAssociateFleet $
--             associateFleet
--
--         , requestDescribeDirectoryConfigs $
--             describeDirectoryConfigs
--
--         , requestDescribeSessions $
--             describeSessions
--
--         , requestDescribeStacks $
--             describeStacks
--
--         , requestDescribeFleets $
--             describeFleets
--
--         , requestStopFleet $
--             stopFleet
--
--         , requestDeleteDirectoryConfig $
--             deleteDirectoryConfig
--
--         , requestUpdateDirectoryConfig $
--             updateDirectoryConfig
--
--         , requestCreateFleet $
--             createFleet
--
--         , requestCreateStack $
--             createStack
--
--         , requestExpireSession $
--             expireSession
--
--         , requestCreateStreamingURL $
--             createStreamingURL
--
--         , requestStartFleet $
--             startFleet
--
--         , requestDescribeImages $
--             describeImages
--
--           ]

--     , testGroup "response"
--         [ responseDisassociateFleet $
--             disassociateFleetResponse
--
--         , responseListAssociatedFleets $
--             listAssociatedFleetsResponse
--
--         , responseDeleteStack $
--             deleteStackResponse
--
--         , responseUpdateStack $
--             updateStackResponse
--
--         , responseCreateDirectoryConfig $
--             createDirectoryConfigResponse
--
--         , responseListAssociatedStacks $
--             listAssociatedStacksResponse
--
--         , responseDeleteFleet $
--             deleteFleetResponse
--
--         , responseUpdateFleet $
--             updateFleetResponse
--
--         , responseAssociateFleet $
--             associateFleetResponse
--
--         , responseDescribeDirectoryConfigs $
--             describeDirectoryConfigsResponse
--
--         , responseDescribeSessions $
--             describeSessionsResponse
--
--         , responseDescribeStacks $
--             describeStacksResponse
--
--         , responseDescribeFleets $
--             describeFleetsResponse
--
--         , responseStopFleet $
--             stopFleetResponse
--
--         , responseDeleteDirectoryConfig $
--             deleteDirectoryConfigResponse
--
--         , responseUpdateDirectoryConfig $
--             updateDirectoryConfigResponse
--
--         , responseCreateFleet $
--             createFleetResponse
--
--         , responseCreateStack $
--             createStackResponse
--
--         , responseExpireSession $
--             expireSessionResponse
--
--         , responseCreateStreamingURL $
--             createStreamingURLResponse
--
--         , responseStartFleet $
--             startFleetResponse
--
--         , responseDescribeImages $
--             describeImagesResponse
--
--           ]
--     ]

-- Requests

requestDisassociateFleet :: DisassociateFleet -> TestTree
requestDisassociateFleet = req
    "DisassociateFleet"
    "fixture/DisassociateFleet.yaml"

requestListAssociatedFleets :: ListAssociatedFleets -> TestTree
requestListAssociatedFleets = req
    "ListAssociatedFleets"
    "fixture/ListAssociatedFleets.yaml"

requestDeleteStack :: DeleteStack -> TestTree
requestDeleteStack = req
    "DeleteStack"
    "fixture/DeleteStack.yaml"

requestUpdateStack :: UpdateStack -> TestTree
requestUpdateStack = req
    "UpdateStack"
    "fixture/UpdateStack.yaml"

requestCreateDirectoryConfig :: CreateDirectoryConfig -> TestTree
requestCreateDirectoryConfig = req
    "CreateDirectoryConfig"
    "fixture/CreateDirectoryConfig.yaml"

requestListAssociatedStacks :: ListAssociatedStacks -> TestTree
requestListAssociatedStacks = req
    "ListAssociatedStacks"
    "fixture/ListAssociatedStacks.yaml"

requestDeleteFleet :: DeleteFleet -> TestTree
requestDeleteFleet = req
    "DeleteFleet"
    "fixture/DeleteFleet.yaml"

requestUpdateFleet :: UpdateFleet -> TestTree
requestUpdateFleet = req
    "UpdateFleet"
    "fixture/UpdateFleet.yaml"

requestAssociateFleet :: AssociateFleet -> TestTree
requestAssociateFleet = req
    "AssociateFleet"
    "fixture/AssociateFleet.yaml"

requestDescribeDirectoryConfigs :: DescribeDirectoryConfigs -> TestTree
requestDescribeDirectoryConfigs = req
    "DescribeDirectoryConfigs"
    "fixture/DescribeDirectoryConfigs.yaml"

requestDescribeSessions :: DescribeSessions -> TestTree
requestDescribeSessions = req
    "DescribeSessions"
    "fixture/DescribeSessions.yaml"

requestDescribeStacks :: DescribeStacks -> TestTree
requestDescribeStacks = req
    "DescribeStacks"
    "fixture/DescribeStacks.yaml"

requestDescribeFleets :: DescribeFleets -> TestTree
requestDescribeFleets = req
    "DescribeFleets"
    "fixture/DescribeFleets.yaml"

requestStopFleet :: StopFleet -> TestTree
requestStopFleet = req
    "StopFleet"
    "fixture/StopFleet.yaml"

requestDeleteDirectoryConfig :: DeleteDirectoryConfig -> TestTree
requestDeleteDirectoryConfig = req
    "DeleteDirectoryConfig"
    "fixture/DeleteDirectoryConfig.yaml"

requestUpdateDirectoryConfig :: UpdateDirectoryConfig -> TestTree
requestUpdateDirectoryConfig = req
    "UpdateDirectoryConfig"
    "fixture/UpdateDirectoryConfig.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet = req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

requestCreateStack :: CreateStack -> TestTree
requestCreateStack = req
    "CreateStack"
    "fixture/CreateStack.yaml"

requestExpireSession :: ExpireSession -> TestTree
requestExpireSession = req
    "ExpireSession"
    "fixture/ExpireSession.yaml"

requestCreateStreamingURL :: CreateStreamingURL -> TestTree
requestCreateStreamingURL = req
    "CreateStreamingURL"
    "fixture/CreateStreamingURL.yaml"

requestStartFleet :: StartFleet -> TestTree
requestStartFleet = req
    "StartFleet"
    "fixture/StartFleet.yaml"

requestDescribeImages :: DescribeImages -> TestTree
requestDescribeImages = req
    "DescribeImages"
    "fixture/DescribeImages.yaml"

-- Responses

responseDisassociateFleet :: DisassociateFleetResponse -> TestTree
responseDisassociateFleet = res
    "DisassociateFleetResponse"
    "fixture/DisassociateFleetResponse.proto"
    appStream
    (Proxy :: Proxy DisassociateFleet)

responseListAssociatedFleets :: ListAssociatedFleetsResponse -> TestTree
responseListAssociatedFleets = res
    "ListAssociatedFleetsResponse"
    "fixture/ListAssociatedFleetsResponse.proto"
    appStream
    (Proxy :: Proxy ListAssociatedFleets)

responseDeleteStack :: DeleteStackResponse -> TestTree
responseDeleteStack = res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse.proto"
    appStream
    (Proxy :: Proxy DeleteStack)

responseUpdateStack :: UpdateStackResponse -> TestTree
responseUpdateStack = res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    appStream
    (Proxy :: Proxy UpdateStack)

responseCreateDirectoryConfig :: CreateDirectoryConfigResponse -> TestTree
responseCreateDirectoryConfig = res
    "CreateDirectoryConfigResponse"
    "fixture/CreateDirectoryConfigResponse.proto"
    appStream
    (Proxy :: Proxy CreateDirectoryConfig)

responseListAssociatedStacks :: ListAssociatedStacksResponse -> TestTree
responseListAssociatedStacks = res
    "ListAssociatedStacksResponse"
    "fixture/ListAssociatedStacksResponse.proto"
    appStream
    (Proxy :: Proxy ListAssociatedStacks)

responseDeleteFleet :: DeleteFleetResponse -> TestTree
responseDeleteFleet = res
    "DeleteFleetResponse"
    "fixture/DeleteFleetResponse.proto"
    appStream
    (Proxy :: Proxy DeleteFleet)

responseUpdateFleet :: UpdateFleetResponse -> TestTree
responseUpdateFleet = res
    "UpdateFleetResponse"
    "fixture/UpdateFleetResponse.proto"
    appStream
    (Proxy :: Proxy UpdateFleet)

responseAssociateFleet :: AssociateFleetResponse -> TestTree
responseAssociateFleet = res
    "AssociateFleetResponse"
    "fixture/AssociateFleetResponse.proto"
    appStream
    (Proxy :: Proxy AssociateFleet)

responseDescribeDirectoryConfigs :: DescribeDirectoryConfigsResponse -> TestTree
responseDescribeDirectoryConfigs = res
    "DescribeDirectoryConfigsResponse"
    "fixture/DescribeDirectoryConfigsResponse.proto"
    appStream
    (Proxy :: Proxy DescribeDirectoryConfigs)

responseDescribeSessions :: DescribeSessionsResponse -> TestTree
responseDescribeSessions = res
    "DescribeSessionsResponse"
    "fixture/DescribeSessionsResponse.proto"
    appStream
    (Proxy :: Proxy DescribeSessions)

responseDescribeStacks :: DescribeStacksResponse -> TestTree
responseDescribeStacks = res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    appStream
    (Proxy :: Proxy DescribeStacks)

responseDescribeFleets :: DescribeFleetsResponse -> TestTree
responseDescribeFleets = res
    "DescribeFleetsResponse"
    "fixture/DescribeFleetsResponse.proto"
    appStream
    (Proxy :: Proxy DescribeFleets)

responseStopFleet :: StopFleetResponse -> TestTree
responseStopFleet = res
    "StopFleetResponse"
    "fixture/StopFleetResponse.proto"
    appStream
    (Proxy :: Proxy StopFleet)

responseDeleteDirectoryConfig :: DeleteDirectoryConfigResponse -> TestTree
responseDeleteDirectoryConfig = res
    "DeleteDirectoryConfigResponse"
    "fixture/DeleteDirectoryConfigResponse.proto"
    appStream
    (Proxy :: Proxy DeleteDirectoryConfig)

responseUpdateDirectoryConfig :: UpdateDirectoryConfigResponse -> TestTree
responseUpdateDirectoryConfig = res
    "UpdateDirectoryConfigResponse"
    "fixture/UpdateDirectoryConfigResponse.proto"
    appStream
    (Proxy :: Proxy UpdateDirectoryConfig)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet = res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    appStream
    (Proxy :: Proxy CreateFleet)

responseCreateStack :: CreateStackResponse -> TestTree
responseCreateStack = res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    appStream
    (Proxy :: Proxy CreateStack)

responseExpireSession :: ExpireSessionResponse -> TestTree
responseExpireSession = res
    "ExpireSessionResponse"
    "fixture/ExpireSessionResponse.proto"
    appStream
    (Proxy :: Proxy ExpireSession)

responseCreateStreamingURL :: CreateStreamingURLResponse -> TestTree
responseCreateStreamingURL = res
    "CreateStreamingURLResponse"
    "fixture/CreateStreamingURLResponse.proto"
    appStream
    (Proxy :: Proxy CreateStreamingURL)

responseStartFleet :: StartFleetResponse -> TestTree
responseStartFleet = res
    "StartFleetResponse"
    "fixture/StartFleetResponse.proto"
    appStream
    (Proxy :: Proxy StartFleet)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages = res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    appStream
    (Proxy :: Proxy DescribeImages)
