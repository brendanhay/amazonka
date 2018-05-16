{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-orphans        #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AppStream
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Test.AWS.Gen.AppStream where

import Data.Proxy
import Network.AWS.AppStream
import Test.AWS.AppStream.Internal
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
--         , requestDeleteImageBuilder $
--             deleteImageBuilder
--
--         , requestAssociateFleet $
--             associateFleet
--
--         , requestCreateImageBuilder $
--             createImageBuilder
--
--         , requestListTagsForResource $
--             listTagsForResource
--
--         , requestDescribeDirectoryConfigs $
--             describeDirectoryConfigs
--
--         , requestCreateImageBuilderStreamingURL $
--             createImageBuilderStreamingURL
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
--         , requestDescribeImageBuilders $
--             describeImageBuilders
--
--         , requestStopFleet $
--             stopFleet
--
--         , requestStartImageBuilder $
--             startImageBuilder
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
--         , requestCopyImage $
--             copyImage
--
--         , requestExpireSession $
--             expireSession
--
--         , requestTagResource $
--             tagResource
--
--         , requestCreateStreamingURL $
--             createStreamingURL
--
--         , requestUntagResource $
--             untagResource
--
--         , requestStartFleet $
--             startFleet
--
--         , requestStopImageBuilder $
--             stopImageBuilder
--
--         , requestDeleteImage $
--             deleteImage
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
--         , responseDeleteImageBuilder $
--             deleteImageBuilderResponse
--
--         , responseAssociateFleet $
--             associateFleetResponse
--
--         , responseCreateImageBuilder $
--             createImageBuilderResponse
--
--         , responseListTagsForResource $
--             listTagsForResourceResponse
--
--         , responseDescribeDirectoryConfigs $
--             describeDirectoryConfigsResponse
--
--         , responseCreateImageBuilderStreamingURL $
--             createImageBuilderStreamingURLResponse
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
--         , responseDescribeImageBuilders $
--             describeImageBuildersResponse
--
--         , responseStopFleet $
--             stopFleetResponse
--
--         , responseStartImageBuilder $
--             startImageBuilderResponse
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
--         , responseCopyImage $
--             copyImageResponse
--
--         , responseExpireSession $
--             expireSessionResponse
--
--         , responseTagResource $
--             tagResourceResponse
--
--         , responseCreateStreamingURL $
--             createStreamingURLResponse
--
--         , responseUntagResource $
--             untagResourceResponse
--
--         , responseStartFleet $
--             startFleetResponse
--
--         , responseStopImageBuilder $
--             stopImageBuilderResponse
--
--         , responseDeleteImage $
--             deleteImageResponse
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

requestDeleteImageBuilder :: DeleteImageBuilder -> TestTree
requestDeleteImageBuilder = req
    "DeleteImageBuilder"
    "fixture/DeleteImageBuilder.yaml"

requestAssociateFleet :: AssociateFleet -> TestTree
requestAssociateFleet = req
    "AssociateFleet"
    "fixture/AssociateFleet.yaml"

requestCreateImageBuilder :: CreateImageBuilder -> TestTree
requestCreateImageBuilder = req
    "CreateImageBuilder"
    "fixture/CreateImageBuilder.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource = req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeDirectoryConfigs :: DescribeDirectoryConfigs -> TestTree
requestDescribeDirectoryConfigs = req
    "DescribeDirectoryConfigs"
    "fixture/DescribeDirectoryConfigs.yaml"

requestCreateImageBuilderStreamingURL :: CreateImageBuilderStreamingURL -> TestTree
requestCreateImageBuilderStreamingURL = req
    "CreateImageBuilderStreamingURL"
    "fixture/CreateImageBuilderStreamingURL.yaml"

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

requestDescribeImageBuilders :: DescribeImageBuilders -> TestTree
requestDescribeImageBuilders = req
    "DescribeImageBuilders"
    "fixture/DescribeImageBuilders.yaml"

requestStopFleet :: StopFleet -> TestTree
requestStopFleet = req
    "StopFleet"
    "fixture/StopFleet.yaml"

requestStartImageBuilder :: StartImageBuilder -> TestTree
requestStartImageBuilder = req
    "StartImageBuilder"
    "fixture/StartImageBuilder.yaml"

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

requestCopyImage :: CopyImage -> TestTree
requestCopyImage = req
    "CopyImage"
    "fixture/CopyImage.yaml"

requestExpireSession :: ExpireSession -> TestTree
requestExpireSession = req
    "ExpireSession"
    "fixture/ExpireSession.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource = req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateStreamingURL :: CreateStreamingURL -> TestTree
requestCreateStreamingURL = req
    "CreateStreamingURL"
    "fixture/CreateStreamingURL.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource = req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestStartFleet :: StartFleet -> TestTree
requestStartFleet = req
    "StartFleet"
    "fixture/StartFleet.yaml"

requestStopImageBuilder :: StopImageBuilder -> TestTree
requestStopImageBuilder = req
    "StopImageBuilder"
    "fixture/StopImageBuilder.yaml"

requestDeleteImage :: DeleteImage -> TestTree
requestDeleteImage = req
    "DeleteImage"
    "fixture/DeleteImage.yaml"

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

responseDeleteImageBuilder :: DeleteImageBuilderResponse -> TestTree
responseDeleteImageBuilder = res
    "DeleteImageBuilderResponse"
    "fixture/DeleteImageBuilderResponse.proto"
    appStream
    (Proxy :: Proxy DeleteImageBuilder)

responseAssociateFleet :: AssociateFleetResponse -> TestTree
responseAssociateFleet = res
    "AssociateFleetResponse"
    "fixture/AssociateFleetResponse.proto"
    appStream
    (Proxy :: Proxy AssociateFleet)

responseCreateImageBuilder :: CreateImageBuilderResponse -> TestTree
responseCreateImageBuilder = res
    "CreateImageBuilderResponse"
    "fixture/CreateImageBuilderResponse.proto"
    appStream
    (Proxy :: Proxy CreateImageBuilder)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource = res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    appStream
    (Proxy :: Proxy ListTagsForResource)

responseDescribeDirectoryConfigs :: DescribeDirectoryConfigsResponse -> TestTree
responseDescribeDirectoryConfigs = res
    "DescribeDirectoryConfigsResponse"
    "fixture/DescribeDirectoryConfigsResponse.proto"
    appStream
    (Proxy :: Proxy DescribeDirectoryConfigs)

responseCreateImageBuilderStreamingURL :: CreateImageBuilderStreamingURLResponse -> TestTree
responseCreateImageBuilderStreamingURL = res
    "CreateImageBuilderStreamingURLResponse"
    "fixture/CreateImageBuilderStreamingURLResponse.proto"
    appStream
    (Proxy :: Proxy CreateImageBuilderStreamingURL)

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

responseDescribeImageBuilders :: DescribeImageBuildersResponse -> TestTree
responseDescribeImageBuilders = res
    "DescribeImageBuildersResponse"
    "fixture/DescribeImageBuildersResponse.proto"
    appStream
    (Proxy :: Proxy DescribeImageBuilders)

responseStopFleet :: StopFleetResponse -> TestTree
responseStopFleet = res
    "StopFleetResponse"
    "fixture/StopFleetResponse.proto"
    appStream
    (Proxy :: Proxy StopFleet)

responseStartImageBuilder :: StartImageBuilderResponse -> TestTree
responseStartImageBuilder = res
    "StartImageBuilderResponse"
    "fixture/StartImageBuilderResponse.proto"
    appStream
    (Proxy :: Proxy StartImageBuilder)

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

responseCopyImage :: CopyImageResponse -> TestTree
responseCopyImage = res
    "CopyImageResponse"
    "fixture/CopyImageResponse.proto"
    appStream
    (Proxy :: Proxy CopyImage)

responseExpireSession :: ExpireSessionResponse -> TestTree
responseExpireSession = res
    "ExpireSessionResponse"
    "fixture/ExpireSessionResponse.proto"
    appStream
    (Proxy :: Proxy ExpireSession)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource = res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    appStream
    (Proxy :: Proxy TagResource)

responseCreateStreamingURL :: CreateStreamingURLResponse -> TestTree
responseCreateStreamingURL = res
    "CreateStreamingURLResponse"
    "fixture/CreateStreamingURLResponse.proto"
    appStream
    (Proxy :: Proxy CreateStreamingURL)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource = res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    appStream
    (Proxy :: Proxy UntagResource)

responseStartFleet :: StartFleetResponse -> TestTree
responseStartFleet = res
    "StartFleetResponse"
    "fixture/StartFleetResponse.proto"
    appStream
    (Proxy :: Proxy StartFleet)

responseStopImageBuilder :: StopImageBuilderResponse -> TestTree
responseStopImageBuilder = res
    "StopImageBuilderResponse"
    "fixture/StopImageBuilderResponse.proto"
    appStream
    (Proxy :: Proxy StopImageBuilder)

responseDeleteImage :: DeleteImageResponse -> TestTree
responseDeleteImage = res
    "DeleteImageResponse"
    "fixture/DeleteImageResponse.proto"
    appStream
    (Proxy :: Proxy DeleteImage)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages = res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    appStream
    (Proxy :: Proxy DescribeImages)
