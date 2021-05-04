{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.AppStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
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
--         [ requestDeleteImageBuilder $
--             newDeleteImageBuilder
--
--         , requestListAssociatedFleets $
--             newListAssociatedFleets
--
--         , requestBatchAssociateUserStack $
--             newBatchAssociateUserStack
--
--         , requestListAssociatedStacks $
--             newListAssociatedStacks
--
--         , requestDeleteUsageReportSubscription $
--             newDeleteUsageReportSubscription
--
--         , requestStopImageBuilder $
--             newStopImageBuilder
--
--         , requestStartFleet $
--             newStartFleet
--
--         , requestStartImageBuilder $
--             newStartImageBuilder
--
--         , requestStopFleet $
--             newStopFleet
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestEnableUser $
--             newEnableUser
--
--         , requestDescribeSessions $
--             newDescribeSessions
--
--         , requestDescribeFleets $
--             newDescribeFleets
--
--         , requestDescribeStacks $
--             newDescribeStacks
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestUpdateDirectoryConfig $
--             newUpdateDirectoryConfig
--
--         , requestCreateStack $
--             newCreateStack
--
--         , requestDeleteDirectoryConfig $
--             newDeleteDirectoryConfig
--
--         , requestCopyImage $
--             newCopyImage
--
--         , requestCreateFleet $
--             newCreateFleet
--
--         , requestCreateImageBuilder $
--             newCreateImageBuilder
--
--         , requestAssociateFleet $
--             newAssociateFleet
--
--         , requestCreateDirectoryConfig $
--             newCreateDirectoryConfig
--
--         , requestUpdateFleet $
--             newUpdateFleet
--
--         , requestDeleteStack $
--             newDeleteStack
--
--         , requestDeleteFleet $
--             newDeleteFleet
--
--         , requestDescribeUsers $
--             newDescribeUsers
--
--         , requestUpdateStack $
--             newUpdateStack
--
--         , requestCreateUsageReportSubscription $
--             newCreateUsageReportSubscription
--
--         , requestDisassociateFleet $
--             newDisassociateFleet
--
--         , requestDescribeImages $
--             newDescribeImages
--
--         , requestBatchDisassociateUserStack $
--             newBatchDisassociateUserStack
--
--         , requestDescribeUsageReportSubscriptions $
--             newDescribeUsageReportSubscriptions
--
--         , requestDeleteImage $
--             newDeleteImage
--
--         , requestDeleteImagePermissions $
--             newDeleteImagePermissions
--
--         , requestUpdateImagePermissions $
--             newUpdateImagePermissions
--
--         , requestCreateStreamingURL $
--             newCreateStreamingURL
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDescribeUserStackAssociations $
--             newDescribeUserStackAssociations
--
--         , requestDescribeImageBuilders $
--             newDescribeImageBuilders
--
--         , requestDescribeDirectoryConfigs $
--             newDescribeDirectoryConfigs
--
--         , requestDisableUser $
--             newDisableUser
--
--         , requestExpireSession $
--             newExpireSession
--
--         , requestCreateImageBuilderStreamingURL $
--             newCreateImageBuilderStreamingURL
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeImagePermissions $
--             newDescribeImagePermissions
--
--           ]

--     , testGroup "response"
--         [ responseDeleteImageBuilder $
--             newDeleteImageBuilderResponse
--
--         , responseListAssociatedFleets $
--             newListAssociatedFleetsResponse
--
--         , responseBatchAssociateUserStack $
--             newBatchAssociateUserStackResponse
--
--         , responseListAssociatedStacks $
--             newListAssociatedStacksResponse
--
--         , responseDeleteUsageReportSubscription $
--             newDeleteUsageReportSubscriptionResponse
--
--         , responseStopImageBuilder $
--             newStopImageBuilderResponse
--
--         , responseStartFleet $
--             newStartFleetResponse
--
--         , responseStartImageBuilder $
--             newStartImageBuilderResponse
--
--         , responseStopFleet $
--             newStopFleetResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseEnableUser $
--             newEnableUserResponse
--
--         , responseDescribeSessions $
--             newDescribeSessionsResponse
--
--         , responseDescribeFleets $
--             newDescribeFleetsResponse
--
--         , responseDescribeStacks $
--             newDescribeStacksResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseUpdateDirectoryConfig $
--             newUpdateDirectoryConfigResponse
--
--         , responseCreateStack $
--             newCreateStackResponse
--
--         , responseDeleteDirectoryConfig $
--             newDeleteDirectoryConfigResponse
--
--         , responseCopyImage $
--             newCopyImageResponse
--
--         , responseCreateFleet $
--             newCreateFleetResponse
--
--         , responseCreateImageBuilder $
--             newCreateImageBuilderResponse
--
--         , responseAssociateFleet $
--             newAssociateFleetResponse
--
--         , responseCreateDirectoryConfig $
--             newCreateDirectoryConfigResponse
--
--         , responseUpdateFleet $
--             newUpdateFleetResponse
--
--         , responseDeleteStack $
--             newDeleteStackResponse
--
--         , responseDeleteFleet $
--             newDeleteFleetResponse
--
--         , responseDescribeUsers $
--             newDescribeUsersResponse
--
--         , responseUpdateStack $
--             newUpdateStackResponse
--
--         , responseCreateUsageReportSubscription $
--             newCreateUsageReportSubscriptionResponse
--
--         , responseDisassociateFleet $
--             newDisassociateFleetResponse
--
--         , responseDescribeImages $
--             newDescribeImagesResponse
--
--         , responseBatchDisassociateUserStack $
--             newBatchDisassociateUserStackResponse
--
--         , responseDescribeUsageReportSubscriptions $
--             newDescribeUsageReportSubscriptionsResponse
--
--         , responseDeleteImage $
--             newDeleteImageResponse
--
--         , responseDeleteImagePermissions $
--             newDeleteImagePermissionsResponse
--
--         , responseUpdateImagePermissions $
--             newUpdateImagePermissionsResponse
--
--         , responseCreateStreamingURL $
--             newCreateStreamingURLResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDescribeUserStackAssociations $
--             newDescribeUserStackAssociationsResponse
--
--         , responseDescribeImageBuilders $
--             newDescribeImageBuildersResponse
--
--         , responseDescribeDirectoryConfigs $
--             newDescribeDirectoryConfigsResponse
--
--         , responseDisableUser $
--             newDisableUserResponse
--
--         , responseExpireSession $
--             newExpireSessionResponse
--
--         , responseCreateImageBuilderStreamingURL $
--             newCreateImageBuilderStreamingURLResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeImagePermissions $
--             newDescribeImagePermissionsResponse
--
--           ]
--     ]

-- Requests

requestDeleteImageBuilder :: DeleteImageBuilder -> TestTree
requestDeleteImageBuilder =
  req
    "DeleteImageBuilder"
    "fixture/DeleteImageBuilder.yaml"

requestListAssociatedFleets :: ListAssociatedFleets -> TestTree
requestListAssociatedFleets =
  req
    "ListAssociatedFleets"
    "fixture/ListAssociatedFleets.yaml"

requestBatchAssociateUserStack :: BatchAssociateUserStack -> TestTree
requestBatchAssociateUserStack =
  req
    "BatchAssociateUserStack"
    "fixture/BatchAssociateUserStack.yaml"

requestListAssociatedStacks :: ListAssociatedStacks -> TestTree
requestListAssociatedStacks =
  req
    "ListAssociatedStacks"
    "fixture/ListAssociatedStacks.yaml"

requestDeleteUsageReportSubscription :: DeleteUsageReportSubscription -> TestTree
requestDeleteUsageReportSubscription =
  req
    "DeleteUsageReportSubscription"
    "fixture/DeleteUsageReportSubscription.yaml"

requestStopImageBuilder :: StopImageBuilder -> TestTree
requestStopImageBuilder =
  req
    "StopImageBuilder"
    "fixture/StopImageBuilder.yaml"

requestStartFleet :: StartFleet -> TestTree
requestStartFleet =
  req
    "StartFleet"
    "fixture/StartFleet.yaml"

requestStartImageBuilder :: StartImageBuilder -> TestTree
requestStartImageBuilder =
  req
    "StartImageBuilder"
    "fixture/StartImageBuilder.yaml"

requestStopFleet :: StopFleet -> TestTree
requestStopFleet =
  req
    "StopFleet"
    "fixture/StopFleet.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestEnableUser :: EnableUser -> TestTree
requestEnableUser =
  req
    "EnableUser"
    "fixture/EnableUser.yaml"

requestDescribeSessions :: DescribeSessions -> TestTree
requestDescribeSessions =
  req
    "DescribeSessions"
    "fixture/DescribeSessions.yaml"

requestDescribeFleets :: DescribeFleets -> TestTree
requestDescribeFleets =
  req
    "DescribeFleets"
    "fixture/DescribeFleets.yaml"

requestDescribeStacks :: DescribeStacks -> TestTree
requestDescribeStacks =
  req
    "DescribeStacks"
    "fixture/DescribeStacks.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestUpdateDirectoryConfig :: UpdateDirectoryConfig -> TestTree
requestUpdateDirectoryConfig =
  req
    "UpdateDirectoryConfig"
    "fixture/UpdateDirectoryConfig.yaml"

requestCreateStack :: CreateStack -> TestTree
requestCreateStack =
  req
    "CreateStack"
    "fixture/CreateStack.yaml"

requestDeleteDirectoryConfig :: DeleteDirectoryConfig -> TestTree
requestDeleteDirectoryConfig =
  req
    "DeleteDirectoryConfig"
    "fixture/DeleteDirectoryConfig.yaml"

requestCopyImage :: CopyImage -> TestTree
requestCopyImage =
  req
    "CopyImage"
    "fixture/CopyImage.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet =
  req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

requestCreateImageBuilder :: CreateImageBuilder -> TestTree
requestCreateImageBuilder =
  req
    "CreateImageBuilder"
    "fixture/CreateImageBuilder.yaml"

requestAssociateFleet :: AssociateFleet -> TestTree
requestAssociateFleet =
  req
    "AssociateFleet"
    "fixture/AssociateFleet.yaml"

requestCreateDirectoryConfig :: CreateDirectoryConfig -> TestTree
requestCreateDirectoryConfig =
  req
    "CreateDirectoryConfig"
    "fixture/CreateDirectoryConfig.yaml"

requestUpdateFleet :: UpdateFleet -> TestTree
requestUpdateFleet =
  req
    "UpdateFleet"
    "fixture/UpdateFleet.yaml"

requestDeleteStack :: DeleteStack -> TestTree
requestDeleteStack =
  req
    "DeleteStack"
    "fixture/DeleteStack.yaml"

requestDeleteFleet :: DeleteFleet -> TestTree
requestDeleteFleet =
  req
    "DeleteFleet"
    "fixture/DeleteFleet.yaml"

requestDescribeUsers :: DescribeUsers -> TestTree
requestDescribeUsers =
  req
    "DescribeUsers"
    "fixture/DescribeUsers.yaml"

requestUpdateStack :: UpdateStack -> TestTree
requestUpdateStack =
  req
    "UpdateStack"
    "fixture/UpdateStack.yaml"

requestCreateUsageReportSubscription :: CreateUsageReportSubscription -> TestTree
requestCreateUsageReportSubscription =
  req
    "CreateUsageReportSubscription"
    "fixture/CreateUsageReportSubscription.yaml"

requestDisassociateFleet :: DisassociateFleet -> TestTree
requestDisassociateFleet =
  req
    "DisassociateFleet"
    "fixture/DisassociateFleet.yaml"

requestDescribeImages :: DescribeImages -> TestTree
requestDescribeImages =
  req
    "DescribeImages"
    "fixture/DescribeImages.yaml"

requestBatchDisassociateUserStack :: BatchDisassociateUserStack -> TestTree
requestBatchDisassociateUserStack =
  req
    "BatchDisassociateUserStack"
    "fixture/BatchDisassociateUserStack.yaml"

requestDescribeUsageReportSubscriptions :: DescribeUsageReportSubscriptions -> TestTree
requestDescribeUsageReportSubscriptions =
  req
    "DescribeUsageReportSubscriptions"
    "fixture/DescribeUsageReportSubscriptions.yaml"

requestDeleteImage :: DeleteImage -> TestTree
requestDeleteImage =
  req
    "DeleteImage"
    "fixture/DeleteImage.yaml"

requestDeleteImagePermissions :: DeleteImagePermissions -> TestTree
requestDeleteImagePermissions =
  req
    "DeleteImagePermissions"
    "fixture/DeleteImagePermissions.yaml"

requestUpdateImagePermissions :: UpdateImagePermissions -> TestTree
requestUpdateImagePermissions =
  req
    "UpdateImagePermissions"
    "fixture/UpdateImagePermissions.yaml"

requestCreateStreamingURL :: CreateStreamingURL -> TestTree
requestCreateStreamingURL =
  req
    "CreateStreamingURL"
    "fixture/CreateStreamingURL.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestDescribeUserStackAssociations :: DescribeUserStackAssociations -> TestTree
requestDescribeUserStackAssociations =
  req
    "DescribeUserStackAssociations"
    "fixture/DescribeUserStackAssociations.yaml"

requestDescribeImageBuilders :: DescribeImageBuilders -> TestTree
requestDescribeImageBuilders =
  req
    "DescribeImageBuilders"
    "fixture/DescribeImageBuilders.yaml"

requestDescribeDirectoryConfigs :: DescribeDirectoryConfigs -> TestTree
requestDescribeDirectoryConfigs =
  req
    "DescribeDirectoryConfigs"
    "fixture/DescribeDirectoryConfigs.yaml"

requestDisableUser :: DisableUser -> TestTree
requestDisableUser =
  req
    "DisableUser"
    "fixture/DisableUser.yaml"

requestExpireSession :: ExpireSession -> TestTree
requestExpireSession =
  req
    "ExpireSession"
    "fixture/ExpireSession.yaml"

requestCreateImageBuilderStreamingURL :: CreateImageBuilderStreamingURL -> TestTree
requestCreateImageBuilderStreamingURL =
  req
    "CreateImageBuilderStreamingURL"
    "fixture/CreateImageBuilderStreamingURL.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeImagePermissions :: DescribeImagePermissions -> TestTree
requestDescribeImagePermissions =
  req
    "DescribeImagePermissions"
    "fixture/DescribeImagePermissions.yaml"

-- Responses

responseDeleteImageBuilder :: DeleteImageBuilderResponse -> TestTree
responseDeleteImageBuilder =
  res
    "DeleteImageBuilderResponse"
    "fixture/DeleteImageBuilderResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteImageBuilder)

responseListAssociatedFleets :: ListAssociatedFleetsResponse -> TestTree
responseListAssociatedFleets =
  res
    "ListAssociatedFleetsResponse"
    "fixture/ListAssociatedFleetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssociatedFleets)

responseBatchAssociateUserStack :: BatchAssociateUserStackResponse -> TestTree
responseBatchAssociateUserStack =
  res
    "BatchAssociateUserStackResponse"
    "fixture/BatchAssociateUserStackResponse.proto"
    defaultService
    (Proxy :: Proxy BatchAssociateUserStack)

responseListAssociatedStacks :: ListAssociatedStacksResponse -> TestTree
responseListAssociatedStacks =
  res
    "ListAssociatedStacksResponse"
    "fixture/ListAssociatedStacksResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssociatedStacks)

responseDeleteUsageReportSubscription :: DeleteUsageReportSubscriptionResponse -> TestTree
responseDeleteUsageReportSubscription =
  res
    "DeleteUsageReportSubscriptionResponse"
    "fixture/DeleteUsageReportSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUsageReportSubscription)

responseStopImageBuilder :: StopImageBuilderResponse -> TestTree
responseStopImageBuilder =
  res
    "StopImageBuilderResponse"
    "fixture/StopImageBuilderResponse.proto"
    defaultService
    (Proxy :: Proxy StopImageBuilder)

responseStartFleet :: StartFleetResponse -> TestTree
responseStartFleet =
  res
    "StartFleetResponse"
    "fixture/StartFleetResponse.proto"
    defaultService
    (Proxy :: Proxy StartFleet)

responseStartImageBuilder :: StartImageBuilderResponse -> TestTree
responseStartImageBuilder =
  res
    "StartImageBuilderResponse"
    "fixture/StartImageBuilderResponse.proto"
    defaultService
    (Proxy :: Proxy StartImageBuilder)

responseStopFleet :: StopFleetResponse -> TestTree
responseStopFleet =
  res
    "StopFleetResponse"
    "fixture/StopFleetResponse.proto"
    defaultService
    (Proxy :: Proxy StopFleet)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseEnableUser :: EnableUserResponse -> TestTree
responseEnableUser =
  res
    "EnableUserResponse"
    "fixture/EnableUserResponse.proto"
    defaultService
    (Proxy :: Proxy EnableUser)

responseDescribeSessions :: DescribeSessionsResponse -> TestTree
responseDescribeSessions =
  res
    "DescribeSessionsResponse"
    "fixture/DescribeSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSessions)

responseDescribeFleets :: DescribeFleetsResponse -> TestTree
responseDescribeFleets =
  res
    "DescribeFleetsResponse"
    "fixture/DescribeFleetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleets)

responseDescribeStacks :: DescribeStacksResponse -> TestTree
responseDescribeStacks =
  res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStacks)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUser)

responseUpdateDirectoryConfig :: UpdateDirectoryConfigResponse -> TestTree
responseUpdateDirectoryConfig =
  res
    "UpdateDirectoryConfigResponse"
    "fixture/UpdateDirectoryConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDirectoryConfig)

responseCreateStack :: CreateStackResponse -> TestTree
responseCreateStack =
  res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStack)

responseDeleteDirectoryConfig :: DeleteDirectoryConfigResponse -> TestTree
responseDeleteDirectoryConfig =
  res
    "DeleteDirectoryConfigResponse"
    "fixture/DeleteDirectoryConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDirectoryConfig)

responseCopyImage :: CopyImageResponse -> TestTree
responseCopyImage =
  res
    "CopyImageResponse"
    "fixture/CopyImageResponse.proto"
    defaultService
    (Proxy :: Proxy CopyImage)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFleet)

responseCreateImageBuilder :: CreateImageBuilderResponse -> TestTree
responseCreateImageBuilder =
  res
    "CreateImageBuilderResponse"
    "fixture/CreateImageBuilderResponse.proto"
    defaultService
    (Proxy :: Proxy CreateImageBuilder)

responseAssociateFleet :: AssociateFleetResponse -> TestTree
responseAssociateFleet =
  res
    "AssociateFleetResponse"
    "fixture/AssociateFleetResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateFleet)

responseCreateDirectoryConfig :: CreateDirectoryConfigResponse -> TestTree
responseCreateDirectoryConfig =
  res
    "CreateDirectoryConfigResponse"
    "fixture/CreateDirectoryConfigResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDirectoryConfig)

responseUpdateFleet :: UpdateFleetResponse -> TestTree
responseUpdateFleet =
  res
    "UpdateFleetResponse"
    "fixture/UpdateFleetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFleet)

responseDeleteStack :: DeleteStackResponse -> TestTree
responseDeleteStack =
  res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStack)

responseDeleteFleet :: DeleteFleetResponse -> TestTree
responseDeleteFleet =
  res
    "DeleteFleetResponse"
    "fixture/DeleteFleetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFleet)

responseDescribeUsers :: DescribeUsersResponse -> TestTree
responseDescribeUsers =
  res
    "DescribeUsersResponse"
    "fixture/DescribeUsersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUsers)

responseUpdateStack :: UpdateStackResponse -> TestTree
responseUpdateStack =
  res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateStack)

responseCreateUsageReportSubscription :: CreateUsageReportSubscriptionResponse -> TestTree
responseCreateUsageReportSubscription =
  res
    "CreateUsageReportSubscriptionResponse"
    "fixture/CreateUsageReportSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUsageReportSubscription)

responseDisassociateFleet :: DisassociateFleetResponse -> TestTree
responseDisassociateFleet =
  res
    "DisassociateFleetResponse"
    "fixture/DisassociateFleetResponse.proto"
    defaultService
    (Proxy :: Proxy DisassociateFleet)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages =
  res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImages)

responseBatchDisassociateUserStack :: BatchDisassociateUserStackResponse -> TestTree
responseBatchDisassociateUserStack =
  res
    "BatchDisassociateUserStackResponse"
    "fixture/BatchDisassociateUserStackResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDisassociateUserStack)

responseDescribeUsageReportSubscriptions :: DescribeUsageReportSubscriptionsResponse -> TestTree
responseDescribeUsageReportSubscriptions =
  res
    "DescribeUsageReportSubscriptionsResponse"
    "fixture/DescribeUsageReportSubscriptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUsageReportSubscriptions)

responseDeleteImage :: DeleteImageResponse -> TestTree
responseDeleteImage =
  res
    "DeleteImageResponse"
    "fixture/DeleteImageResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteImage)

responseDeleteImagePermissions :: DeleteImagePermissionsResponse -> TestTree
responseDeleteImagePermissions =
  res
    "DeleteImagePermissionsResponse"
    "fixture/DeleteImagePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteImagePermissions)

responseUpdateImagePermissions :: UpdateImagePermissionsResponse -> TestTree
responseUpdateImagePermissions =
  res
    "UpdateImagePermissionsResponse"
    "fixture/UpdateImagePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateImagePermissions)

responseCreateStreamingURL :: CreateStreamingURLResponse -> TestTree
responseCreateStreamingURL =
  res
    "CreateStreamingURLResponse"
    "fixture/CreateStreamingURLResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStreamingURL)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUser)

responseDescribeUserStackAssociations :: DescribeUserStackAssociationsResponse -> TestTree
responseDescribeUserStackAssociations =
  res
    "DescribeUserStackAssociationsResponse"
    "fixture/DescribeUserStackAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserStackAssociations)

responseDescribeImageBuilders :: DescribeImageBuildersResponse -> TestTree
responseDescribeImageBuilders =
  res
    "DescribeImageBuildersResponse"
    "fixture/DescribeImageBuildersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImageBuilders)

responseDescribeDirectoryConfigs :: DescribeDirectoryConfigsResponse -> TestTree
responseDescribeDirectoryConfigs =
  res
    "DescribeDirectoryConfigsResponse"
    "fixture/DescribeDirectoryConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDirectoryConfigs)

responseDisableUser :: DisableUserResponse -> TestTree
responseDisableUser =
  res
    "DisableUserResponse"
    "fixture/DisableUserResponse.proto"
    defaultService
    (Proxy :: Proxy DisableUser)

responseExpireSession :: ExpireSessionResponse -> TestTree
responseExpireSession =
  res
    "ExpireSessionResponse"
    "fixture/ExpireSessionResponse.proto"
    defaultService
    (Proxy :: Proxy ExpireSession)

responseCreateImageBuilderStreamingURL :: CreateImageBuilderStreamingURLResponse -> TestTree
responseCreateImageBuilderStreamingURL =
  res
    "CreateImageBuilderStreamingURLResponse"
    "fixture/CreateImageBuilderStreamingURLResponse.proto"
    defaultService
    (Proxy :: Proxy CreateImageBuilderStreamingURL)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDescribeImagePermissions :: DescribeImagePermissionsResponse -> TestTree
responseDescribeImagePermissions =
  res
    "DescribeImagePermissionsResponse"
    "fixture/DescribeImagePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImagePermissions)
