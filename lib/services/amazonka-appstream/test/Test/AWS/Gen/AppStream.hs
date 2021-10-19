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
--         [ requestCreateUsageReportSubscription $
--             newCreateUsageReportSubscription
--
--         , requestDisassociateFleet $
--             newDisassociateFleet
--
--         , requestListAssociatedFleets $
--             newListAssociatedFleets
--
--         , requestDeleteStack $
--             newDeleteStack
--
--         , requestUpdateStack $
--             newUpdateStack
--
--         , requestCreateDirectoryConfig $
--             newCreateDirectoryConfig
--
--         , requestDescribeUsers $
--             newDescribeUsers
--
--         , requestListAssociatedStacks $
--             newListAssociatedStacks
--
--         , requestDeleteFleet $
--             newDeleteFleet
--
--         , requestUpdateFleet $
--             newUpdateFleet
--
--         , requestDeleteImageBuilder $
--             newDeleteImageBuilder
--
--         , requestAssociateFleet $
--             newAssociateFleet
--
--         , requestCreateImageBuilder $
--             newCreateImageBuilder
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeDirectoryConfigs $
--             newDescribeDirectoryConfigs
--
--         , requestCreateImageBuilderStreamingURL $
--             newCreateImageBuilderStreamingURL
--
--         , requestDescribeSessions $
--             newDescribeSessions
--
--         , requestDescribeStacks $
--             newDescribeStacks
--
--         , requestDescribeFleets $
--             newDescribeFleets
--
--         , requestDescribeImageBuilders $
--             newDescribeImageBuilders
--
--         , requestEnableUser $
--             newEnableUser
--
--         , requestDescribeUserStackAssociations $
--             newDescribeUserStackAssociations
--
--         , requestCreateUpdatedImage $
--             newCreateUpdatedImage
--
--         , requestDescribeUsageReportSubscriptions $
--             newDescribeUsageReportSubscriptions
--
--         , requestUpdateImagePermissions $
--             newUpdateImagePermissions
--
--         , requestDeleteImagePermissions $
--             newDeleteImagePermissions
--
--         , requestStopFleet $
--             newStopFleet
--
--         , requestStartImageBuilder $
--             newStartImageBuilder
--
--         , requestBatchAssociateUserStack $
--             newBatchAssociateUserStack
--
--         , requestDescribeImagePermissions $
--             newDescribeImagePermissions
--
--         , requestDeleteDirectoryConfig $
--             newDeleteDirectoryConfig
--
--         , requestUpdateDirectoryConfig $
--             newUpdateDirectoryConfig
--
--         , requestCreateFleet $
--             newCreateFleet
--
--         , requestCreateStack $
--             newCreateStack
--
--         , requestCopyImage $
--             newCopyImage
--
--         , requestExpireSession $
--             newExpireSession
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestDisableUser $
--             newDisableUser
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestTagResource $
--             newTagResource
--
--         , requestCreateStreamingURL $
--             newCreateStreamingURL
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestStartFleet $
--             newStartFleet
--
--         , requestStopImageBuilder $
--             newStopImageBuilder
--
--         , requestDeleteImage $
--             newDeleteImage
--
--         , requestDeleteUsageReportSubscription $
--             newDeleteUsageReportSubscription
--
--         , requestBatchDisassociateUserStack $
--             newBatchDisassociateUserStack
--
--         , requestDescribeImages $
--             newDescribeImages
--
--           ]

--     , testGroup "response"
--         [ responseCreateUsageReportSubscription $
--             newCreateUsageReportSubscriptionResponse
--
--         , responseDisassociateFleet $
--             newDisassociateFleetResponse
--
--         , responseListAssociatedFleets $
--             newListAssociatedFleetsResponse
--
--         , responseDeleteStack $
--             newDeleteStackResponse
--
--         , responseUpdateStack $
--             newUpdateStackResponse
--
--         , responseCreateDirectoryConfig $
--             newCreateDirectoryConfigResponse
--
--         , responseDescribeUsers $
--             newDescribeUsersResponse
--
--         , responseListAssociatedStacks $
--             newListAssociatedStacksResponse
--
--         , responseDeleteFleet $
--             newDeleteFleetResponse
--
--         , responseUpdateFleet $
--             newUpdateFleetResponse
--
--         , responseDeleteImageBuilder $
--             newDeleteImageBuilderResponse
--
--         , responseAssociateFleet $
--             newAssociateFleetResponse
--
--         , responseCreateImageBuilder $
--             newCreateImageBuilderResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseDescribeDirectoryConfigs $
--             newDescribeDirectoryConfigsResponse
--
--         , responseCreateImageBuilderStreamingURL $
--             newCreateImageBuilderStreamingURLResponse
--
--         , responseDescribeSessions $
--             newDescribeSessionsResponse
--
--         , responseDescribeStacks $
--             newDescribeStacksResponse
--
--         , responseDescribeFleets $
--             newDescribeFleetsResponse
--
--         , responseDescribeImageBuilders $
--             newDescribeImageBuildersResponse
--
--         , responseEnableUser $
--             newEnableUserResponse
--
--         , responseDescribeUserStackAssociations $
--             newDescribeUserStackAssociationsResponse
--
--         , responseCreateUpdatedImage $
--             newCreateUpdatedImageResponse
--
--         , responseDescribeUsageReportSubscriptions $
--             newDescribeUsageReportSubscriptionsResponse
--
--         , responseUpdateImagePermissions $
--             newUpdateImagePermissionsResponse
--
--         , responseDeleteImagePermissions $
--             newDeleteImagePermissionsResponse
--
--         , responseStopFleet $
--             newStopFleetResponse
--
--         , responseStartImageBuilder $
--             newStartImageBuilderResponse
--
--         , responseBatchAssociateUserStack $
--             newBatchAssociateUserStackResponse
--
--         , responseDescribeImagePermissions $
--             newDescribeImagePermissionsResponse
--
--         , responseDeleteDirectoryConfig $
--             newDeleteDirectoryConfigResponse
--
--         , responseUpdateDirectoryConfig $
--             newUpdateDirectoryConfigResponse
--
--         , responseCreateFleet $
--             newCreateFleetResponse
--
--         , responseCreateStack $
--             newCreateStackResponse
--
--         , responseCopyImage $
--             newCopyImageResponse
--
--         , responseExpireSession $
--             newExpireSessionResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseDisableUser $
--             newDisableUserResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseCreateStreamingURL $
--             newCreateStreamingURLResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseStartFleet $
--             newStartFleetResponse
--
--         , responseStopImageBuilder $
--             newStopImageBuilderResponse
--
--         , responseDeleteImage $
--             newDeleteImageResponse
--
--         , responseDeleteUsageReportSubscription $
--             newDeleteUsageReportSubscriptionResponse
--
--         , responseBatchDisassociateUserStack $
--             newBatchDisassociateUserStackResponse
--
--         , responseDescribeImages $
--             newDescribeImagesResponse
--
--           ]
--     ]

-- Requests

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

requestListAssociatedFleets :: ListAssociatedFleets -> TestTree
requestListAssociatedFleets =
  req
    "ListAssociatedFleets"
    "fixture/ListAssociatedFleets.yaml"

requestDeleteStack :: DeleteStack -> TestTree
requestDeleteStack =
  req
    "DeleteStack"
    "fixture/DeleteStack.yaml"

requestUpdateStack :: UpdateStack -> TestTree
requestUpdateStack =
  req
    "UpdateStack"
    "fixture/UpdateStack.yaml"

requestCreateDirectoryConfig :: CreateDirectoryConfig -> TestTree
requestCreateDirectoryConfig =
  req
    "CreateDirectoryConfig"
    "fixture/CreateDirectoryConfig.yaml"

requestDescribeUsers :: DescribeUsers -> TestTree
requestDescribeUsers =
  req
    "DescribeUsers"
    "fixture/DescribeUsers.yaml"

requestListAssociatedStacks :: ListAssociatedStacks -> TestTree
requestListAssociatedStacks =
  req
    "ListAssociatedStacks"
    "fixture/ListAssociatedStacks.yaml"

requestDeleteFleet :: DeleteFleet -> TestTree
requestDeleteFleet =
  req
    "DeleteFleet"
    "fixture/DeleteFleet.yaml"

requestUpdateFleet :: UpdateFleet -> TestTree
requestUpdateFleet =
  req
    "UpdateFleet"
    "fixture/UpdateFleet.yaml"

requestDeleteImageBuilder :: DeleteImageBuilder -> TestTree
requestDeleteImageBuilder =
  req
    "DeleteImageBuilder"
    "fixture/DeleteImageBuilder.yaml"

requestAssociateFleet :: AssociateFleet -> TestTree
requestAssociateFleet =
  req
    "AssociateFleet"
    "fixture/AssociateFleet.yaml"

requestCreateImageBuilder :: CreateImageBuilder -> TestTree
requestCreateImageBuilder =
  req
    "CreateImageBuilder"
    "fixture/CreateImageBuilder.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestDescribeDirectoryConfigs :: DescribeDirectoryConfigs -> TestTree
requestDescribeDirectoryConfigs =
  req
    "DescribeDirectoryConfigs"
    "fixture/DescribeDirectoryConfigs.yaml"

requestCreateImageBuilderStreamingURL :: CreateImageBuilderStreamingURL -> TestTree
requestCreateImageBuilderStreamingURL =
  req
    "CreateImageBuilderStreamingURL"
    "fixture/CreateImageBuilderStreamingURL.yaml"

requestDescribeSessions :: DescribeSessions -> TestTree
requestDescribeSessions =
  req
    "DescribeSessions"
    "fixture/DescribeSessions.yaml"

requestDescribeStacks :: DescribeStacks -> TestTree
requestDescribeStacks =
  req
    "DescribeStacks"
    "fixture/DescribeStacks.yaml"

requestDescribeFleets :: DescribeFleets -> TestTree
requestDescribeFleets =
  req
    "DescribeFleets"
    "fixture/DescribeFleets.yaml"

requestDescribeImageBuilders :: DescribeImageBuilders -> TestTree
requestDescribeImageBuilders =
  req
    "DescribeImageBuilders"
    "fixture/DescribeImageBuilders.yaml"

requestEnableUser :: EnableUser -> TestTree
requestEnableUser =
  req
    "EnableUser"
    "fixture/EnableUser.yaml"

requestDescribeUserStackAssociations :: DescribeUserStackAssociations -> TestTree
requestDescribeUserStackAssociations =
  req
    "DescribeUserStackAssociations"
    "fixture/DescribeUserStackAssociations.yaml"

requestCreateUpdatedImage :: CreateUpdatedImage -> TestTree
requestCreateUpdatedImage =
  req
    "CreateUpdatedImage"
    "fixture/CreateUpdatedImage.yaml"

requestDescribeUsageReportSubscriptions :: DescribeUsageReportSubscriptions -> TestTree
requestDescribeUsageReportSubscriptions =
  req
    "DescribeUsageReportSubscriptions"
    "fixture/DescribeUsageReportSubscriptions.yaml"

requestUpdateImagePermissions :: UpdateImagePermissions -> TestTree
requestUpdateImagePermissions =
  req
    "UpdateImagePermissions"
    "fixture/UpdateImagePermissions.yaml"

requestDeleteImagePermissions :: DeleteImagePermissions -> TestTree
requestDeleteImagePermissions =
  req
    "DeleteImagePermissions"
    "fixture/DeleteImagePermissions.yaml"

requestStopFleet :: StopFleet -> TestTree
requestStopFleet =
  req
    "StopFleet"
    "fixture/StopFleet.yaml"

requestStartImageBuilder :: StartImageBuilder -> TestTree
requestStartImageBuilder =
  req
    "StartImageBuilder"
    "fixture/StartImageBuilder.yaml"

requestBatchAssociateUserStack :: BatchAssociateUserStack -> TestTree
requestBatchAssociateUserStack =
  req
    "BatchAssociateUserStack"
    "fixture/BatchAssociateUserStack.yaml"

requestDescribeImagePermissions :: DescribeImagePermissions -> TestTree
requestDescribeImagePermissions =
  req
    "DescribeImagePermissions"
    "fixture/DescribeImagePermissions.yaml"

requestDeleteDirectoryConfig :: DeleteDirectoryConfig -> TestTree
requestDeleteDirectoryConfig =
  req
    "DeleteDirectoryConfig"
    "fixture/DeleteDirectoryConfig.yaml"

requestUpdateDirectoryConfig :: UpdateDirectoryConfig -> TestTree
requestUpdateDirectoryConfig =
  req
    "UpdateDirectoryConfig"
    "fixture/UpdateDirectoryConfig.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet =
  req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

requestCreateStack :: CreateStack -> TestTree
requestCreateStack =
  req
    "CreateStack"
    "fixture/CreateStack.yaml"

requestCopyImage :: CopyImage -> TestTree
requestCopyImage =
  req
    "CopyImage"
    "fixture/CopyImage.yaml"

requestExpireSession :: ExpireSession -> TestTree
requestExpireSession =
  req
    "ExpireSession"
    "fixture/ExpireSession.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

requestDisableUser :: DisableUser -> TestTree
requestDisableUser =
  req
    "DisableUser"
    "fixture/DisableUser.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestCreateStreamingURL :: CreateStreamingURL -> TestTree
requestCreateStreamingURL =
  req
    "CreateStreamingURL"
    "fixture/CreateStreamingURL.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestStartFleet :: StartFleet -> TestTree
requestStartFleet =
  req
    "StartFleet"
    "fixture/StartFleet.yaml"

requestStopImageBuilder :: StopImageBuilder -> TestTree
requestStopImageBuilder =
  req
    "StopImageBuilder"
    "fixture/StopImageBuilder.yaml"

requestDeleteImage :: DeleteImage -> TestTree
requestDeleteImage =
  req
    "DeleteImage"
    "fixture/DeleteImage.yaml"

requestDeleteUsageReportSubscription :: DeleteUsageReportSubscription -> TestTree
requestDeleteUsageReportSubscription =
  req
    "DeleteUsageReportSubscription"
    "fixture/DeleteUsageReportSubscription.yaml"

requestBatchDisassociateUserStack :: BatchDisassociateUserStack -> TestTree
requestBatchDisassociateUserStack =
  req
    "BatchDisassociateUserStack"
    "fixture/BatchDisassociateUserStack.yaml"

requestDescribeImages :: DescribeImages -> TestTree
requestDescribeImages =
  req
    "DescribeImages"
    "fixture/DescribeImages.yaml"

-- Responses

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

responseListAssociatedFleets :: ListAssociatedFleetsResponse -> TestTree
responseListAssociatedFleets =
  res
    "ListAssociatedFleetsResponse"
    "fixture/ListAssociatedFleetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssociatedFleets)

responseDeleteStack :: DeleteStackResponse -> TestTree
responseDeleteStack =
  res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteStack)

responseUpdateStack :: UpdateStackResponse -> TestTree
responseUpdateStack =
  res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateStack)

responseCreateDirectoryConfig :: CreateDirectoryConfigResponse -> TestTree
responseCreateDirectoryConfig =
  res
    "CreateDirectoryConfigResponse"
    "fixture/CreateDirectoryConfigResponse.proto"
    defaultService
    (Proxy :: Proxy CreateDirectoryConfig)

responseDescribeUsers :: DescribeUsersResponse -> TestTree
responseDescribeUsers =
  res
    "DescribeUsersResponse"
    "fixture/DescribeUsersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUsers)

responseListAssociatedStacks :: ListAssociatedStacksResponse -> TestTree
responseListAssociatedStacks =
  res
    "ListAssociatedStacksResponse"
    "fixture/ListAssociatedStacksResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssociatedStacks)

responseDeleteFleet :: DeleteFleetResponse -> TestTree
responseDeleteFleet =
  res
    "DeleteFleetResponse"
    "fixture/DeleteFleetResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteFleet)

responseUpdateFleet :: UpdateFleetResponse -> TestTree
responseUpdateFleet =
  res
    "UpdateFleetResponse"
    "fixture/UpdateFleetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFleet)

responseDeleteImageBuilder :: DeleteImageBuilderResponse -> TestTree
responseDeleteImageBuilder =
  res
    "DeleteImageBuilderResponse"
    "fixture/DeleteImageBuilderResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteImageBuilder)

responseAssociateFleet :: AssociateFleetResponse -> TestTree
responseAssociateFleet =
  res
    "AssociateFleetResponse"
    "fixture/AssociateFleetResponse.proto"
    defaultService
    (Proxy :: Proxy AssociateFleet)

responseCreateImageBuilder :: CreateImageBuilderResponse -> TestTree
responseCreateImageBuilder =
  res
    "CreateImageBuilderResponse"
    "fixture/CreateImageBuilderResponse.proto"
    defaultService
    (Proxy :: Proxy CreateImageBuilder)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseDescribeDirectoryConfigs :: DescribeDirectoryConfigsResponse -> TestTree
responseDescribeDirectoryConfigs =
  res
    "DescribeDirectoryConfigsResponse"
    "fixture/DescribeDirectoryConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDirectoryConfigs)

responseCreateImageBuilderStreamingURL :: CreateImageBuilderStreamingURLResponse -> TestTree
responseCreateImageBuilderStreamingURL =
  res
    "CreateImageBuilderStreamingURLResponse"
    "fixture/CreateImageBuilderStreamingURLResponse.proto"
    defaultService
    (Proxy :: Proxy CreateImageBuilderStreamingURL)

responseDescribeSessions :: DescribeSessionsResponse -> TestTree
responseDescribeSessions =
  res
    "DescribeSessionsResponse"
    "fixture/DescribeSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSessions)

responseDescribeStacks :: DescribeStacksResponse -> TestTree
responseDescribeStacks =
  res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStacks)

responseDescribeFleets :: DescribeFleetsResponse -> TestTree
responseDescribeFleets =
  res
    "DescribeFleetsResponse"
    "fixture/DescribeFleetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleets)

responseDescribeImageBuilders :: DescribeImageBuildersResponse -> TestTree
responseDescribeImageBuilders =
  res
    "DescribeImageBuildersResponse"
    "fixture/DescribeImageBuildersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImageBuilders)

responseEnableUser :: EnableUserResponse -> TestTree
responseEnableUser =
  res
    "EnableUserResponse"
    "fixture/EnableUserResponse.proto"
    defaultService
    (Proxy :: Proxy EnableUser)

responseDescribeUserStackAssociations :: DescribeUserStackAssociationsResponse -> TestTree
responseDescribeUserStackAssociations =
  res
    "DescribeUserStackAssociationsResponse"
    "fixture/DescribeUserStackAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserStackAssociations)

responseCreateUpdatedImage :: CreateUpdatedImageResponse -> TestTree
responseCreateUpdatedImage =
  res
    "CreateUpdatedImageResponse"
    "fixture/CreateUpdatedImageResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUpdatedImage)

responseDescribeUsageReportSubscriptions :: DescribeUsageReportSubscriptionsResponse -> TestTree
responseDescribeUsageReportSubscriptions =
  res
    "DescribeUsageReportSubscriptionsResponse"
    "fixture/DescribeUsageReportSubscriptionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUsageReportSubscriptions)

responseUpdateImagePermissions :: UpdateImagePermissionsResponse -> TestTree
responseUpdateImagePermissions =
  res
    "UpdateImagePermissionsResponse"
    "fixture/UpdateImagePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateImagePermissions)

responseDeleteImagePermissions :: DeleteImagePermissionsResponse -> TestTree
responseDeleteImagePermissions =
  res
    "DeleteImagePermissionsResponse"
    "fixture/DeleteImagePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteImagePermissions)

responseStopFleet :: StopFleetResponse -> TestTree
responseStopFleet =
  res
    "StopFleetResponse"
    "fixture/StopFleetResponse.proto"
    defaultService
    (Proxy :: Proxy StopFleet)

responseStartImageBuilder :: StartImageBuilderResponse -> TestTree
responseStartImageBuilder =
  res
    "StartImageBuilderResponse"
    "fixture/StartImageBuilderResponse.proto"
    defaultService
    (Proxy :: Proxy StartImageBuilder)

responseBatchAssociateUserStack :: BatchAssociateUserStackResponse -> TestTree
responseBatchAssociateUserStack =
  res
    "BatchAssociateUserStackResponse"
    "fixture/BatchAssociateUserStackResponse.proto"
    defaultService
    (Proxy :: Proxy BatchAssociateUserStack)

responseDescribeImagePermissions :: DescribeImagePermissionsResponse -> TestTree
responseDescribeImagePermissions =
  res
    "DescribeImagePermissionsResponse"
    "fixture/DescribeImagePermissionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImagePermissions)

responseDeleteDirectoryConfig :: DeleteDirectoryConfigResponse -> TestTree
responseDeleteDirectoryConfig =
  res
    "DeleteDirectoryConfigResponse"
    "fixture/DeleteDirectoryConfigResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDirectoryConfig)

responseUpdateDirectoryConfig :: UpdateDirectoryConfigResponse -> TestTree
responseUpdateDirectoryConfig =
  res
    "UpdateDirectoryConfigResponse"
    "fixture/UpdateDirectoryConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDirectoryConfig)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFleet)

responseCreateStack :: CreateStackResponse -> TestTree
responseCreateStack =
  res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStack)

responseCopyImage :: CopyImageResponse -> TestTree
responseCopyImage =
  res
    "CopyImageResponse"
    "fixture/CopyImageResponse.proto"
    defaultService
    (Proxy :: Proxy CopyImage)

responseExpireSession :: ExpireSessionResponse -> TestTree
responseExpireSession =
  res
    "ExpireSessionResponse"
    "fixture/ExpireSessionResponse.proto"
    defaultService
    (Proxy :: Proxy ExpireSession)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUser)

responseDisableUser :: DisableUserResponse -> TestTree
responseDisableUser =
  res
    "DisableUserResponse"
    "fixture/DisableUserResponse.proto"
    defaultService
    (Proxy :: Proxy DisableUser)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUser)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseCreateStreamingURL :: CreateStreamingURLResponse -> TestTree
responseCreateStreamingURL =
  res
    "CreateStreamingURLResponse"
    "fixture/CreateStreamingURLResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStreamingURL)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseStartFleet :: StartFleetResponse -> TestTree
responseStartFleet =
  res
    "StartFleetResponse"
    "fixture/StartFleetResponse.proto"
    defaultService
    (Proxy :: Proxy StartFleet)

responseStopImageBuilder :: StopImageBuilderResponse -> TestTree
responseStopImageBuilder =
  res
    "StopImageBuilderResponse"
    "fixture/StopImageBuilderResponse.proto"
    defaultService
    (Proxy :: Proxy StopImageBuilder)

responseDeleteImage :: DeleteImageResponse -> TestTree
responseDeleteImage =
  res
    "DeleteImageResponse"
    "fixture/DeleteImageResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteImage)

responseDeleteUsageReportSubscription :: DeleteUsageReportSubscriptionResponse -> TestTree
responseDeleteUsageReportSubscription =
  res
    "DeleteUsageReportSubscriptionResponse"
    "fixture/DeleteUsageReportSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUsageReportSubscription)

responseBatchDisassociateUserStack :: BatchDisassociateUserStackResponse -> TestTree
responseBatchDisassociateUserStack =
  res
    "BatchDisassociateUserStackResponse"
    "fixture/BatchDisassociateUserStackResponse.proto"
    defaultService
    (Proxy :: Proxy BatchDisassociateUserStack)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages =
  res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImages)
