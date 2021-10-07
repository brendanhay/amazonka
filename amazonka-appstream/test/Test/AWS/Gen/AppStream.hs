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
--         [ requestListAssociatedStacks $
--             newListAssociatedStacks
--
--         , requestDeleteImageBuilder $
--             newDeleteImageBuilder
--
--         , requestBatchAssociateUserStack $
--             newBatchAssociateUserStack
--
--         , requestListAssociatedFleets $
--             newListAssociatedFleets
--
--         , requestDeleteUsageReportSubscription $
--             newDeleteUsageReportSubscription
--
--         , requestStopFleet $
--             newStopFleet
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
--         , requestUntagResource $
--             newUntagResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestDescribeFleets $
--             newDescribeFleets
--
--         , requestDescribeSessions $
--             newDescribeSessions
--
--         , requestEnableUser $
--             newEnableUser
--
--         , requestDescribeStacks $
--             newDescribeStacks
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestDeleteDirectoryConfig $
--             newDeleteDirectoryConfig
--
--         , requestCopyImage $
--             newCopyImage
--
--         , requestCreateImageBuilder $
--             newCreateImageBuilder
--
--         , requestCreateStack $
--             newCreateStack
--
--         , requestCreateFleet $
--             newCreateFleet
--
--         , requestUpdateDirectoryConfig $
--             newUpdateDirectoryConfig
--
--         , requestAssociateFleet $
--             newAssociateFleet
--
--         , requestCreateDirectoryConfig $
--             newCreateDirectoryConfig
--
--         , requestDeleteStack $
--             newDeleteStack
--
--         , requestDeleteFleet $
--             newDeleteFleet
--
--         , requestUpdateStack $
--             newUpdateStack
--
--         , requestDescribeUsers $
--             newDescribeUsers
--
--         , requestUpdateFleet $
--             newUpdateFleet
--
--         , requestCreateUsageReportSubscription $
--             newCreateUsageReportSubscription
--
--         , requestDisassociateFleet $
--             newDisassociateFleet
--
--         , requestBatchDisassociateUserStack $
--             newBatchDisassociateUserStack
--
--         , requestDescribeImages $
--             newDescribeImages
--
--         , requestDescribeUsageReportSubscriptions $
--             newDescribeUsageReportSubscriptions
--
--         , requestUpdateImagePermissions $
--             newUpdateImagePermissions
--
--         , requestCreateUpdatedImage $
--             newCreateUpdatedImage
--
--         , requestDeleteImage $
--             newDeleteImage
--
--         , requestDeleteImagePermissions $
--             newDeleteImagePermissions
--
--         , requestCreateStreamingURL $
--             newCreateStreamingURL
--
--         , requestDescribeImageBuilders $
--             newDescribeImageBuilders
--
--         , requestDescribeUserStackAssociations $
--             newDescribeUserStackAssociations
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDisableUser $
--             newDisableUser
--
--         , requestCreateImageBuilderStreamingURL $
--             newCreateImageBuilderStreamingURL
--
--         , requestExpireSession $
--             newExpireSession
--
--         , requestDescribeDirectoryConfigs $
--             newDescribeDirectoryConfigs
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestDescribeImagePermissions $
--             newDescribeImagePermissions
--
--           ]

--     , testGroup "response"
--         [ responseListAssociatedStacks $
--             newListAssociatedStacksResponse
--
--         , responseDeleteImageBuilder $
--             newDeleteImageBuilderResponse
--
--         , responseBatchAssociateUserStack $
--             newBatchAssociateUserStackResponse
--
--         , responseListAssociatedFleets $
--             newListAssociatedFleetsResponse
--
--         , responseDeleteUsageReportSubscription $
--             newDeleteUsageReportSubscriptionResponse
--
--         , responseStopFleet $
--             newStopFleetResponse
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
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseDescribeFleets $
--             newDescribeFleetsResponse
--
--         , responseDescribeSessions $
--             newDescribeSessionsResponse
--
--         , responseEnableUser $
--             newEnableUserResponse
--
--         , responseDescribeStacks $
--             newDescribeStacksResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseDeleteDirectoryConfig $
--             newDeleteDirectoryConfigResponse
--
--         , responseCopyImage $
--             newCopyImageResponse
--
--         , responseCreateImageBuilder $
--             newCreateImageBuilderResponse
--
--         , responseCreateStack $
--             newCreateStackResponse
--
--         , responseCreateFleet $
--             newCreateFleetResponse
--
--         , responseUpdateDirectoryConfig $
--             newUpdateDirectoryConfigResponse
--
--         , responseAssociateFleet $
--             newAssociateFleetResponse
--
--         , responseCreateDirectoryConfig $
--             newCreateDirectoryConfigResponse
--
--         , responseDeleteStack $
--             newDeleteStackResponse
--
--         , responseDeleteFleet $
--             newDeleteFleetResponse
--
--         , responseUpdateStack $
--             newUpdateStackResponse
--
--         , responseDescribeUsers $
--             newDescribeUsersResponse
--
--         , responseUpdateFleet $
--             newUpdateFleetResponse
--
--         , responseCreateUsageReportSubscription $
--             newCreateUsageReportSubscriptionResponse
--
--         , responseDisassociateFleet $
--             newDisassociateFleetResponse
--
--         , responseBatchDisassociateUserStack $
--             newBatchDisassociateUserStackResponse
--
--         , responseDescribeImages $
--             newDescribeImagesResponse
--
--         , responseDescribeUsageReportSubscriptions $
--             newDescribeUsageReportSubscriptionsResponse
--
--         , responseUpdateImagePermissions $
--             newUpdateImagePermissionsResponse
--
--         , responseCreateUpdatedImage $
--             newCreateUpdatedImageResponse
--
--         , responseDeleteImage $
--             newDeleteImageResponse
--
--         , responseDeleteImagePermissions $
--             newDeleteImagePermissionsResponse
--
--         , responseCreateStreamingURL $
--             newCreateStreamingURLResponse
--
--         , responseDescribeImageBuilders $
--             newDescribeImageBuildersResponse
--
--         , responseDescribeUserStackAssociations $
--             newDescribeUserStackAssociationsResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDisableUser $
--             newDisableUserResponse
--
--         , responseCreateImageBuilderStreamingURL $
--             newCreateImageBuilderStreamingURLResponse
--
--         , responseExpireSession $
--             newExpireSessionResponse
--
--         , responseDescribeDirectoryConfigs $
--             newDescribeDirectoryConfigsResponse
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

requestListAssociatedStacks :: ListAssociatedStacks -> TestTree
requestListAssociatedStacks =
  req
    "ListAssociatedStacks"
    "fixture/ListAssociatedStacks.yaml"

requestDeleteImageBuilder :: DeleteImageBuilder -> TestTree
requestDeleteImageBuilder =
  req
    "DeleteImageBuilder"
    "fixture/DeleteImageBuilder.yaml"

requestBatchAssociateUserStack :: BatchAssociateUserStack -> TestTree
requestBatchAssociateUserStack =
  req
    "BatchAssociateUserStack"
    "fixture/BatchAssociateUserStack.yaml"

requestListAssociatedFleets :: ListAssociatedFleets -> TestTree
requestListAssociatedFleets =
  req
    "ListAssociatedFleets"
    "fixture/ListAssociatedFleets.yaml"

requestDeleteUsageReportSubscription :: DeleteUsageReportSubscription -> TestTree
requestDeleteUsageReportSubscription =
  req
    "DeleteUsageReportSubscription"
    "fixture/DeleteUsageReportSubscription.yaml"

requestStopFleet :: StopFleet -> TestTree
requestStopFleet =
  req
    "StopFleet"
    "fixture/StopFleet.yaml"

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

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestDescribeFleets :: DescribeFleets -> TestTree
requestDescribeFleets =
  req
    "DescribeFleets"
    "fixture/DescribeFleets.yaml"

requestDescribeSessions :: DescribeSessions -> TestTree
requestDescribeSessions =
  req
    "DescribeSessions"
    "fixture/DescribeSessions.yaml"

requestEnableUser :: EnableUser -> TestTree
requestEnableUser =
  req
    "EnableUser"
    "fixture/EnableUser.yaml"

requestDescribeStacks :: DescribeStacks -> TestTree
requestDescribeStacks =
  req
    "DescribeStacks"
    "fixture/DescribeStacks.yaml"

requestCreateUser :: CreateUser -> TestTree
requestCreateUser =
  req
    "CreateUser"
    "fixture/CreateUser.yaml"

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

requestCreateImageBuilder :: CreateImageBuilder -> TestTree
requestCreateImageBuilder =
  req
    "CreateImageBuilder"
    "fixture/CreateImageBuilder.yaml"

requestCreateStack :: CreateStack -> TestTree
requestCreateStack =
  req
    "CreateStack"
    "fixture/CreateStack.yaml"

requestCreateFleet :: CreateFleet -> TestTree
requestCreateFleet =
  req
    "CreateFleet"
    "fixture/CreateFleet.yaml"

requestUpdateDirectoryConfig :: UpdateDirectoryConfig -> TestTree
requestUpdateDirectoryConfig =
  req
    "UpdateDirectoryConfig"
    "fixture/UpdateDirectoryConfig.yaml"

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

requestUpdateStack :: UpdateStack -> TestTree
requestUpdateStack =
  req
    "UpdateStack"
    "fixture/UpdateStack.yaml"

requestDescribeUsers :: DescribeUsers -> TestTree
requestDescribeUsers =
  req
    "DescribeUsers"
    "fixture/DescribeUsers.yaml"

requestUpdateFleet :: UpdateFleet -> TestTree
requestUpdateFleet =
  req
    "UpdateFleet"
    "fixture/UpdateFleet.yaml"

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

requestCreateUpdatedImage :: CreateUpdatedImage -> TestTree
requestCreateUpdatedImage =
  req
    "CreateUpdatedImage"
    "fixture/CreateUpdatedImage.yaml"

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

requestCreateStreamingURL :: CreateStreamingURL -> TestTree
requestCreateStreamingURL =
  req
    "CreateStreamingURL"
    "fixture/CreateStreamingURL.yaml"

requestDescribeImageBuilders :: DescribeImageBuilders -> TestTree
requestDescribeImageBuilders =
  req
    "DescribeImageBuilders"
    "fixture/DescribeImageBuilders.yaml"

requestDescribeUserStackAssociations :: DescribeUserStackAssociations -> TestTree
requestDescribeUserStackAssociations =
  req
    "DescribeUserStackAssociations"
    "fixture/DescribeUserStackAssociations.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestDisableUser :: DisableUser -> TestTree
requestDisableUser =
  req
    "DisableUser"
    "fixture/DisableUser.yaml"

requestCreateImageBuilderStreamingURL :: CreateImageBuilderStreamingURL -> TestTree
requestCreateImageBuilderStreamingURL =
  req
    "CreateImageBuilderStreamingURL"
    "fixture/CreateImageBuilderStreamingURL.yaml"

requestExpireSession :: ExpireSession -> TestTree
requestExpireSession =
  req
    "ExpireSession"
    "fixture/ExpireSession.yaml"

requestDescribeDirectoryConfigs :: DescribeDirectoryConfigs -> TestTree
requestDescribeDirectoryConfigs =
  req
    "DescribeDirectoryConfigs"
    "fixture/DescribeDirectoryConfigs.yaml"

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

responseListAssociatedStacks :: ListAssociatedStacksResponse -> TestTree
responseListAssociatedStacks =
  res
    "ListAssociatedStacksResponse"
    "fixture/ListAssociatedStacksResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssociatedStacks)

responseDeleteImageBuilder :: DeleteImageBuilderResponse -> TestTree
responseDeleteImageBuilder =
  res
    "DeleteImageBuilderResponse"
    "fixture/DeleteImageBuilderResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteImageBuilder)

responseBatchAssociateUserStack :: BatchAssociateUserStackResponse -> TestTree
responseBatchAssociateUserStack =
  res
    "BatchAssociateUserStackResponse"
    "fixture/BatchAssociateUserStackResponse.proto"
    defaultService
    (Proxy :: Proxy BatchAssociateUserStack)

responseListAssociatedFleets :: ListAssociatedFleetsResponse -> TestTree
responseListAssociatedFleets =
  res
    "ListAssociatedFleetsResponse"
    "fixture/ListAssociatedFleetsResponse.proto"
    defaultService
    (Proxy :: Proxy ListAssociatedFleets)

responseDeleteUsageReportSubscription :: DeleteUsageReportSubscriptionResponse -> TestTree
responseDeleteUsageReportSubscription =
  res
    "DeleteUsageReportSubscriptionResponse"
    "fixture/DeleteUsageReportSubscriptionResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUsageReportSubscription)

responseStopFleet :: StopFleetResponse -> TestTree
responseStopFleet =
  res
    "StopFleetResponse"
    "fixture/StopFleetResponse.proto"
    defaultService
    (Proxy :: Proxy StopFleet)

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

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseDescribeFleets :: DescribeFleetsResponse -> TestTree
responseDescribeFleets =
  res
    "DescribeFleetsResponse"
    "fixture/DescribeFleetsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeFleets)

responseDescribeSessions :: DescribeSessionsResponse -> TestTree
responseDescribeSessions =
  res
    "DescribeSessionsResponse"
    "fixture/DescribeSessionsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeSessions)

responseEnableUser :: EnableUserResponse -> TestTree
responseEnableUser =
  res
    "EnableUserResponse"
    "fixture/EnableUserResponse.proto"
    defaultService
    (Proxy :: Proxy EnableUser)

responseDescribeStacks :: DescribeStacksResponse -> TestTree
responseDescribeStacks =
  res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeStacks)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUser)

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

responseCreateImageBuilder :: CreateImageBuilderResponse -> TestTree
responseCreateImageBuilder =
  res
    "CreateImageBuilderResponse"
    "fixture/CreateImageBuilderResponse.proto"
    defaultService
    (Proxy :: Proxy CreateImageBuilder)

responseCreateStack :: CreateStackResponse -> TestTree
responseCreateStack =
  res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStack)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    defaultService
    (Proxy :: Proxy CreateFleet)

responseUpdateDirectoryConfig :: UpdateDirectoryConfigResponse -> TestTree
responseUpdateDirectoryConfig =
  res
    "UpdateDirectoryConfigResponse"
    "fixture/UpdateDirectoryConfigResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDirectoryConfig)

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

responseUpdateStack :: UpdateStackResponse -> TestTree
responseUpdateStack =
  res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateStack)

responseDescribeUsers :: DescribeUsersResponse -> TestTree
responseDescribeUsers =
  res
    "DescribeUsersResponse"
    "fixture/DescribeUsersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUsers)

responseUpdateFleet :: UpdateFleetResponse -> TestTree
responseUpdateFleet =
  res
    "UpdateFleetResponse"
    "fixture/UpdateFleetResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateFleet)

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

responseCreateUpdatedImage :: CreateUpdatedImageResponse -> TestTree
responseCreateUpdatedImage =
  res
    "CreateUpdatedImageResponse"
    "fixture/CreateUpdatedImageResponse.proto"
    defaultService
    (Proxy :: Proxy CreateUpdatedImage)

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

responseCreateStreamingURL :: CreateStreamingURLResponse -> TestTree
responseCreateStreamingURL =
  res
    "CreateStreamingURLResponse"
    "fixture/CreateStreamingURLResponse.proto"
    defaultService
    (Proxy :: Proxy CreateStreamingURL)

responseDescribeImageBuilders :: DescribeImageBuildersResponse -> TestTree
responseDescribeImageBuilders =
  res
    "DescribeImageBuildersResponse"
    "fixture/DescribeImageBuildersResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeImageBuilders)

responseDescribeUserStackAssociations :: DescribeUserStackAssociationsResponse -> TestTree
responseDescribeUserStackAssociations =
  res
    "DescribeUserStackAssociationsResponse"
    "fixture/DescribeUserStackAssociationsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeUserStackAssociations)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteUser)

responseDisableUser :: DisableUserResponse -> TestTree
responseDisableUser =
  res
    "DisableUserResponse"
    "fixture/DisableUserResponse.proto"
    defaultService
    (Proxy :: Proxy DisableUser)

responseCreateImageBuilderStreamingURL :: CreateImageBuilderStreamingURLResponse -> TestTree
responseCreateImageBuilderStreamingURL =
  res
    "CreateImageBuilderStreamingURLResponse"
    "fixture/CreateImageBuilderStreamingURLResponse.proto"
    defaultService
    (Proxy :: Proxy CreateImageBuilderStreamingURL)

responseExpireSession :: ExpireSessionResponse -> TestTree
responseExpireSession =
  res
    "ExpireSessionResponse"
    "fixture/ExpireSessionResponse.proto"
    defaultService
    (Proxy :: Proxy ExpireSession)

responseDescribeDirectoryConfigs :: DescribeDirectoryConfigsResponse -> TestTree
responseDescribeDirectoryConfigs =
  res
    "DescribeDirectoryConfigsResponse"
    "fixture/DescribeDirectoryConfigsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDirectoryConfigs)

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
