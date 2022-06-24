{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.AppStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.AppStream where

import Amazonka.AppStream
import qualified Data.Proxy as Proxy
import Test.Amazonka.AppStream.Internal
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateFleet $
--             newAssociateFleet
--
--         , requestBatchAssociateUserStack $
--             newBatchAssociateUserStack
--
--         , requestBatchDisassociateUserStack $
--             newBatchDisassociateUserStack
--
--         , requestCopyImage $
--             newCopyImage
--
--         , requestCreateDirectoryConfig $
--             newCreateDirectoryConfig
--
--         , requestCreateFleet $
--             newCreateFleet
--
--         , requestCreateImageBuilder $
--             newCreateImageBuilder
--
--         , requestCreateImageBuilderStreamingURL $
--             newCreateImageBuilderStreamingURL
--
--         , requestCreateStack $
--             newCreateStack
--
--         , requestCreateStreamingURL $
--             newCreateStreamingURL
--
--         , requestCreateUpdatedImage $
--             newCreateUpdatedImage
--
--         , requestCreateUsageReportSubscription $
--             newCreateUsageReportSubscription
--
--         , requestCreateUser $
--             newCreateUser
--
--         , requestDeleteDirectoryConfig $
--             newDeleteDirectoryConfig
--
--         , requestDeleteFleet $
--             newDeleteFleet
--
--         , requestDeleteImage $
--             newDeleteImage
--
--         , requestDeleteImageBuilder $
--             newDeleteImageBuilder
--
--         , requestDeleteImagePermissions $
--             newDeleteImagePermissions
--
--         , requestDeleteStack $
--             newDeleteStack
--
--         , requestDeleteUsageReportSubscription $
--             newDeleteUsageReportSubscription
--
--         , requestDeleteUser $
--             newDeleteUser
--
--         , requestDescribeDirectoryConfigs $
--             newDescribeDirectoryConfigs
--
--         , requestDescribeFleets $
--             newDescribeFleets
--
--         , requestDescribeImageBuilders $
--             newDescribeImageBuilders
--
--         , requestDescribeImagePermissions $
--             newDescribeImagePermissions
--
--         , requestDescribeImages $
--             newDescribeImages
--
--         , requestDescribeSessions $
--             newDescribeSessions
--
--         , requestDescribeStacks $
--             newDescribeStacks
--
--         , requestDescribeUsageReportSubscriptions $
--             newDescribeUsageReportSubscriptions
--
--         , requestDescribeUserStackAssociations $
--             newDescribeUserStackAssociations
--
--         , requestDescribeUsers $
--             newDescribeUsers
--
--         , requestDisableUser $
--             newDisableUser
--
--         , requestDisassociateFleet $
--             newDisassociateFleet
--
--         , requestEnableUser $
--             newEnableUser
--
--         , requestExpireSession $
--             newExpireSession
--
--         , requestListAssociatedFleets $
--             newListAssociatedFleets
--
--         , requestListAssociatedStacks $
--             newListAssociatedStacks
--
--         , requestListTagsForResource $
--             newListTagsForResource
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
--         , requestStopImageBuilder $
--             newStopImageBuilder
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDirectoryConfig $
--             newUpdateDirectoryConfig
--
--         , requestUpdateFleet $
--             newUpdateFleet
--
--         , requestUpdateImagePermissions $
--             newUpdateImagePermissions
--
--         , requestUpdateStack $
--             newUpdateStack
--
--           ]

--     , testGroup "response"
--         [ responseAssociateFleet $
--             newAssociateFleetResponse
--
--         , responseBatchAssociateUserStack $
--             newBatchAssociateUserStackResponse
--
--         , responseBatchDisassociateUserStack $
--             newBatchDisassociateUserStackResponse
--
--         , responseCopyImage $
--             newCopyImageResponse
--
--         , responseCreateDirectoryConfig $
--             newCreateDirectoryConfigResponse
--
--         , responseCreateFleet $
--             newCreateFleetResponse
--
--         , responseCreateImageBuilder $
--             newCreateImageBuilderResponse
--
--         , responseCreateImageBuilderStreamingURL $
--             newCreateImageBuilderStreamingURLResponse
--
--         , responseCreateStack $
--             newCreateStackResponse
--
--         , responseCreateStreamingURL $
--             newCreateStreamingURLResponse
--
--         , responseCreateUpdatedImage $
--             newCreateUpdatedImageResponse
--
--         , responseCreateUsageReportSubscription $
--             newCreateUsageReportSubscriptionResponse
--
--         , responseCreateUser $
--             newCreateUserResponse
--
--         , responseDeleteDirectoryConfig $
--             newDeleteDirectoryConfigResponse
--
--         , responseDeleteFleet $
--             newDeleteFleetResponse
--
--         , responseDeleteImage $
--             newDeleteImageResponse
--
--         , responseDeleteImageBuilder $
--             newDeleteImageBuilderResponse
--
--         , responseDeleteImagePermissions $
--             newDeleteImagePermissionsResponse
--
--         , responseDeleteStack $
--             newDeleteStackResponse
--
--         , responseDeleteUsageReportSubscription $
--             newDeleteUsageReportSubscriptionResponse
--
--         , responseDeleteUser $
--             newDeleteUserResponse
--
--         , responseDescribeDirectoryConfigs $
--             newDescribeDirectoryConfigsResponse
--
--         , responseDescribeFleets $
--             newDescribeFleetsResponse
--
--         , responseDescribeImageBuilders $
--             newDescribeImageBuildersResponse
--
--         , responseDescribeImagePermissions $
--             newDescribeImagePermissionsResponse
--
--         , responseDescribeImages $
--             newDescribeImagesResponse
--
--         , responseDescribeSessions $
--             newDescribeSessionsResponse
--
--         , responseDescribeStacks $
--             newDescribeStacksResponse
--
--         , responseDescribeUsageReportSubscriptions $
--             newDescribeUsageReportSubscriptionsResponse
--
--         , responseDescribeUserStackAssociations $
--             newDescribeUserStackAssociationsResponse
--
--         , responseDescribeUsers $
--             newDescribeUsersResponse
--
--         , responseDisableUser $
--             newDisableUserResponse
--
--         , responseDisassociateFleet $
--             newDisassociateFleetResponse
--
--         , responseEnableUser $
--             newEnableUserResponse
--
--         , responseExpireSession $
--             newExpireSessionResponse
--
--         , responseListAssociatedFleets $
--             newListAssociatedFleetsResponse
--
--         , responseListAssociatedStacks $
--             newListAssociatedStacksResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
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
--         , responseStopImageBuilder $
--             newStopImageBuilderResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDirectoryConfig $
--             newUpdateDirectoryConfigResponse
--
--         , responseUpdateFleet $
--             newUpdateFleetResponse
--
--         , responseUpdateImagePermissions $
--             newUpdateImagePermissionsResponse
--
--         , responseUpdateStack $
--             newUpdateStackResponse
--
--           ]
--     ]

-- Requests

requestAssociateFleet :: AssociateFleet -> TestTree
requestAssociateFleet =
  req
    "AssociateFleet"
    "fixture/AssociateFleet.yaml"

requestBatchAssociateUserStack :: BatchAssociateUserStack -> TestTree
requestBatchAssociateUserStack =
  req
    "BatchAssociateUserStack"
    "fixture/BatchAssociateUserStack.yaml"

requestBatchDisassociateUserStack :: BatchDisassociateUserStack -> TestTree
requestBatchDisassociateUserStack =
  req
    "BatchDisassociateUserStack"
    "fixture/BatchDisassociateUserStack.yaml"

requestCopyImage :: CopyImage -> TestTree
requestCopyImage =
  req
    "CopyImage"
    "fixture/CopyImage.yaml"

requestCreateDirectoryConfig :: CreateDirectoryConfig -> TestTree
requestCreateDirectoryConfig =
  req
    "CreateDirectoryConfig"
    "fixture/CreateDirectoryConfig.yaml"

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

requestCreateImageBuilderStreamingURL :: CreateImageBuilderStreamingURL -> TestTree
requestCreateImageBuilderStreamingURL =
  req
    "CreateImageBuilderStreamingURL"
    "fixture/CreateImageBuilderStreamingURL.yaml"

requestCreateStack :: CreateStack -> TestTree
requestCreateStack =
  req
    "CreateStack"
    "fixture/CreateStack.yaml"

requestCreateStreamingURL :: CreateStreamingURL -> TestTree
requestCreateStreamingURL =
  req
    "CreateStreamingURL"
    "fixture/CreateStreamingURL.yaml"

requestCreateUpdatedImage :: CreateUpdatedImage -> TestTree
requestCreateUpdatedImage =
  req
    "CreateUpdatedImage"
    "fixture/CreateUpdatedImage.yaml"

requestCreateUsageReportSubscription :: CreateUsageReportSubscription -> TestTree
requestCreateUsageReportSubscription =
  req
    "CreateUsageReportSubscription"
    "fixture/CreateUsageReportSubscription.yaml"

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

requestDeleteFleet :: DeleteFleet -> TestTree
requestDeleteFleet =
  req
    "DeleteFleet"
    "fixture/DeleteFleet.yaml"

requestDeleteImage :: DeleteImage -> TestTree
requestDeleteImage =
  req
    "DeleteImage"
    "fixture/DeleteImage.yaml"

requestDeleteImageBuilder :: DeleteImageBuilder -> TestTree
requestDeleteImageBuilder =
  req
    "DeleteImageBuilder"
    "fixture/DeleteImageBuilder.yaml"

requestDeleteImagePermissions :: DeleteImagePermissions -> TestTree
requestDeleteImagePermissions =
  req
    "DeleteImagePermissions"
    "fixture/DeleteImagePermissions.yaml"

requestDeleteStack :: DeleteStack -> TestTree
requestDeleteStack =
  req
    "DeleteStack"
    "fixture/DeleteStack.yaml"

requestDeleteUsageReportSubscription :: DeleteUsageReportSubscription -> TestTree
requestDeleteUsageReportSubscription =
  req
    "DeleteUsageReportSubscription"
    "fixture/DeleteUsageReportSubscription.yaml"

requestDeleteUser :: DeleteUser -> TestTree
requestDeleteUser =
  req
    "DeleteUser"
    "fixture/DeleteUser.yaml"

requestDescribeDirectoryConfigs :: DescribeDirectoryConfigs -> TestTree
requestDescribeDirectoryConfigs =
  req
    "DescribeDirectoryConfigs"
    "fixture/DescribeDirectoryConfigs.yaml"

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

requestDescribeImagePermissions :: DescribeImagePermissions -> TestTree
requestDescribeImagePermissions =
  req
    "DescribeImagePermissions"
    "fixture/DescribeImagePermissions.yaml"

requestDescribeImages :: DescribeImages -> TestTree
requestDescribeImages =
  req
    "DescribeImages"
    "fixture/DescribeImages.yaml"

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

requestDescribeUsageReportSubscriptions :: DescribeUsageReportSubscriptions -> TestTree
requestDescribeUsageReportSubscriptions =
  req
    "DescribeUsageReportSubscriptions"
    "fixture/DescribeUsageReportSubscriptions.yaml"

requestDescribeUserStackAssociations :: DescribeUserStackAssociations -> TestTree
requestDescribeUserStackAssociations =
  req
    "DescribeUserStackAssociations"
    "fixture/DescribeUserStackAssociations.yaml"

requestDescribeUsers :: DescribeUsers -> TestTree
requestDescribeUsers =
  req
    "DescribeUsers"
    "fixture/DescribeUsers.yaml"

requestDisableUser :: DisableUser -> TestTree
requestDisableUser =
  req
    "DisableUser"
    "fixture/DisableUser.yaml"

requestDisassociateFleet :: DisassociateFleet -> TestTree
requestDisassociateFleet =
  req
    "DisassociateFleet"
    "fixture/DisassociateFleet.yaml"

requestEnableUser :: EnableUser -> TestTree
requestEnableUser =
  req
    "EnableUser"
    "fixture/EnableUser.yaml"

requestExpireSession :: ExpireSession -> TestTree
requestExpireSession =
  req
    "ExpireSession"
    "fixture/ExpireSession.yaml"

requestListAssociatedFleets :: ListAssociatedFleets -> TestTree
requestListAssociatedFleets =
  req
    "ListAssociatedFleets"
    "fixture/ListAssociatedFleets.yaml"

requestListAssociatedStacks :: ListAssociatedStacks -> TestTree
requestListAssociatedStacks =
  req
    "ListAssociatedStacks"
    "fixture/ListAssociatedStacks.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

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

requestStopImageBuilder :: StopImageBuilder -> TestTree
requestStopImageBuilder =
  req
    "StopImageBuilder"
    "fixture/StopImageBuilder.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateDirectoryConfig :: UpdateDirectoryConfig -> TestTree
requestUpdateDirectoryConfig =
  req
    "UpdateDirectoryConfig"
    "fixture/UpdateDirectoryConfig.yaml"

requestUpdateFleet :: UpdateFleet -> TestTree
requestUpdateFleet =
  req
    "UpdateFleet"
    "fixture/UpdateFleet.yaml"

requestUpdateImagePermissions :: UpdateImagePermissions -> TestTree
requestUpdateImagePermissions =
  req
    "UpdateImagePermissions"
    "fixture/UpdateImagePermissions.yaml"

requestUpdateStack :: UpdateStack -> TestTree
requestUpdateStack =
  req
    "UpdateStack"
    "fixture/UpdateStack.yaml"

-- Responses

responseAssociateFleet :: AssociateFleetResponse -> TestTree
responseAssociateFleet =
  res
    "AssociateFleetResponse"
    "fixture/AssociateFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateFleet)

responseBatchAssociateUserStack :: BatchAssociateUserStackResponse -> TestTree
responseBatchAssociateUserStack =
  res
    "BatchAssociateUserStackResponse"
    "fixture/BatchAssociateUserStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchAssociateUserStack)

responseBatchDisassociateUserStack :: BatchDisassociateUserStackResponse -> TestTree
responseBatchDisassociateUserStack =
  res
    "BatchDisassociateUserStackResponse"
    "fixture/BatchDisassociateUserStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy BatchDisassociateUserStack)

responseCopyImage :: CopyImageResponse -> TestTree
responseCopyImage =
  res
    "CopyImageResponse"
    "fixture/CopyImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CopyImage)

responseCreateDirectoryConfig :: CreateDirectoryConfigResponse -> TestTree
responseCreateDirectoryConfig =
  res
    "CreateDirectoryConfigResponse"
    "fixture/CreateDirectoryConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDirectoryConfig)

responseCreateFleet :: CreateFleetResponse -> TestTree
responseCreateFleet =
  res
    "CreateFleetResponse"
    "fixture/CreateFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFleet)

responseCreateImageBuilder :: CreateImageBuilderResponse -> TestTree
responseCreateImageBuilder =
  res
    "CreateImageBuilderResponse"
    "fixture/CreateImageBuilderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateImageBuilder)

responseCreateImageBuilderStreamingURL :: CreateImageBuilderStreamingURLResponse -> TestTree
responseCreateImageBuilderStreamingURL =
  res
    "CreateImageBuilderStreamingURLResponse"
    "fixture/CreateImageBuilderStreamingURLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateImageBuilderStreamingURL)

responseCreateStack :: CreateStackResponse -> TestTree
responseCreateStack =
  res
    "CreateStackResponse"
    "fixture/CreateStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStack)

responseCreateStreamingURL :: CreateStreamingURLResponse -> TestTree
responseCreateStreamingURL =
  res
    "CreateStreamingURLResponse"
    "fixture/CreateStreamingURLResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateStreamingURL)

responseCreateUpdatedImage :: CreateUpdatedImageResponse -> TestTree
responseCreateUpdatedImage =
  res
    "CreateUpdatedImageResponse"
    "fixture/CreateUpdatedImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUpdatedImage)

responseCreateUsageReportSubscription :: CreateUsageReportSubscriptionResponse -> TestTree
responseCreateUsageReportSubscription =
  res
    "CreateUsageReportSubscriptionResponse"
    "fixture/CreateUsageReportSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUsageReportSubscription)

responseCreateUser :: CreateUserResponse -> TestTree
responseCreateUser =
  res
    "CreateUserResponse"
    "fixture/CreateUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateUser)

responseDeleteDirectoryConfig :: DeleteDirectoryConfigResponse -> TestTree
responseDeleteDirectoryConfig =
  res
    "DeleteDirectoryConfigResponse"
    "fixture/DeleteDirectoryConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDirectoryConfig)

responseDeleteFleet :: DeleteFleetResponse -> TestTree
responseDeleteFleet =
  res
    "DeleteFleetResponse"
    "fixture/DeleteFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFleet)

responseDeleteImage :: DeleteImageResponse -> TestTree
responseDeleteImage =
  res
    "DeleteImageResponse"
    "fixture/DeleteImageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteImage)

responseDeleteImageBuilder :: DeleteImageBuilderResponse -> TestTree
responseDeleteImageBuilder =
  res
    "DeleteImageBuilderResponse"
    "fixture/DeleteImageBuilderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteImageBuilder)

responseDeleteImagePermissions :: DeleteImagePermissionsResponse -> TestTree
responseDeleteImagePermissions =
  res
    "DeleteImagePermissionsResponse"
    "fixture/DeleteImagePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteImagePermissions)

responseDeleteStack :: DeleteStackResponse -> TestTree
responseDeleteStack =
  res
    "DeleteStackResponse"
    "fixture/DeleteStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteStack)

responseDeleteUsageReportSubscription :: DeleteUsageReportSubscriptionResponse -> TestTree
responseDeleteUsageReportSubscription =
  res
    "DeleteUsageReportSubscriptionResponse"
    "fixture/DeleteUsageReportSubscriptionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUsageReportSubscription)

responseDeleteUser :: DeleteUserResponse -> TestTree
responseDeleteUser =
  res
    "DeleteUserResponse"
    "fixture/DeleteUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteUser)

responseDescribeDirectoryConfigs :: DescribeDirectoryConfigsResponse -> TestTree
responseDescribeDirectoryConfigs =
  res
    "DescribeDirectoryConfigsResponse"
    "fixture/DescribeDirectoryConfigsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDirectoryConfigs)

responseDescribeFleets :: DescribeFleetsResponse -> TestTree
responseDescribeFleets =
  res
    "DescribeFleetsResponse"
    "fixture/DescribeFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeFleets)

responseDescribeImageBuilders :: DescribeImageBuildersResponse -> TestTree
responseDescribeImageBuilders =
  res
    "DescribeImageBuildersResponse"
    "fixture/DescribeImageBuildersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImageBuilders)

responseDescribeImagePermissions :: DescribeImagePermissionsResponse -> TestTree
responseDescribeImagePermissions =
  res
    "DescribeImagePermissionsResponse"
    "fixture/DescribeImagePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImagePermissions)

responseDescribeImages :: DescribeImagesResponse -> TestTree
responseDescribeImages =
  res
    "DescribeImagesResponse"
    "fixture/DescribeImagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeImages)

responseDescribeSessions :: DescribeSessionsResponse -> TestTree
responseDescribeSessions =
  res
    "DescribeSessionsResponse"
    "fixture/DescribeSessionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeSessions)

responseDescribeStacks :: DescribeStacksResponse -> TestTree
responseDescribeStacks =
  res
    "DescribeStacksResponse"
    "fixture/DescribeStacksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeStacks)

responseDescribeUsageReportSubscriptions :: DescribeUsageReportSubscriptionsResponse -> TestTree
responseDescribeUsageReportSubscriptions =
  res
    "DescribeUsageReportSubscriptionsResponse"
    "fixture/DescribeUsageReportSubscriptionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUsageReportSubscriptions)

responseDescribeUserStackAssociations :: DescribeUserStackAssociationsResponse -> TestTree
responseDescribeUserStackAssociations =
  res
    "DescribeUserStackAssociationsResponse"
    "fixture/DescribeUserStackAssociationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUserStackAssociations)

responseDescribeUsers :: DescribeUsersResponse -> TestTree
responseDescribeUsers =
  res
    "DescribeUsersResponse"
    "fixture/DescribeUsersResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeUsers)

responseDisableUser :: DisableUserResponse -> TestTree
responseDisableUser =
  res
    "DisableUserResponse"
    "fixture/DisableUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisableUser)

responseDisassociateFleet :: DisassociateFleetResponse -> TestTree
responseDisassociateFleet =
  res
    "DisassociateFleetResponse"
    "fixture/DisassociateFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateFleet)

responseEnableUser :: EnableUserResponse -> TestTree
responseEnableUser =
  res
    "EnableUserResponse"
    "fixture/EnableUserResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy EnableUser)

responseExpireSession :: ExpireSessionResponse -> TestTree
responseExpireSession =
  res
    "ExpireSessionResponse"
    "fixture/ExpireSessionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExpireSession)

responseListAssociatedFleets :: ListAssociatedFleetsResponse -> TestTree
responseListAssociatedFleets =
  res
    "ListAssociatedFleetsResponse"
    "fixture/ListAssociatedFleetsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociatedFleets)

responseListAssociatedStacks :: ListAssociatedStacksResponse -> TestTree
responseListAssociatedStacks =
  res
    "ListAssociatedStacksResponse"
    "fixture/ListAssociatedStacksResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociatedStacks)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseStartFleet :: StartFleetResponse -> TestTree
responseStartFleet =
  res
    "StartFleetResponse"
    "fixture/StartFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartFleet)

responseStartImageBuilder :: StartImageBuilderResponse -> TestTree
responseStartImageBuilder =
  res
    "StartImageBuilderResponse"
    "fixture/StartImageBuilderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StartImageBuilder)

responseStopFleet :: StopFleetResponse -> TestTree
responseStopFleet =
  res
    "StopFleetResponse"
    "fixture/StopFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopFleet)

responseStopImageBuilder :: StopImageBuilderResponse -> TestTree
responseStopImageBuilder =
  res
    "StopImageBuilderResponse"
    "fixture/StopImageBuilderResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy StopImageBuilder)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateDirectoryConfig :: UpdateDirectoryConfigResponse -> TestTree
responseUpdateDirectoryConfig =
  res
    "UpdateDirectoryConfigResponse"
    "fixture/UpdateDirectoryConfigResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDirectoryConfig)

responseUpdateFleet :: UpdateFleetResponse -> TestTree
responseUpdateFleet =
  res
    "UpdateFleetResponse"
    "fixture/UpdateFleetResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFleet)

responseUpdateImagePermissions :: UpdateImagePermissionsResponse -> TestTree
responseUpdateImagePermissions =
  res
    "UpdateImagePermissionsResponse"
    "fixture/UpdateImagePermissionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateImagePermissions)

responseUpdateStack :: UpdateStackResponse -> TestTree
responseUpdateStack =
  res
    "UpdateStackResponse"
    "fixture/UpdateStackResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateStack)
