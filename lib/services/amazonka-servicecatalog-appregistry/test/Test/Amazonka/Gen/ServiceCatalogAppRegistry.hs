{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ServiceCatalogAppRegistry
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ServiceCatalogAppRegistry where

import Amazonka.ServiceCatalogAppRegistry
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Prelude
import Test.Amazonka.ServiceCatalogAppRegistry.Internal
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateAttributeGroup $
--             newAssociateAttributeGroup
--
--         , requestAssociateResource $
--             newAssociateResource
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestCreateAttributeGroup $
--             newCreateAttributeGroup
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestDeleteAttributeGroup $
--             newDeleteAttributeGroup
--
--         , requestDisassociateAttributeGroup $
--             newDisassociateAttributeGroup
--
--         , requestDisassociateResource $
--             newDisassociateResource
--
--         , requestGetApplication $
--             newGetApplication
--
--         , requestGetAssociatedResource $
--             newGetAssociatedResource
--
--         , requestGetAttributeGroup $
--             newGetAttributeGroup
--
--         , requestGetConfiguration $
--             newGetConfiguration
--
--         , requestListApplications $
--             newListApplications
--
--         , requestListAssociatedAttributeGroups $
--             newListAssociatedAttributeGroups
--
--         , requestListAssociatedResources $
--             newListAssociatedResources
--
--         , requestListAttributeGroups $
--             newListAttributeGroups
--
--         , requestListAttributeGroupsForApplication $
--             newListAttributeGroupsForApplication
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestPutConfiguration $
--             newPutConfiguration
--
--         , requestSyncResource $
--             newSyncResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--         , requestUpdateAttributeGroup $
--             newUpdateAttributeGroup
--
--           ]

--     , testGroup "response"
--         [ responseAssociateAttributeGroup $
--             newAssociateAttributeGroupResponse
--
--         , responseAssociateResource $
--             newAssociateResourceResponse
--
--         , responseCreateApplication $
--             newCreateApplicationResponse
--
--         , responseCreateAttributeGroup $
--             newCreateAttributeGroupResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseDeleteAttributeGroup $
--             newDeleteAttributeGroupResponse
--
--         , responseDisassociateAttributeGroup $
--             newDisassociateAttributeGroupResponse
--
--         , responseDisassociateResource $
--             newDisassociateResourceResponse
--
--         , responseGetApplication $
--             newGetApplicationResponse
--
--         , responseGetAssociatedResource $
--             newGetAssociatedResourceResponse
--
--         , responseGetAttributeGroup $
--             newGetAttributeGroupResponse
--
--         , responseGetConfiguration $
--             newGetConfigurationResponse
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseListAssociatedAttributeGroups $
--             newListAssociatedAttributeGroupsResponse
--
--         , responseListAssociatedResources $
--             newListAssociatedResourcesResponse
--
--         , responseListAttributeGroups $
--             newListAttributeGroupsResponse
--
--         , responseListAttributeGroupsForApplication $
--             newListAttributeGroupsForApplicationResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responsePutConfiguration $
--             newPutConfigurationResponse
--
--         , responseSyncResource $
--             newSyncResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateApplication $
--             newUpdateApplicationResponse
--
--         , responseUpdateAttributeGroup $
--             newUpdateAttributeGroupResponse
--
--           ]
--     ]

-- Requests

requestAssociateAttributeGroup :: AssociateAttributeGroup -> TestTree
requestAssociateAttributeGroup =
  req
    "AssociateAttributeGroup"
    "fixture/AssociateAttributeGroup.yaml"

requestAssociateResource :: AssociateResource -> TestTree
requestAssociateResource =
  req
    "AssociateResource"
    "fixture/AssociateResource.yaml"

requestCreateApplication :: CreateApplication -> TestTree
requestCreateApplication =
  req
    "CreateApplication"
    "fixture/CreateApplication.yaml"

requestCreateAttributeGroup :: CreateAttributeGroup -> TestTree
requestCreateAttributeGroup =
  req
    "CreateAttributeGroup"
    "fixture/CreateAttributeGroup.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestDeleteAttributeGroup :: DeleteAttributeGroup -> TestTree
requestDeleteAttributeGroup =
  req
    "DeleteAttributeGroup"
    "fixture/DeleteAttributeGroup.yaml"

requestDisassociateAttributeGroup :: DisassociateAttributeGroup -> TestTree
requestDisassociateAttributeGroup =
  req
    "DisassociateAttributeGroup"
    "fixture/DisassociateAttributeGroup.yaml"

requestDisassociateResource :: DisassociateResource -> TestTree
requestDisassociateResource =
  req
    "DisassociateResource"
    "fixture/DisassociateResource.yaml"

requestGetApplication :: GetApplication -> TestTree
requestGetApplication =
  req
    "GetApplication"
    "fixture/GetApplication.yaml"

requestGetAssociatedResource :: GetAssociatedResource -> TestTree
requestGetAssociatedResource =
  req
    "GetAssociatedResource"
    "fixture/GetAssociatedResource.yaml"

requestGetAttributeGroup :: GetAttributeGroup -> TestTree
requestGetAttributeGroup =
  req
    "GetAttributeGroup"
    "fixture/GetAttributeGroup.yaml"

requestGetConfiguration :: GetConfiguration -> TestTree
requestGetConfiguration =
  req
    "GetConfiguration"
    "fixture/GetConfiguration.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

requestListAssociatedAttributeGroups :: ListAssociatedAttributeGroups -> TestTree
requestListAssociatedAttributeGroups =
  req
    "ListAssociatedAttributeGroups"
    "fixture/ListAssociatedAttributeGroups.yaml"

requestListAssociatedResources :: ListAssociatedResources -> TestTree
requestListAssociatedResources =
  req
    "ListAssociatedResources"
    "fixture/ListAssociatedResources.yaml"

requestListAttributeGroups :: ListAttributeGroups -> TestTree
requestListAttributeGroups =
  req
    "ListAttributeGroups"
    "fixture/ListAttributeGroups.yaml"

requestListAttributeGroupsForApplication :: ListAttributeGroupsForApplication -> TestTree
requestListAttributeGroupsForApplication =
  req
    "ListAttributeGroupsForApplication"
    "fixture/ListAttributeGroupsForApplication.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestPutConfiguration :: PutConfiguration -> TestTree
requestPutConfiguration =
  req
    "PutConfiguration"
    "fixture/PutConfiguration.yaml"

requestSyncResource :: SyncResource -> TestTree
requestSyncResource =
  req
    "SyncResource"
    "fixture/SyncResource.yaml"

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

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

requestUpdateAttributeGroup :: UpdateAttributeGroup -> TestTree
requestUpdateAttributeGroup =
  req
    "UpdateAttributeGroup"
    "fixture/UpdateAttributeGroup.yaml"

-- Responses

responseAssociateAttributeGroup :: AssociateAttributeGroupResponse -> TestTree
responseAssociateAttributeGroup =
  res
    "AssociateAttributeGroupResponse"
    "fixture/AssociateAttributeGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateAttributeGroup)

responseAssociateResource :: AssociateResourceResponse -> TestTree
responseAssociateResource =
  res
    "AssociateResourceResponse"
    "fixture/AssociateResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateResource)

responseCreateApplication :: CreateApplicationResponse -> TestTree
responseCreateApplication =
  res
    "CreateApplicationResponse"
    "fixture/CreateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplication)

responseCreateAttributeGroup :: CreateAttributeGroupResponse -> TestTree
responseCreateAttributeGroup =
  res
    "CreateAttributeGroupResponse"
    "fixture/CreateAttributeGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAttributeGroup)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplication)

responseDeleteAttributeGroup :: DeleteAttributeGroupResponse -> TestTree
responseDeleteAttributeGroup =
  res
    "DeleteAttributeGroupResponse"
    "fixture/DeleteAttributeGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAttributeGroup)

responseDisassociateAttributeGroup :: DisassociateAttributeGroupResponse -> TestTree
responseDisassociateAttributeGroup =
  res
    "DisassociateAttributeGroupResponse"
    "fixture/DisassociateAttributeGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateAttributeGroup)

responseDisassociateResource :: DisassociateResourceResponse -> TestTree
responseDisassociateResource =
  res
    "DisassociateResourceResponse"
    "fixture/DisassociateResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateResource)

responseGetApplication :: GetApplicationResponse -> TestTree
responseGetApplication =
  res
    "GetApplicationResponse"
    "fixture/GetApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetApplication)

responseGetAssociatedResource :: GetAssociatedResourceResponse -> TestTree
responseGetAssociatedResource =
  res
    "GetAssociatedResourceResponse"
    "fixture/GetAssociatedResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAssociatedResource)

responseGetAttributeGroup :: GetAttributeGroupResponse -> TestTree
responseGetAttributeGroup =
  res
    "GetAttributeGroupResponse"
    "fixture/GetAttributeGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAttributeGroup)

responseGetConfiguration :: GetConfigurationResponse -> TestTree
responseGetConfiguration =
  res
    "GetConfigurationResponse"
    "fixture/GetConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetConfiguration)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplications)

responseListAssociatedAttributeGroups :: ListAssociatedAttributeGroupsResponse -> TestTree
responseListAssociatedAttributeGroups =
  res
    "ListAssociatedAttributeGroupsResponse"
    "fixture/ListAssociatedAttributeGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociatedAttributeGroups)

responseListAssociatedResources :: ListAssociatedResourcesResponse -> TestTree
responseListAssociatedResources =
  res
    "ListAssociatedResourcesResponse"
    "fixture/ListAssociatedResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociatedResources)

responseListAttributeGroups :: ListAttributeGroupsResponse -> TestTree
responseListAttributeGroups =
  res
    "ListAttributeGroupsResponse"
    "fixture/ListAttributeGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttributeGroups)

responseListAttributeGroupsForApplication :: ListAttributeGroupsForApplicationResponse -> TestTree
responseListAttributeGroupsForApplication =
  res
    "ListAttributeGroupsForApplicationResponse"
    "fixture/ListAttributeGroupsForApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttributeGroupsForApplication)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responsePutConfiguration :: PutConfigurationResponse -> TestTree
responsePutConfiguration =
  res
    "PutConfigurationResponse"
    "fixture/PutConfigurationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutConfiguration)

responseSyncResource :: SyncResourceResponse -> TestTree
responseSyncResource =
  res
    "SyncResourceResponse"
    "fixture/SyncResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SyncResource)

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

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplication)

responseUpdateAttributeGroup :: UpdateAttributeGroupResponse -> TestTree
responseUpdateAttributeGroup =
  res
    "UpdateAttributeGroupResponse"
    "fixture/UpdateAttributeGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAttributeGroup)
