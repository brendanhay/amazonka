{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ServiceCatalogAppRegistry
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--         , requestListAttributeGroups $
--             newListAttributeGroups
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestSyncResource $
--             newSyncResource
--
--         , requestDeleteApplication $
--             newDeleteApplication
--
--         , requestUpdateApplication $
--             newUpdateApplication
--
--         , requestAssociateResource $
--             newAssociateResource
--
--         , requestCreateApplication $
--             newCreateApplication
--
--         , requestDisassociateAttributeGroup $
--             newDisassociateAttributeGroup
--
--         , requestGetApplication $
--             newGetApplication
--
--         , requestGetAssociatedResource $
--             newGetAssociatedResource
--
--         , requestCreateAttributeGroup $
--             newCreateAttributeGroup
--
--         , requestDeleteAttributeGroup $
--             newDeleteAttributeGroup
--
--         , requestUpdateAttributeGroup $
--             newUpdateAttributeGroup
--
--         , requestListAssociatedAttributeGroups $
--             newListAssociatedAttributeGroups
--
--         , requestGetAttributeGroup $
--             newGetAttributeGroup
--
--         , requestDisassociateResource $
--             newDisassociateResource
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListAssociatedResources $
--             newListAssociatedResources
--
--         , requestListApplications $
--             newListApplications
--
--         , requestUntagResource $
--             newUntagResource
--
--           ]

--     , testGroup "response"
--         [ responseAssociateAttributeGroup $
--             newAssociateAttributeGroupResponse
--
--         , responseListAttributeGroups $
--             newListAttributeGroupsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseSyncResource $
--             newSyncResourceResponse
--
--         , responseDeleteApplication $
--             newDeleteApplicationResponse
--
--         , responseUpdateApplication $
--             newUpdateApplicationResponse
--
--         , responseAssociateResource $
--             newAssociateResourceResponse
--
--         , responseCreateApplication $
--             newCreateApplicationResponse
--
--         , responseDisassociateAttributeGroup $
--             newDisassociateAttributeGroupResponse
--
--         , responseGetApplication $
--             newGetApplicationResponse
--
--         , responseGetAssociatedResource $
--             newGetAssociatedResourceResponse
--
--         , responseCreateAttributeGroup $
--             newCreateAttributeGroupResponse
--
--         , responseDeleteAttributeGroup $
--             newDeleteAttributeGroupResponse
--
--         , responseUpdateAttributeGroup $
--             newUpdateAttributeGroupResponse
--
--         , responseListAssociatedAttributeGroups $
--             newListAssociatedAttributeGroupsResponse
--
--         , responseGetAttributeGroup $
--             newGetAttributeGroupResponse
--
--         , responseDisassociateResource $
--             newDisassociateResourceResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListAssociatedResources $
--             newListAssociatedResourcesResponse
--
--         , responseListApplications $
--             newListApplicationsResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--           ]
--     ]

-- Requests

requestAssociateAttributeGroup :: AssociateAttributeGroup -> TestTree
requestAssociateAttributeGroup =
  req
    "AssociateAttributeGroup"
    "fixture/AssociateAttributeGroup.yaml"

requestListAttributeGroups :: ListAttributeGroups -> TestTree
requestListAttributeGroups =
  req
    "ListAttributeGroups"
    "fixture/ListAttributeGroups.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestSyncResource :: SyncResource -> TestTree
requestSyncResource =
  req
    "SyncResource"
    "fixture/SyncResource.yaml"

requestDeleteApplication :: DeleteApplication -> TestTree
requestDeleteApplication =
  req
    "DeleteApplication"
    "fixture/DeleteApplication.yaml"

requestUpdateApplication :: UpdateApplication -> TestTree
requestUpdateApplication =
  req
    "UpdateApplication"
    "fixture/UpdateApplication.yaml"

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

requestDisassociateAttributeGroup :: DisassociateAttributeGroup -> TestTree
requestDisassociateAttributeGroup =
  req
    "DisassociateAttributeGroup"
    "fixture/DisassociateAttributeGroup.yaml"

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

requestCreateAttributeGroup :: CreateAttributeGroup -> TestTree
requestCreateAttributeGroup =
  req
    "CreateAttributeGroup"
    "fixture/CreateAttributeGroup.yaml"

requestDeleteAttributeGroup :: DeleteAttributeGroup -> TestTree
requestDeleteAttributeGroup =
  req
    "DeleteAttributeGroup"
    "fixture/DeleteAttributeGroup.yaml"

requestUpdateAttributeGroup :: UpdateAttributeGroup -> TestTree
requestUpdateAttributeGroup =
  req
    "UpdateAttributeGroup"
    "fixture/UpdateAttributeGroup.yaml"

requestListAssociatedAttributeGroups :: ListAssociatedAttributeGroups -> TestTree
requestListAssociatedAttributeGroups =
  req
    "ListAssociatedAttributeGroups"
    "fixture/ListAssociatedAttributeGroups.yaml"

requestGetAttributeGroup :: GetAttributeGroup -> TestTree
requestGetAttributeGroup =
  req
    "GetAttributeGroup"
    "fixture/GetAttributeGroup.yaml"

requestDisassociateResource :: DisassociateResource -> TestTree
requestDisassociateResource =
  req
    "DisassociateResource"
    "fixture/DisassociateResource.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListAssociatedResources :: ListAssociatedResources -> TestTree
requestListAssociatedResources =
  req
    "ListAssociatedResources"
    "fixture/ListAssociatedResources.yaml"

requestListApplications :: ListApplications -> TestTree
requestListApplications =
  req
    "ListApplications"
    "fixture/ListApplications.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

-- Responses

responseAssociateAttributeGroup :: AssociateAttributeGroupResponse -> TestTree
responseAssociateAttributeGroup =
  res
    "AssociateAttributeGroupResponse"
    "fixture/AssociateAttributeGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateAttributeGroup)

responseListAttributeGroups :: ListAttributeGroupsResponse -> TestTree
responseListAttributeGroups =
  res
    "ListAttributeGroupsResponse"
    "fixture/ListAttributeGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAttributeGroups)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseSyncResource :: SyncResourceResponse -> TestTree
responseSyncResource =
  res
    "SyncResourceResponse"
    "fixture/SyncResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SyncResource)

responseDeleteApplication :: DeleteApplicationResponse -> TestTree
responseDeleteApplication =
  res
    "DeleteApplicationResponse"
    "fixture/DeleteApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteApplication)

responseUpdateApplication :: UpdateApplicationResponse -> TestTree
responseUpdateApplication =
  res
    "UpdateApplicationResponse"
    "fixture/UpdateApplicationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateApplication)

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

responseDisassociateAttributeGroup :: DisassociateAttributeGroupResponse -> TestTree
responseDisassociateAttributeGroup =
  res
    "DisassociateAttributeGroupResponse"
    "fixture/DisassociateAttributeGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateAttributeGroup)

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

responseCreateAttributeGroup :: CreateAttributeGroupResponse -> TestTree
responseCreateAttributeGroup =
  res
    "CreateAttributeGroupResponse"
    "fixture/CreateAttributeGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateAttributeGroup)

responseDeleteAttributeGroup :: DeleteAttributeGroupResponse -> TestTree
responseDeleteAttributeGroup =
  res
    "DeleteAttributeGroupResponse"
    "fixture/DeleteAttributeGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteAttributeGroup)

responseUpdateAttributeGroup :: UpdateAttributeGroupResponse -> TestTree
responseUpdateAttributeGroup =
  res
    "UpdateAttributeGroupResponse"
    "fixture/UpdateAttributeGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateAttributeGroup)

responseListAssociatedAttributeGroups :: ListAssociatedAttributeGroupsResponse -> TestTree
responseListAssociatedAttributeGroups =
  res
    "ListAssociatedAttributeGroupsResponse"
    "fixture/ListAssociatedAttributeGroupsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociatedAttributeGroups)

responseGetAttributeGroup :: GetAttributeGroupResponse -> TestTree
responseGetAttributeGroup =
  res
    "GetAttributeGroupResponse"
    "fixture/GetAttributeGroupResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAttributeGroup)

responseDisassociateResource :: DisassociateResourceResponse -> TestTree
responseDisassociateResource =
  res
    "DisassociateResourceResponse"
    "fixture/DisassociateResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DisassociateResource)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseListAssociatedResources :: ListAssociatedResourcesResponse -> TestTree
responseListAssociatedResources =
  res
    "ListAssociatedResourcesResponse"
    "fixture/ListAssociatedResourcesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAssociatedResources)

responseListApplications :: ListApplicationsResponse -> TestTree
responseListApplications =
  res
    "ListApplicationsResponse"
    "fixture/ListApplicationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplications)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)
