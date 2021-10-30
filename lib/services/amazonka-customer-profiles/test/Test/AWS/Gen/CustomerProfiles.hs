{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.CustomerProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.CustomerProfiles where

import qualified Data.Proxy as Proxy
import Network.AWS.CustomerProfiles
import Test.AWS.CustomerProfiles.Internal
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
--         [ requestDeleteProfileObjectType $
--             newDeleteProfileObjectType
--
--         , requestListIntegrations $
--             newListIntegrations
--
--         , requestPutProfileObjectType $
--             newPutProfileObjectType
--
--         , requestListProfileObjects $
--             newListProfileObjects
--
--         , requestListProfileObjectTypeTemplates $
--             newListProfileObjectTypeTemplates
--
--         , requestDeleteProfile $
--             newDeleteProfile
--
--         , requestUpdateProfile $
--             newUpdateProfile
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestAddProfileKey $
--             newAddProfileKey
--
--         , requestGetProfileObjectTypeTemplate $
--             newGetProfileObjectTypeTemplate
--
--         , requestGetIntegration $
--             newGetIntegration
--
--         , requestGetDomain $
--             newGetDomain
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestDeleteIntegration $
--             newDeleteIntegration
--
--         , requestCreateProfile $
--             newCreateProfile
--
--         , requestPutProfileObject $
--             newPutProfileObject
--
--         , requestPutIntegration $
--             newPutIntegration
--
--         , requestDeleteProfileObject $
--             newDeleteProfileObject
--
--         , requestListProfileObjectTypes $
--             newListProfileObjectTypes
--
--         , requestDeleteProfileKey $
--             newDeleteProfileKey
--
--         , requestGetProfileObjectType $
--             newGetProfileObjectType
--
--         , requestMergeProfiles $
--             newMergeProfiles
--
--         , requestTagResource $
--             newTagResource
--
--         , requestGetMatches $
--             newGetMatches
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestSearchProfiles $
--             newSearchProfiles
--
--         , requestListAccountIntegrations $
--             newListAccountIntegrations
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestUpdateDomain $
--             newUpdateDomain
--
--         , requestListDomains $
--             newListDomains
--
--           ]

--     , testGroup "response"
--         [ responseDeleteProfileObjectType $
--             newDeleteProfileObjectTypeResponse
--
--         , responseListIntegrations $
--             newListIntegrationsResponse
--
--         , responsePutProfileObjectType $
--             newPutProfileObjectTypeResponse
--
--         , responseListProfileObjects $
--             newListProfileObjectsResponse
--
--         , responseListProfileObjectTypeTemplates $
--             newListProfileObjectTypeTemplatesResponse
--
--         , responseDeleteProfile $
--             newDeleteProfileResponse
--
--         , responseUpdateProfile $
--             newUpdateProfileResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseAddProfileKey $
--             newAddProfileKeyResponse
--
--         , responseGetProfileObjectTypeTemplate $
--             newGetProfileObjectTypeTemplateResponse
--
--         , responseGetIntegration $
--             newGetIntegrationResponse
--
--         , responseGetDomain $
--             newGetDomainResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseDeleteIntegration $
--             newDeleteIntegrationResponse
--
--         , responseCreateProfile $
--             newCreateProfileResponse
--
--         , responsePutProfileObject $
--             newPutProfileObjectResponse
--
--         , responsePutIntegration $
--             newPutIntegrationResponse
--
--         , responseDeleteProfileObject $
--             newDeleteProfileObjectResponse
--
--         , responseListProfileObjectTypes $
--             newListProfileObjectTypesResponse
--
--         , responseDeleteProfileKey $
--             newDeleteProfileKeyResponse
--
--         , responseGetProfileObjectType $
--             newGetProfileObjectTypeResponse
--
--         , responseMergeProfiles $
--             newMergeProfilesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseGetMatches $
--             newGetMatchesResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseSearchProfiles $
--             newSearchProfilesResponse
--
--         , responseListAccountIntegrations $
--             newListAccountIntegrationsResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseUpdateDomain $
--             newUpdateDomainResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--           ]
--     ]

-- Requests

requestDeleteProfileObjectType :: DeleteProfileObjectType -> TestTree
requestDeleteProfileObjectType =
  req
    "DeleteProfileObjectType"
    "fixture/DeleteProfileObjectType.yaml"

requestListIntegrations :: ListIntegrations -> TestTree
requestListIntegrations =
  req
    "ListIntegrations"
    "fixture/ListIntegrations.yaml"

requestPutProfileObjectType :: PutProfileObjectType -> TestTree
requestPutProfileObjectType =
  req
    "PutProfileObjectType"
    "fixture/PutProfileObjectType.yaml"

requestListProfileObjects :: ListProfileObjects -> TestTree
requestListProfileObjects =
  req
    "ListProfileObjects"
    "fixture/ListProfileObjects.yaml"

requestListProfileObjectTypeTemplates :: ListProfileObjectTypeTemplates -> TestTree
requestListProfileObjectTypeTemplates =
  req
    "ListProfileObjectTypeTemplates"
    "fixture/ListProfileObjectTypeTemplates.yaml"

requestDeleteProfile :: DeleteProfile -> TestTree
requestDeleteProfile =
  req
    "DeleteProfile"
    "fixture/DeleteProfile.yaml"

requestUpdateProfile :: UpdateProfile -> TestTree
requestUpdateProfile =
  req
    "UpdateProfile"
    "fixture/UpdateProfile.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestAddProfileKey :: AddProfileKey -> TestTree
requestAddProfileKey =
  req
    "AddProfileKey"
    "fixture/AddProfileKey.yaml"

requestGetProfileObjectTypeTemplate :: GetProfileObjectTypeTemplate -> TestTree
requestGetProfileObjectTypeTemplate =
  req
    "GetProfileObjectTypeTemplate"
    "fixture/GetProfileObjectTypeTemplate.yaml"

requestGetIntegration :: GetIntegration -> TestTree
requestGetIntegration =
  req
    "GetIntegration"
    "fixture/GetIntegration.yaml"

requestGetDomain :: GetDomain -> TestTree
requestGetDomain =
  req
    "GetDomain"
    "fixture/GetDomain.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestDeleteIntegration :: DeleteIntegration -> TestTree
requestDeleteIntegration =
  req
    "DeleteIntegration"
    "fixture/DeleteIntegration.yaml"

requestCreateProfile :: CreateProfile -> TestTree
requestCreateProfile =
  req
    "CreateProfile"
    "fixture/CreateProfile.yaml"

requestPutProfileObject :: PutProfileObject -> TestTree
requestPutProfileObject =
  req
    "PutProfileObject"
    "fixture/PutProfileObject.yaml"

requestPutIntegration :: PutIntegration -> TestTree
requestPutIntegration =
  req
    "PutIntegration"
    "fixture/PutIntegration.yaml"

requestDeleteProfileObject :: DeleteProfileObject -> TestTree
requestDeleteProfileObject =
  req
    "DeleteProfileObject"
    "fixture/DeleteProfileObject.yaml"

requestListProfileObjectTypes :: ListProfileObjectTypes -> TestTree
requestListProfileObjectTypes =
  req
    "ListProfileObjectTypes"
    "fixture/ListProfileObjectTypes.yaml"

requestDeleteProfileKey :: DeleteProfileKey -> TestTree
requestDeleteProfileKey =
  req
    "DeleteProfileKey"
    "fixture/DeleteProfileKey.yaml"

requestGetProfileObjectType :: GetProfileObjectType -> TestTree
requestGetProfileObjectType =
  req
    "GetProfileObjectType"
    "fixture/GetProfileObjectType.yaml"

requestMergeProfiles :: MergeProfiles -> TestTree
requestMergeProfiles =
  req
    "MergeProfiles"
    "fixture/MergeProfiles.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestGetMatches :: GetMatches -> TestTree
requestGetMatches =
  req
    "GetMatches"
    "fixture/GetMatches.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestSearchProfiles :: SearchProfiles -> TestTree
requestSearchProfiles =
  req
    "SearchProfiles"
    "fixture/SearchProfiles.yaml"

requestListAccountIntegrations :: ListAccountIntegrations -> TestTree
requestListAccountIntegrations =
  req
    "ListAccountIntegrations"
    "fixture/ListAccountIntegrations.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestUpdateDomain :: UpdateDomain -> TestTree
requestUpdateDomain =
  req
    "UpdateDomain"
    "fixture/UpdateDomain.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

-- Responses

responseDeleteProfileObjectType :: DeleteProfileObjectTypeResponse -> TestTree
responseDeleteProfileObjectType =
  res
    "DeleteProfileObjectTypeResponse"
    "fixture/DeleteProfileObjectTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProfileObjectType)

responseListIntegrations :: ListIntegrationsResponse -> TestTree
responseListIntegrations =
  res
    "ListIntegrationsResponse"
    "fixture/ListIntegrationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIntegrations)

responsePutProfileObjectType :: PutProfileObjectTypeResponse -> TestTree
responsePutProfileObjectType =
  res
    "PutProfileObjectTypeResponse"
    "fixture/PutProfileObjectTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutProfileObjectType)

responseListProfileObjects :: ListProfileObjectsResponse -> TestTree
responseListProfileObjects =
  res
    "ListProfileObjectsResponse"
    "fixture/ListProfileObjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProfileObjects)

responseListProfileObjectTypeTemplates :: ListProfileObjectTypeTemplatesResponse -> TestTree
responseListProfileObjectTypeTemplates =
  res
    "ListProfileObjectTypeTemplatesResponse"
    "fixture/ListProfileObjectTypeTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProfileObjectTypeTemplates)

responseDeleteProfile :: DeleteProfileResponse -> TestTree
responseDeleteProfile =
  res
    "DeleteProfileResponse"
    "fixture/DeleteProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProfile)

responseUpdateProfile :: UpdateProfileResponse -> TestTree
responseUpdateProfile =
  res
    "UpdateProfileResponse"
    "fixture/UpdateProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProfile)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseAddProfileKey :: AddProfileKeyResponse -> TestTree
responseAddProfileKey =
  res
    "AddProfileKeyResponse"
    "fixture/AddProfileKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddProfileKey)

responseGetProfileObjectTypeTemplate :: GetProfileObjectTypeTemplateResponse -> TestTree
responseGetProfileObjectTypeTemplate =
  res
    "GetProfileObjectTypeTemplateResponse"
    "fixture/GetProfileObjectTypeTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProfileObjectTypeTemplate)

responseGetIntegration :: GetIntegrationResponse -> TestTree
responseGetIntegration =
  res
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntegration)

responseGetDomain :: GetDomainResponse -> TestTree
responseGetDomain =
  res
    "GetDomainResponse"
    "fixture/GetDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomain)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomain)

responseDeleteIntegration :: DeleteIntegrationResponse -> TestTree
responseDeleteIntegration =
  res
    "DeleteIntegrationResponse"
    "fixture/DeleteIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIntegration)

responseCreateProfile :: CreateProfileResponse -> TestTree
responseCreateProfile =
  res
    "CreateProfileResponse"
    "fixture/CreateProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProfile)

responsePutProfileObject :: PutProfileObjectResponse -> TestTree
responsePutProfileObject =
  res
    "PutProfileObjectResponse"
    "fixture/PutProfileObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutProfileObject)

responsePutIntegration :: PutIntegrationResponse -> TestTree
responsePutIntegration =
  res
    "PutIntegrationResponse"
    "fixture/PutIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutIntegration)

responseDeleteProfileObject :: DeleteProfileObjectResponse -> TestTree
responseDeleteProfileObject =
  res
    "DeleteProfileObjectResponse"
    "fixture/DeleteProfileObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProfileObject)

responseListProfileObjectTypes :: ListProfileObjectTypesResponse -> TestTree
responseListProfileObjectTypes =
  res
    "ListProfileObjectTypesResponse"
    "fixture/ListProfileObjectTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProfileObjectTypes)

responseDeleteProfileKey :: DeleteProfileKeyResponse -> TestTree
responseDeleteProfileKey =
  res
    "DeleteProfileKeyResponse"
    "fixture/DeleteProfileKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProfileKey)

responseGetProfileObjectType :: GetProfileObjectTypeResponse -> TestTree
responseGetProfileObjectType =
  res
    "GetProfileObjectTypeResponse"
    "fixture/GetProfileObjectTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProfileObjectType)

responseMergeProfiles :: MergeProfilesResponse -> TestTree
responseMergeProfiles =
  res
    "MergeProfilesResponse"
    "fixture/MergeProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MergeProfiles)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseGetMatches :: GetMatchesResponse -> TestTree
responseGetMatches =
  res
    "GetMatchesResponse"
    "fixture/GetMatchesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMatches)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseSearchProfiles :: SearchProfilesResponse -> TestTree
responseSearchProfiles =
  res
    "SearchProfilesResponse"
    "fixture/SearchProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchProfiles)

responseListAccountIntegrations :: ListAccountIntegrationsResponse -> TestTree
responseListAccountIntegrations =
  res
    "ListAccountIntegrationsResponse"
    "fixture/ListAccountIntegrationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountIntegrations)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomain)

responseUpdateDomain :: UpdateDomainResponse -> TestTree
responseUpdateDomain =
  res
    "UpdateDomainResponse"
    "fixture/UpdateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomain)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomains)
