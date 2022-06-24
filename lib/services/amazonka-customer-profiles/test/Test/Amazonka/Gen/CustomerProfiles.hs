{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CustomerProfiles
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.CustomerProfiles where

import Amazonka.CustomerProfiles
import qualified Data.Proxy as Proxy
import Test.Amazonka.CustomerProfiles.Internal
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
--         [ requestAddProfileKey $
--             newAddProfileKey
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestCreateProfile $
--             newCreateProfile
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestDeleteIntegration $
--             newDeleteIntegration
--
--         , requestDeleteProfile $
--             newDeleteProfile
--
--         , requestDeleteProfileKey $
--             newDeleteProfileKey
--
--         , requestDeleteProfileObject $
--             newDeleteProfileObject
--
--         , requestDeleteProfileObjectType $
--             newDeleteProfileObjectType
--
--         , requestGetDomain $
--             newGetDomain
--
--         , requestGetIntegration $
--             newGetIntegration
--
--         , requestGetMatches $
--             newGetMatches
--
--         , requestGetProfileObjectType $
--             newGetProfileObjectType
--
--         , requestGetProfileObjectTypeTemplate $
--             newGetProfileObjectTypeTemplate
--
--         , requestListAccountIntegrations $
--             newListAccountIntegrations
--
--         , requestListDomains $
--             newListDomains
--
--         , requestListIntegrations $
--             newListIntegrations
--
--         , requestListProfileObjectTypeTemplates $
--             newListProfileObjectTypeTemplates
--
--         , requestListProfileObjectTypes $
--             newListProfileObjectTypes
--
--         , requestListProfileObjects $
--             newListProfileObjects
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestMergeProfiles $
--             newMergeProfiles
--
--         , requestPutIntegration $
--             newPutIntegration
--
--         , requestPutProfileObject $
--             newPutProfileObject
--
--         , requestPutProfileObjectType $
--             newPutProfileObjectType
--
--         , requestSearchProfiles $
--             newSearchProfiles
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDomain $
--             newUpdateDomain
--
--         , requestUpdateProfile $
--             newUpdateProfile
--
--           ]

--     , testGroup "response"
--         [ responseAddProfileKey $
--             newAddProfileKeyResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseCreateProfile $
--             newCreateProfileResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseDeleteIntegration $
--             newDeleteIntegrationResponse
--
--         , responseDeleteProfile $
--             newDeleteProfileResponse
--
--         , responseDeleteProfileKey $
--             newDeleteProfileKeyResponse
--
--         , responseDeleteProfileObject $
--             newDeleteProfileObjectResponse
--
--         , responseDeleteProfileObjectType $
--             newDeleteProfileObjectTypeResponse
--
--         , responseGetDomain $
--             newGetDomainResponse
--
--         , responseGetIntegration $
--             newGetIntegrationResponse
--
--         , responseGetMatches $
--             newGetMatchesResponse
--
--         , responseGetProfileObjectType $
--             newGetProfileObjectTypeResponse
--
--         , responseGetProfileObjectTypeTemplate $
--             newGetProfileObjectTypeTemplateResponse
--
--         , responseListAccountIntegrations $
--             newListAccountIntegrationsResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseListIntegrations $
--             newListIntegrationsResponse
--
--         , responseListProfileObjectTypeTemplates $
--             newListProfileObjectTypeTemplatesResponse
--
--         , responseListProfileObjectTypes $
--             newListProfileObjectTypesResponse
--
--         , responseListProfileObjects $
--             newListProfileObjectsResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseMergeProfiles $
--             newMergeProfilesResponse
--
--         , responsePutIntegration $
--             newPutIntegrationResponse
--
--         , responsePutProfileObject $
--             newPutProfileObjectResponse
--
--         , responsePutProfileObjectType $
--             newPutProfileObjectTypeResponse
--
--         , responseSearchProfiles $
--             newSearchProfilesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDomain $
--             newUpdateDomainResponse
--
--         , responseUpdateProfile $
--             newUpdateProfileResponse
--
--           ]
--     ]

-- Requests

requestAddProfileKey :: AddProfileKey -> TestTree
requestAddProfileKey =
  req
    "AddProfileKey"
    "fixture/AddProfileKey.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestCreateProfile :: CreateProfile -> TestTree
requestCreateProfile =
  req
    "CreateProfile"
    "fixture/CreateProfile.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestDeleteIntegration :: DeleteIntegration -> TestTree
requestDeleteIntegration =
  req
    "DeleteIntegration"
    "fixture/DeleteIntegration.yaml"

requestDeleteProfile :: DeleteProfile -> TestTree
requestDeleteProfile =
  req
    "DeleteProfile"
    "fixture/DeleteProfile.yaml"

requestDeleteProfileKey :: DeleteProfileKey -> TestTree
requestDeleteProfileKey =
  req
    "DeleteProfileKey"
    "fixture/DeleteProfileKey.yaml"

requestDeleteProfileObject :: DeleteProfileObject -> TestTree
requestDeleteProfileObject =
  req
    "DeleteProfileObject"
    "fixture/DeleteProfileObject.yaml"

requestDeleteProfileObjectType :: DeleteProfileObjectType -> TestTree
requestDeleteProfileObjectType =
  req
    "DeleteProfileObjectType"
    "fixture/DeleteProfileObjectType.yaml"

requestGetDomain :: GetDomain -> TestTree
requestGetDomain =
  req
    "GetDomain"
    "fixture/GetDomain.yaml"

requestGetIntegration :: GetIntegration -> TestTree
requestGetIntegration =
  req
    "GetIntegration"
    "fixture/GetIntegration.yaml"

requestGetMatches :: GetMatches -> TestTree
requestGetMatches =
  req
    "GetMatches"
    "fixture/GetMatches.yaml"

requestGetProfileObjectType :: GetProfileObjectType -> TestTree
requestGetProfileObjectType =
  req
    "GetProfileObjectType"
    "fixture/GetProfileObjectType.yaml"

requestGetProfileObjectTypeTemplate :: GetProfileObjectTypeTemplate -> TestTree
requestGetProfileObjectTypeTemplate =
  req
    "GetProfileObjectTypeTemplate"
    "fixture/GetProfileObjectTypeTemplate.yaml"

requestListAccountIntegrations :: ListAccountIntegrations -> TestTree
requestListAccountIntegrations =
  req
    "ListAccountIntegrations"
    "fixture/ListAccountIntegrations.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestListIntegrations :: ListIntegrations -> TestTree
requestListIntegrations =
  req
    "ListIntegrations"
    "fixture/ListIntegrations.yaml"

requestListProfileObjectTypeTemplates :: ListProfileObjectTypeTemplates -> TestTree
requestListProfileObjectTypeTemplates =
  req
    "ListProfileObjectTypeTemplates"
    "fixture/ListProfileObjectTypeTemplates.yaml"

requestListProfileObjectTypes :: ListProfileObjectTypes -> TestTree
requestListProfileObjectTypes =
  req
    "ListProfileObjectTypes"
    "fixture/ListProfileObjectTypes.yaml"

requestListProfileObjects :: ListProfileObjects -> TestTree
requestListProfileObjects =
  req
    "ListProfileObjects"
    "fixture/ListProfileObjects.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestMergeProfiles :: MergeProfiles -> TestTree
requestMergeProfiles =
  req
    "MergeProfiles"
    "fixture/MergeProfiles.yaml"

requestPutIntegration :: PutIntegration -> TestTree
requestPutIntegration =
  req
    "PutIntegration"
    "fixture/PutIntegration.yaml"

requestPutProfileObject :: PutProfileObject -> TestTree
requestPutProfileObject =
  req
    "PutProfileObject"
    "fixture/PutProfileObject.yaml"

requestPutProfileObjectType :: PutProfileObjectType -> TestTree
requestPutProfileObjectType =
  req
    "PutProfileObjectType"
    "fixture/PutProfileObjectType.yaml"

requestSearchProfiles :: SearchProfiles -> TestTree
requestSearchProfiles =
  req
    "SearchProfiles"
    "fixture/SearchProfiles.yaml"

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

requestUpdateDomain :: UpdateDomain -> TestTree
requestUpdateDomain =
  req
    "UpdateDomain"
    "fixture/UpdateDomain.yaml"

requestUpdateProfile :: UpdateProfile -> TestTree
requestUpdateProfile =
  req
    "UpdateProfile"
    "fixture/UpdateProfile.yaml"

-- Responses

responseAddProfileKey :: AddProfileKeyResponse -> TestTree
responseAddProfileKey =
  res
    "AddProfileKeyResponse"
    "fixture/AddProfileKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AddProfileKey)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomain)

responseCreateProfile :: CreateProfileResponse -> TestTree
responseCreateProfile =
  res
    "CreateProfileResponse"
    "fixture/CreateProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProfile)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomain)

responseDeleteIntegration :: DeleteIntegrationResponse -> TestTree
responseDeleteIntegration =
  res
    "DeleteIntegrationResponse"
    "fixture/DeleteIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteIntegration)

responseDeleteProfile :: DeleteProfileResponse -> TestTree
responseDeleteProfile =
  res
    "DeleteProfileResponse"
    "fixture/DeleteProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProfile)

responseDeleteProfileKey :: DeleteProfileKeyResponse -> TestTree
responseDeleteProfileKey =
  res
    "DeleteProfileKeyResponse"
    "fixture/DeleteProfileKeyResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProfileKey)

responseDeleteProfileObject :: DeleteProfileObjectResponse -> TestTree
responseDeleteProfileObject =
  res
    "DeleteProfileObjectResponse"
    "fixture/DeleteProfileObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProfileObject)

responseDeleteProfileObjectType :: DeleteProfileObjectTypeResponse -> TestTree
responseDeleteProfileObjectType =
  res
    "DeleteProfileObjectTypeResponse"
    "fixture/DeleteProfileObjectTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteProfileObjectType)

responseGetDomain :: GetDomainResponse -> TestTree
responseGetDomain =
  res
    "GetDomainResponse"
    "fixture/GetDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomain)

responseGetIntegration :: GetIntegrationResponse -> TestTree
responseGetIntegration =
  res
    "GetIntegrationResponse"
    "fixture/GetIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIntegration)

responseGetMatches :: GetMatchesResponse -> TestTree
responseGetMatches =
  res
    "GetMatchesResponse"
    "fixture/GetMatchesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetMatches)

responseGetProfileObjectType :: GetProfileObjectTypeResponse -> TestTree
responseGetProfileObjectType =
  res
    "GetProfileObjectTypeResponse"
    "fixture/GetProfileObjectTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProfileObjectType)

responseGetProfileObjectTypeTemplate :: GetProfileObjectTypeTemplateResponse -> TestTree
responseGetProfileObjectTypeTemplate =
  res
    "GetProfileObjectTypeTemplateResponse"
    "fixture/GetProfileObjectTypeTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetProfileObjectTypeTemplate)

responseListAccountIntegrations :: ListAccountIntegrationsResponse -> TestTree
responseListAccountIntegrations =
  res
    "ListAccountIntegrationsResponse"
    "fixture/ListAccountIntegrationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountIntegrations)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomains)

responseListIntegrations :: ListIntegrationsResponse -> TestTree
responseListIntegrations =
  res
    "ListIntegrationsResponse"
    "fixture/ListIntegrationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIntegrations)

responseListProfileObjectTypeTemplates :: ListProfileObjectTypeTemplatesResponse -> TestTree
responseListProfileObjectTypeTemplates =
  res
    "ListProfileObjectTypeTemplatesResponse"
    "fixture/ListProfileObjectTypeTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProfileObjectTypeTemplates)

responseListProfileObjectTypes :: ListProfileObjectTypesResponse -> TestTree
responseListProfileObjectTypes =
  res
    "ListProfileObjectTypesResponse"
    "fixture/ListProfileObjectTypesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProfileObjectTypes)

responseListProfileObjects :: ListProfileObjectsResponse -> TestTree
responseListProfileObjects =
  res
    "ListProfileObjectsResponse"
    "fixture/ListProfileObjectsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListProfileObjects)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseMergeProfiles :: MergeProfilesResponse -> TestTree
responseMergeProfiles =
  res
    "MergeProfilesResponse"
    "fixture/MergeProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy MergeProfiles)

responsePutIntegration :: PutIntegrationResponse -> TestTree
responsePutIntegration =
  res
    "PutIntegrationResponse"
    "fixture/PutIntegrationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutIntegration)

responsePutProfileObject :: PutProfileObjectResponse -> TestTree
responsePutProfileObject =
  res
    "PutProfileObjectResponse"
    "fixture/PutProfileObjectResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutProfileObject)

responsePutProfileObjectType :: PutProfileObjectTypeResponse -> TestTree
responsePutProfileObjectType =
  res
    "PutProfileObjectTypeResponse"
    "fixture/PutProfileObjectTypeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy PutProfileObjectType)

responseSearchProfiles :: SearchProfilesResponse -> TestTree
responseSearchProfiles =
  res
    "SearchProfilesResponse"
    "fixture/SearchProfilesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchProfiles)

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

responseUpdateDomain :: UpdateDomainResponse -> TestTree
responseUpdateDomain =
  res
    "UpdateDomainResponse"
    "fixture/UpdateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDomain)

responseUpdateProfile :: UpdateProfileResponse -> TestTree
responseUpdateProfile =
  res
    "UpdateProfileResponse"
    "fixture/UpdateProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateProfile)
