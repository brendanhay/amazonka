{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.CustomerProfiles
-- Copyright   : (c) 2013-2023 Brendan Hay
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
--         , requestCreateCalculatedAttributeDefinition $
--             newCreateCalculatedAttributeDefinition
--
--         , requestCreateDomain $
--             newCreateDomain
--
--         , requestCreateEventStream $
--             newCreateEventStream
--
--         , requestCreateIntegrationWorkflow $
--             newCreateIntegrationWorkflow
--
--         , requestCreateProfile $
--             newCreateProfile
--
--         , requestDeleteCalculatedAttributeDefinition $
--             newDeleteCalculatedAttributeDefinition
--
--         , requestDeleteDomain $
--             newDeleteDomain
--
--         , requestDeleteEventStream $
--             newDeleteEventStream
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
--         , requestDeleteWorkflow $
--             newDeleteWorkflow
--
--         , requestGetAutoMergingPreview $
--             newGetAutoMergingPreview
--
--         , requestGetCalculatedAttributeDefinition $
--             newGetCalculatedAttributeDefinition
--
--         , requestGetCalculatedAttributeForProfile $
--             newGetCalculatedAttributeForProfile
--
--         , requestGetDomain $
--             newGetDomain
--
--         , requestGetEventStream $
--             newGetEventStream
--
--         , requestGetIdentityResolutionJob $
--             newGetIdentityResolutionJob
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
--         , requestGetWorkflow $
--             newGetWorkflow
--
--         , requestGetWorkflowSteps $
--             newGetWorkflowSteps
--
--         , requestListAccountIntegrations $
--             newListAccountIntegrations
--
--         , requestListCalculatedAttributeDefinitions $
--             newListCalculatedAttributeDefinitions
--
--         , requestListCalculatedAttributesForProfile $
--             newListCalculatedAttributesForProfile
--
--         , requestListDomains $
--             newListDomains
--
--         , requestListEventStreams $
--             newListEventStreams
--
--         , requestListIdentityResolutionJobs $
--             newListIdentityResolutionJobs
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
--         , requestListWorkflows $
--             newListWorkflows
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
--         , requestUpdateCalculatedAttributeDefinition $
--             newUpdateCalculatedAttributeDefinition
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
--         , responseCreateCalculatedAttributeDefinition $
--             newCreateCalculatedAttributeDefinitionResponse
--
--         , responseCreateDomain $
--             newCreateDomainResponse
--
--         , responseCreateEventStream $
--             newCreateEventStreamResponse
--
--         , responseCreateIntegrationWorkflow $
--             newCreateIntegrationWorkflowResponse
--
--         , responseCreateProfile $
--             newCreateProfileResponse
--
--         , responseDeleteCalculatedAttributeDefinition $
--             newDeleteCalculatedAttributeDefinitionResponse
--
--         , responseDeleteDomain $
--             newDeleteDomainResponse
--
--         , responseDeleteEventStream $
--             newDeleteEventStreamResponse
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
--         , responseDeleteWorkflow $
--             newDeleteWorkflowResponse
--
--         , responseGetAutoMergingPreview $
--             newGetAutoMergingPreviewResponse
--
--         , responseGetCalculatedAttributeDefinition $
--             newGetCalculatedAttributeDefinitionResponse
--
--         , responseGetCalculatedAttributeForProfile $
--             newGetCalculatedAttributeForProfileResponse
--
--         , responseGetDomain $
--             newGetDomainResponse
--
--         , responseGetEventStream $
--             newGetEventStreamResponse
--
--         , responseGetIdentityResolutionJob $
--             newGetIdentityResolutionJobResponse
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
--         , responseGetWorkflow $
--             newGetWorkflowResponse
--
--         , responseGetWorkflowSteps $
--             newGetWorkflowStepsResponse
--
--         , responseListAccountIntegrations $
--             newListAccountIntegrationsResponse
--
--         , responseListCalculatedAttributeDefinitions $
--             newListCalculatedAttributeDefinitionsResponse
--
--         , responseListCalculatedAttributesForProfile $
--             newListCalculatedAttributesForProfileResponse
--
--         , responseListDomains $
--             newListDomainsResponse
--
--         , responseListEventStreams $
--             newListEventStreamsResponse
--
--         , responseListIdentityResolutionJobs $
--             newListIdentityResolutionJobsResponse
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
--         , responseListWorkflows $
--             newListWorkflowsResponse
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
--         , responseUpdateCalculatedAttributeDefinition $
--             newUpdateCalculatedAttributeDefinitionResponse
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

requestCreateCalculatedAttributeDefinition :: CreateCalculatedAttributeDefinition -> TestTree
requestCreateCalculatedAttributeDefinition =
  req
    "CreateCalculatedAttributeDefinition"
    "fixture/CreateCalculatedAttributeDefinition.yaml"

requestCreateDomain :: CreateDomain -> TestTree
requestCreateDomain =
  req
    "CreateDomain"
    "fixture/CreateDomain.yaml"

requestCreateEventStream :: CreateEventStream -> TestTree
requestCreateEventStream =
  req
    "CreateEventStream"
    "fixture/CreateEventStream.yaml"

requestCreateIntegrationWorkflow :: CreateIntegrationWorkflow -> TestTree
requestCreateIntegrationWorkflow =
  req
    "CreateIntegrationWorkflow"
    "fixture/CreateIntegrationWorkflow.yaml"

requestCreateProfile :: CreateProfile -> TestTree
requestCreateProfile =
  req
    "CreateProfile"
    "fixture/CreateProfile.yaml"

requestDeleteCalculatedAttributeDefinition :: DeleteCalculatedAttributeDefinition -> TestTree
requestDeleteCalculatedAttributeDefinition =
  req
    "DeleteCalculatedAttributeDefinition"
    "fixture/DeleteCalculatedAttributeDefinition.yaml"

requestDeleteDomain :: DeleteDomain -> TestTree
requestDeleteDomain =
  req
    "DeleteDomain"
    "fixture/DeleteDomain.yaml"

requestDeleteEventStream :: DeleteEventStream -> TestTree
requestDeleteEventStream =
  req
    "DeleteEventStream"
    "fixture/DeleteEventStream.yaml"

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

requestDeleteWorkflow :: DeleteWorkflow -> TestTree
requestDeleteWorkflow =
  req
    "DeleteWorkflow"
    "fixture/DeleteWorkflow.yaml"

requestGetAutoMergingPreview :: GetAutoMergingPreview -> TestTree
requestGetAutoMergingPreview =
  req
    "GetAutoMergingPreview"
    "fixture/GetAutoMergingPreview.yaml"

requestGetCalculatedAttributeDefinition :: GetCalculatedAttributeDefinition -> TestTree
requestGetCalculatedAttributeDefinition =
  req
    "GetCalculatedAttributeDefinition"
    "fixture/GetCalculatedAttributeDefinition.yaml"

requestGetCalculatedAttributeForProfile :: GetCalculatedAttributeForProfile -> TestTree
requestGetCalculatedAttributeForProfile =
  req
    "GetCalculatedAttributeForProfile"
    "fixture/GetCalculatedAttributeForProfile.yaml"

requestGetDomain :: GetDomain -> TestTree
requestGetDomain =
  req
    "GetDomain"
    "fixture/GetDomain.yaml"

requestGetEventStream :: GetEventStream -> TestTree
requestGetEventStream =
  req
    "GetEventStream"
    "fixture/GetEventStream.yaml"

requestGetIdentityResolutionJob :: GetIdentityResolutionJob -> TestTree
requestGetIdentityResolutionJob =
  req
    "GetIdentityResolutionJob"
    "fixture/GetIdentityResolutionJob.yaml"

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

requestGetWorkflow :: GetWorkflow -> TestTree
requestGetWorkflow =
  req
    "GetWorkflow"
    "fixture/GetWorkflow.yaml"

requestGetWorkflowSteps :: GetWorkflowSteps -> TestTree
requestGetWorkflowSteps =
  req
    "GetWorkflowSteps"
    "fixture/GetWorkflowSteps.yaml"

requestListAccountIntegrations :: ListAccountIntegrations -> TestTree
requestListAccountIntegrations =
  req
    "ListAccountIntegrations"
    "fixture/ListAccountIntegrations.yaml"

requestListCalculatedAttributeDefinitions :: ListCalculatedAttributeDefinitions -> TestTree
requestListCalculatedAttributeDefinitions =
  req
    "ListCalculatedAttributeDefinitions"
    "fixture/ListCalculatedAttributeDefinitions.yaml"

requestListCalculatedAttributesForProfile :: ListCalculatedAttributesForProfile -> TestTree
requestListCalculatedAttributesForProfile =
  req
    "ListCalculatedAttributesForProfile"
    "fixture/ListCalculatedAttributesForProfile.yaml"

requestListDomains :: ListDomains -> TestTree
requestListDomains =
  req
    "ListDomains"
    "fixture/ListDomains.yaml"

requestListEventStreams :: ListEventStreams -> TestTree
requestListEventStreams =
  req
    "ListEventStreams"
    "fixture/ListEventStreams.yaml"

requestListIdentityResolutionJobs :: ListIdentityResolutionJobs -> TestTree
requestListIdentityResolutionJobs =
  req
    "ListIdentityResolutionJobs"
    "fixture/ListIdentityResolutionJobs.yaml"

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

requestListWorkflows :: ListWorkflows -> TestTree
requestListWorkflows =
  req
    "ListWorkflows"
    "fixture/ListWorkflows.yaml"

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

requestUpdateCalculatedAttributeDefinition :: UpdateCalculatedAttributeDefinition -> TestTree
requestUpdateCalculatedAttributeDefinition =
  req
    "UpdateCalculatedAttributeDefinition"
    "fixture/UpdateCalculatedAttributeDefinition.yaml"

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

responseCreateCalculatedAttributeDefinition :: CreateCalculatedAttributeDefinitionResponse -> TestTree
responseCreateCalculatedAttributeDefinition =
  res
    "CreateCalculatedAttributeDefinitionResponse"
    "fixture/CreateCalculatedAttributeDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateCalculatedAttributeDefinition)

responseCreateDomain :: CreateDomainResponse -> TestTree
responseCreateDomain =
  res
    "CreateDomainResponse"
    "fixture/CreateDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateDomain)

responseCreateEventStream :: CreateEventStreamResponse -> TestTree
responseCreateEventStream =
  res
    "CreateEventStreamResponse"
    "fixture/CreateEventStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateEventStream)

responseCreateIntegrationWorkflow :: CreateIntegrationWorkflowResponse -> TestTree
responseCreateIntegrationWorkflow =
  res
    "CreateIntegrationWorkflowResponse"
    "fixture/CreateIntegrationWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateIntegrationWorkflow)

responseCreateProfile :: CreateProfileResponse -> TestTree
responseCreateProfile =
  res
    "CreateProfileResponse"
    "fixture/CreateProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateProfile)

responseDeleteCalculatedAttributeDefinition :: DeleteCalculatedAttributeDefinitionResponse -> TestTree
responseDeleteCalculatedAttributeDefinition =
  res
    "DeleteCalculatedAttributeDefinitionResponse"
    "fixture/DeleteCalculatedAttributeDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteCalculatedAttributeDefinition)

responseDeleteDomain :: DeleteDomainResponse -> TestTree
responseDeleteDomain =
  res
    "DeleteDomainResponse"
    "fixture/DeleteDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDomain)

responseDeleteEventStream :: DeleteEventStreamResponse -> TestTree
responseDeleteEventStream =
  res
    "DeleteEventStreamResponse"
    "fixture/DeleteEventStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteEventStream)

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

responseDeleteWorkflow :: DeleteWorkflowResponse -> TestTree
responseDeleteWorkflow =
  res
    "DeleteWorkflowResponse"
    "fixture/DeleteWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteWorkflow)

responseGetAutoMergingPreview :: GetAutoMergingPreviewResponse -> TestTree
responseGetAutoMergingPreview =
  res
    "GetAutoMergingPreviewResponse"
    "fixture/GetAutoMergingPreviewResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAutoMergingPreview)

responseGetCalculatedAttributeDefinition :: GetCalculatedAttributeDefinitionResponse -> TestTree
responseGetCalculatedAttributeDefinition =
  res
    "GetCalculatedAttributeDefinitionResponse"
    "fixture/GetCalculatedAttributeDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCalculatedAttributeDefinition)

responseGetCalculatedAttributeForProfile :: GetCalculatedAttributeForProfileResponse -> TestTree
responseGetCalculatedAttributeForProfile =
  res
    "GetCalculatedAttributeForProfileResponse"
    "fixture/GetCalculatedAttributeForProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetCalculatedAttributeForProfile)

responseGetDomain :: GetDomainResponse -> TestTree
responseGetDomain =
  res
    "GetDomainResponse"
    "fixture/GetDomainResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetDomain)

responseGetEventStream :: GetEventStreamResponse -> TestTree
responseGetEventStream =
  res
    "GetEventStreamResponse"
    "fixture/GetEventStreamResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEventStream)

responseGetIdentityResolutionJob :: GetIdentityResolutionJobResponse -> TestTree
responseGetIdentityResolutionJob =
  res
    "GetIdentityResolutionJobResponse"
    "fixture/GetIdentityResolutionJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetIdentityResolutionJob)

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

responseGetWorkflow :: GetWorkflowResponse -> TestTree
responseGetWorkflow =
  res
    "GetWorkflowResponse"
    "fixture/GetWorkflowResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkflow)

responseGetWorkflowSteps :: GetWorkflowStepsResponse -> TestTree
responseGetWorkflowSteps =
  res
    "GetWorkflowStepsResponse"
    "fixture/GetWorkflowStepsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetWorkflowSteps)

responseListAccountIntegrations :: ListAccountIntegrationsResponse -> TestTree
responseListAccountIntegrations =
  res
    "ListAccountIntegrationsResponse"
    "fixture/ListAccountIntegrationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListAccountIntegrations)

responseListCalculatedAttributeDefinitions :: ListCalculatedAttributeDefinitionsResponse -> TestTree
responseListCalculatedAttributeDefinitions =
  res
    "ListCalculatedAttributeDefinitionsResponse"
    "fixture/ListCalculatedAttributeDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCalculatedAttributeDefinitions)

responseListCalculatedAttributesForProfile :: ListCalculatedAttributesForProfileResponse -> TestTree
responseListCalculatedAttributesForProfile =
  res
    "ListCalculatedAttributesForProfileResponse"
    "fixture/ListCalculatedAttributesForProfileResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListCalculatedAttributesForProfile)

responseListDomains :: ListDomainsResponse -> TestTree
responseListDomains =
  res
    "ListDomainsResponse"
    "fixture/ListDomainsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDomains)

responseListEventStreams :: ListEventStreamsResponse -> TestTree
responseListEventStreams =
  res
    "ListEventStreamsResponse"
    "fixture/ListEventStreamsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListEventStreams)

responseListIdentityResolutionJobs :: ListIdentityResolutionJobsResponse -> TestTree
responseListIdentityResolutionJobs =
  res
    "ListIdentityResolutionJobsResponse"
    "fixture/ListIdentityResolutionJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListIdentityResolutionJobs)

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

responseListWorkflows :: ListWorkflowsResponse -> TestTree
responseListWorkflows =
  res
    "ListWorkflowsResponse"
    "fixture/ListWorkflowsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListWorkflows)

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

responseUpdateCalculatedAttributeDefinition :: UpdateCalculatedAttributeDefinitionResponse -> TestTree
responseUpdateCalculatedAttributeDefinition =
  res
    "UpdateCalculatedAttributeDefinitionResponse"
    "fixture/UpdateCalculatedAttributeDefinitionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateCalculatedAttributeDefinition)

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
