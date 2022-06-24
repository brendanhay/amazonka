{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.IoTThingsGraph
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.IoTThingsGraph where

import Amazonka.IoTThingsGraph
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.IoTThingsGraph.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestAssociateEntityToThing $
--             newAssociateEntityToThing
--
--         , requestCreateFlowTemplate $
--             newCreateFlowTemplate
--
--         , requestCreateSystemInstance $
--             newCreateSystemInstance
--
--         , requestCreateSystemTemplate $
--             newCreateSystemTemplate
--
--         , requestDeleteFlowTemplate $
--             newDeleteFlowTemplate
--
--         , requestDeleteNamespace $
--             newDeleteNamespace
--
--         , requestDeleteSystemInstance $
--             newDeleteSystemInstance
--
--         , requestDeleteSystemTemplate $
--             newDeleteSystemTemplate
--
--         , requestDeploySystemInstance $
--             newDeploySystemInstance
--
--         , requestDeprecateFlowTemplate $
--             newDeprecateFlowTemplate
--
--         , requestDeprecateSystemTemplate $
--             newDeprecateSystemTemplate
--
--         , requestDescribeNamespace $
--             newDescribeNamespace
--
--         , requestDissociateEntityFromThing $
--             newDissociateEntityFromThing
--
--         , requestGetEntities $
--             newGetEntities
--
--         , requestGetFlowTemplate $
--             newGetFlowTemplate
--
--         , requestGetFlowTemplateRevisions $
--             newGetFlowTemplateRevisions
--
--         , requestGetNamespaceDeletionStatus $
--             newGetNamespaceDeletionStatus
--
--         , requestGetSystemInstance $
--             newGetSystemInstance
--
--         , requestGetSystemTemplate $
--             newGetSystemTemplate
--
--         , requestGetSystemTemplateRevisions $
--             newGetSystemTemplateRevisions
--
--         , requestGetUploadStatus $
--             newGetUploadStatus
--
--         , requestListFlowExecutionMessages $
--             newListFlowExecutionMessages
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestSearchEntities $
--             newSearchEntities
--
--         , requestSearchFlowExecutions $
--             newSearchFlowExecutions
--
--         , requestSearchFlowTemplates $
--             newSearchFlowTemplates
--
--         , requestSearchSystemInstances $
--             newSearchSystemInstances
--
--         , requestSearchSystemTemplates $
--             newSearchSystemTemplates
--
--         , requestSearchThings $
--             newSearchThings
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUndeploySystemInstance $
--             newUndeploySystemInstance
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateFlowTemplate $
--             newUpdateFlowTemplate
--
--         , requestUpdateSystemTemplate $
--             newUpdateSystemTemplate
--
--         , requestUploadEntityDefinitions $
--             newUploadEntityDefinitions
--
--           ]

--     , testGroup "response"
--         [ responseAssociateEntityToThing $
--             newAssociateEntityToThingResponse
--
--         , responseCreateFlowTemplate $
--             newCreateFlowTemplateResponse
--
--         , responseCreateSystemInstance $
--             newCreateSystemInstanceResponse
--
--         , responseCreateSystemTemplate $
--             newCreateSystemTemplateResponse
--
--         , responseDeleteFlowTemplate $
--             newDeleteFlowTemplateResponse
--
--         , responseDeleteNamespace $
--             newDeleteNamespaceResponse
--
--         , responseDeleteSystemInstance $
--             newDeleteSystemInstanceResponse
--
--         , responseDeleteSystemTemplate $
--             newDeleteSystemTemplateResponse
--
--         , responseDeploySystemInstance $
--             newDeploySystemInstanceResponse
--
--         , responseDeprecateFlowTemplate $
--             newDeprecateFlowTemplateResponse
--
--         , responseDeprecateSystemTemplate $
--             newDeprecateSystemTemplateResponse
--
--         , responseDescribeNamespace $
--             newDescribeNamespaceResponse
--
--         , responseDissociateEntityFromThing $
--             newDissociateEntityFromThingResponse
--
--         , responseGetEntities $
--             newGetEntitiesResponse
--
--         , responseGetFlowTemplate $
--             newGetFlowTemplateResponse
--
--         , responseGetFlowTemplateRevisions $
--             newGetFlowTemplateRevisionsResponse
--
--         , responseGetNamespaceDeletionStatus $
--             newGetNamespaceDeletionStatusResponse
--
--         , responseGetSystemInstance $
--             newGetSystemInstanceResponse
--
--         , responseGetSystemTemplate $
--             newGetSystemTemplateResponse
--
--         , responseGetSystemTemplateRevisions $
--             newGetSystemTemplateRevisionsResponse
--
--         , responseGetUploadStatus $
--             newGetUploadStatusResponse
--
--         , responseListFlowExecutionMessages $
--             newListFlowExecutionMessagesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseSearchEntities $
--             newSearchEntitiesResponse
--
--         , responseSearchFlowExecutions $
--             newSearchFlowExecutionsResponse
--
--         , responseSearchFlowTemplates $
--             newSearchFlowTemplatesResponse
--
--         , responseSearchSystemInstances $
--             newSearchSystemInstancesResponse
--
--         , responseSearchSystemTemplates $
--             newSearchSystemTemplatesResponse
--
--         , responseSearchThings $
--             newSearchThingsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUndeploySystemInstance $
--             newUndeploySystemInstanceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateFlowTemplate $
--             newUpdateFlowTemplateResponse
--
--         , responseUpdateSystemTemplate $
--             newUpdateSystemTemplateResponse
--
--         , responseUploadEntityDefinitions $
--             newUploadEntityDefinitionsResponse
--
--           ]
--     ]

-- Requests

requestAssociateEntityToThing :: AssociateEntityToThing -> TestTree
requestAssociateEntityToThing =
  req
    "AssociateEntityToThing"
    "fixture/AssociateEntityToThing.yaml"

requestCreateFlowTemplate :: CreateFlowTemplate -> TestTree
requestCreateFlowTemplate =
  req
    "CreateFlowTemplate"
    "fixture/CreateFlowTemplate.yaml"

requestCreateSystemInstance :: CreateSystemInstance -> TestTree
requestCreateSystemInstance =
  req
    "CreateSystemInstance"
    "fixture/CreateSystemInstance.yaml"

requestCreateSystemTemplate :: CreateSystemTemplate -> TestTree
requestCreateSystemTemplate =
  req
    "CreateSystemTemplate"
    "fixture/CreateSystemTemplate.yaml"

requestDeleteFlowTemplate :: DeleteFlowTemplate -> TestTree
requestDeleteFlowTemplate =
  req
    "DeleteFlowTemplate"
    "fixture/DeleteFlowTemplate.yaml"

requestDeleteNamespace :: DeleteNamespace -> TestTree
requestDeleteNamespace =
  req
    "DeleteNamespace"
    "fixture/DeleteNamespace.yaml"

requestDeleteSystemInstance :: DeleteSystemInstance -> TestTree
requestDeleteSystemInstance =
  req
    "DeleteSystemInstance"
    "fixture/DeleteSystemInstance.yaml"

requestDeleteSystemTemplate :: DeleteSystemTemplate -> TestTree
requestDeleteSystemTemplate =
  req
    "DeleteSystemTemplate"
    "fixture/DeleteSystemTemplate.yaml"

requestDeploySystemInstance :: DeploySystemInstance -> TestTree
requestDeploySystemInstance =
  req
    "DeploySystemInstance"
    "fixture/DeploySystemInstance.yaml"

requestDeprecateFlowTemplate :: DeprecateFlowTemplate -> TestTree
requestDeprecateFlowTemplate =
  req
    "DeprecateFlowTemplate"
    "fixture/DeprecateFlowTemplate.yaml"

requestDeprecateSystemTemplate :: DeprecateSystemTemplate -> TestTree
requestDeprecateSystemTemplate =
  req
    "DeprecateSystemTemplate"
    "fixture/DeprecateSystemTemplate.yaml"

requestDescribeNamespace :: DescribeNamespace -> TestTree
requestDescribeNamespace =
  req
    "DescribeNamespace"
    "fixture/DescribeNamespace.yaml"

requestDissociateEntityFromThing :: DissociateEntityFromThing -> TestTree
requestDissociateEntityFromThing =
  req
    "DissociateEntityFromThing"
    "fixture/DissociateEntityFromThing.yaml"

requestGetEntities :: GetEntities -> TestTree
requestGetEntities =
  req
    "GetEntities"
    "fixture/GetEntities.yaml"

requestGetFlowTemplate :: GetFlowTemplate -> TestTree
requestGetFlowTemplate =
  req
    "GetFlowTemplate"
    "fixture/GetFlowTemplate.yaml"

requestGetFlowTemplateRevisions :: GetFlowTemplateRevisions -> TestTree
requestGetFlowTemplateRevisions =
  req
    "GetFlowTemplateRevisions"
    "fixture/GetFlowTemplateRevisions.yaml"

requestGetNamespaceDeletionStatus :: GetNamespaceDeletionStatus -> TestTree
requestGetNamespaceDeletionStatus =
  req
    "GetNamespaceDeletionStatus"
    "fixture/GetNamespaceDeletionStatus.yaml"

requestGetSystemInstance :: GetSystemInstance -> TestTree
requestGetSystemInstance =
  req
    "GetSystemInstance"
    "fixture/GetSystemInstance.yaml"

requestGetSystemTemplate :: GetSystemTemplate -> TestTree
requestGetSystemTemplate =
  req
    "GetSystemTemplate"
    "fixture/GetSystemTemplate.yaml"

requestGetSystemTemplateRevisions :: GetSystemTemplateRevisions -> TestTree
requestGetSystemTemplateRevisions =
  req
    "GetSystemTemplateRevisions"
    "fixture/GetSystemTemplateRevisions.yaml"

requestGetUploadStatus :: GetUploadStatus -> TestTree
requestGetUploadStatus =
  req
    "GetUploadStatus"
    "fixture/GetUploadStatus.yaml"

requestListFlowExecutionMessages :: ListFlowExecutionMessages -> TestTree
requestListFlowExecutionMessages =
  req
    "ListFlowExecutionMessages"
    "fixture/ListFlowExecutionMessages.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestSearchEntities :: SearchEntities -> TestTree
requestSearchEntities =
  req
    "SearchEntities"
    "fixture/SearchEntities.yaml"

requestSearchFlowExecutions :: SearchFlowExecutions -> TestTree
requestSearchFlowExecutions =
  req
    "SearchFlowExecutions"
    "fixture/SearchFlowExecutions.yaml"

requestSearchFlowTemplates :: SearchFlowTemplates -> TestTree
requestSearchFlowTemplates =
  req
    "SearchFlowTemplates"
    "fixture/SearchFlowTemplates.yaml"

requestSearchSystemInstances :: SearchSystemInstances -> TestTree
requestSearchSystemInstances =
  req
    "SearchSystemInstances"
    "fixture/SearchSystemInstances.yaml"

requestSearchSystemTemplates :: SearchSystemTemplates -> TestTree
requestSearchSystemTemplates =
  req
    "SearchSystemTemplates"
    "fixture/SearchSystemTemplates.yaml"

requestSearchThings :: SearchThings -> TestTree
requestSearchThings =
  req
    "SearchThings"
    "fixture/SearchThings.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestUndeploySystemInstance :: UndeploySystemInstance -> TestTree
requestUndeploySystemInstance =
  req
    "UndeploySystemInstance"
    "fixture/UndeploySystemInstance.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestUpdateFlowTemplate :: UpdateFlowTemplate -> TestTree
requestUpdateFlowTemplate =
  req
    "UpdateFlowTemplate"
    "fixture/UpdateFlowTemplate.yaml"

requestUpdateSystemTemplate :: UpdateSystemTemplate -> TestTree
requestUpdateSystemTemplate =
  req
    "UpdateSystemTemplate"
    "fixture/UpdateSystemTemplate.yaml"

requestUploadEntityDefinitions :: UploadEntityDefinitions -> TestTree
requestUploadEntityDefinitions =
  req
    "UploadEntityDefinitions"
    "fixture/UploadEntityDefinitions.yaml"

-- Responses

responseAssociateEntityToThing :: AssociateEntityToThingResponse -> TestTree
responseAssociateEntityToThing =
  res
    "AssociateEntityToThingResponse"
    "fixture/AssociateEntityToThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateEntityToThing)

responseCreateFlowTemplate :: CreateFlowTemplateResponse -> TestTree
responseCreateFlowTemplate =
  res
    "CreateFlowTemplateResponse"
    "fixture/CreateFlowTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFlowTemplate)

responseCreateSystemInstance :: CreateSystemInstanceResponse -> TestTree
responseCreateSystemInstance =
  res
    "CreateSystemInstanceResponse"
    "fixture/CreateSystemInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSystemInstance)

responseCreateSystemTemplate :: CreateSystemTemplateResponse -> TestTree
responseCreateSystemTemplate =
  res
    "CreateSystemTemplateResponse"
    "fixture/CreateSystemTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSystemTemplate)

responseDeleteFlowTemplate :: DeleteFlowTemplateResponse -> TestTree
responseDeleteFlowTemplate =
  res
    "DeleteFlowTemplateResponse"
    "fixture/DeleteFlowTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFlowTemplate)

responseDeleteNamespace :: DeleteNamespaceResponse -> TestTree
responseDeleteNamespace =
  res
    "DeleteNamespaceResponse"
    "fixture/DeleteNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNamespace)

responseDeleteSystemInstance :: DeleteSystemInstanceResponse -> TestTree
responseDeleteSystemInstance =
  res
    "DeleteSystemInstanceResponse"
    "fixture/DeleteSystemInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSystemInstance)

responseDeleteSystemTemplate :: DeleteSystemTemplateResponse -> TestTree
responseDeleteSystemTemplate =
  res
    "DeleteSystemTemplateResponse"
    "fixture/DeleteSystemTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSystemTemplate)

responseDeploySystemInstance :: DeploySystemInstanceResponse -> TestTree
responseDeploySystemInstance =
  res
    "DeploySystemInstanceResponse"
    "fixture/DeploySystemInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeploySystemInstance)

responseDeprecateFlowTemplate :: DeprecateFlowTemplateResponse -> TestTree
responseDeprecateFlowTemplate =
  res
    "DeprecateFlowTemplateResponse"
    "fixture/DeprecateFlowTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeprecateFlowTemplate)

responseDeprecateSystemTemplate :: DeprecateSystemTemplateResponse -> TestTree
responseDeprecateSystemTemplate =
  res
    "DeprecateSystemTemplateResponse"
    "fixture/DeprecateSystemTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeprecateSystemTemplate)

responseDescribeNamespace :: DescribeNamespaceResponse -> TestTree
responseDescribeNamespace =
  res
    "DescribeNamespaceResponse"
    "fixture/DescribeNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNamespace)

responseDissociateEntityFromThing :: DissociateEntityFromThingResponse -> TestTree
responseDissociateEntityFromThing =
  res
    "DissociateEntityFromThingResponse"
    "fixture/DissociateEntityFromThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DissociateEntityFromThing)

responseGetEntities :: GetEntitiesResponse -> TestTree
responseGetEntities =
  res
    "GetEntitiesResponse"
    "fixture/GetEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEntities)

responseGetFlowTemplate :: GetFlowTemplateResponse -> TestTree
responseGetFlowTemplate =
  res
    "GetFlowTemplateResponse"
    "fixture/GetFlowTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFlowTemplate)

responseGetFlowTemplateRevisions :: GetFlowTemplateRevisionsResponse -> TestTree
responseGetFlowTemplateRevisions =
  res
    "GetFlowTemplateRevisionsResponse"
    "fixture/GetFlowTemplateRevisionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFlowTemplateRevisions)

responseGetNamespaceDeletionStatus :: GetNamespaceDeletionStatusResponse -> TestTree
responseGetNamespaceDeletionStatus =
  res
    "GetNamespaceDeletionStatusResponse"
    "fixture/GetNamespaceDeletionStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetNamespaceDeletionStatus)

responseGetSystemInstance :: GetSystemInstanceResponse -> TestTree
responseGetSystemInstance =
  res
    "GetSystemInstanceResponse"
    "fixture/GetSystemInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSystemInstance)

responseGetSystemTemplate :: GetSystemTemplateResponse -> TestTree
responseGetSystemTemplate =
  res
    "GetSystemTemplateResponse"
    "fixture/GetSystemTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSystemTemplate)

responseGetSystemTemplateRevisions :: GetSystemTemplateRevisionsResponse -> TestTree
responseGetSystemTemplateRevisions =
  res
    "GetSystemTemplateRevisionsResponse"
    "fixture/GetSystemTemplateRevisionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSystemTemplateRevisions)

responseGetUploadStatus :: GetUploadStatusResponse -> TestTree
responseGetUploadStatus =
  res
    "GetUploadStatusResponse"
    "fixture/GetUploadStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUploadStatus)

responseListFlowExecutionMessages :: ListFlowExecutionMessagesResponse -> TestTree
responseListFlowExecutionMessages =
  res
    "ListFlowExecutionMessagesResponse"
    "fixture/ListFlowExecutionMessagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListFlowExecutionMessages)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseSearchEntities :: SearchEntitiesResponse -> TestTree
responseSearchEntities =
  res
    "SearchEntitiesResponse"
    "fixture/SearchEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchEntities)

responseSearchFlowExecutions :: SearchFlowExecutionsResponse -> TestTree
responseSearchFlowExecutions =
  res
    "SearchFlowExecutionsResponse"
    "fixture/SearchFlowExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchFlowExecutions)

responseSearchFlowTemplates :: SearchFlowTemplatesResponse -> TestTree
responseSearchFlowTemplates =
  res
    "SearchFlowTemplatesResponse"
    "fixture/SearchFlowTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchFlowTemplates)

responseSearchSystemInstances :: SearchSystemInstancesResponse -> TestTree
responseSearchSystemInstances =
  res
    "SearchSystemInstancesResponse"
    "fixture/SearchSystemInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchSystemInstances)

responseSearchSystemTemplates :: SearchSystemTemplatesResponse -> TestTree
responseSearchSystemTemplates =
  res
    "SearchSystemTemplatesResponse"
    "fixture/SearchSystemTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchSystemTemplates)

responseSearchThings :: SearchThingsResponse -> TestTree
responseSearchThings =
  res
    "SearchThingsResponse"
    "fixture/SearchThingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchThings)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy TagResource)

responseUndeploySystemInstance :: UndeploySystemInstanceResponse -> TestTree
responseUndeploySystemInstance =
  res
    "UndeploySystemInstanceResponse"
    "fixture/UndeploySystemInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UndeploySystemInstance)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UntagResource)

responseUpdateFlowTemplate :: UpdateFlowTemplateResponse -> TestTree
responseUpdateFlowTemplate =
  res
    "UpdateFlowTemplateResponse"
    "fixture/UpdateFlowTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFlowTemplate)

responseUpdateSystemTemplate :: UpdateSystemTemplateResponse -> TestTree
responseUpdateSystemTemplate =
  res
    "UpdateSystemTemplateResponse"
    "fixture/UpdateSystemTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSystemTemplate)

responseUploadEntityDefinitions :: UploadEntityDefinitionsResponse -> TestTree
responseUploadEntityDefinitions =
  res
    "UploadEntityDefinitionsResponse"
    "fixture/UploadEntityDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UploadEntityDefinitions)
