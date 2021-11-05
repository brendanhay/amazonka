{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.IoTThingsGraph
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.IoTThingsGraph where

import qualified Data.Proxy as Proxy
import Network.AWS.IoTThingsGraph
import Test.AWS.Fixture
import Test.AWS.IoTThingsGraph.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestGetFlowTemplate $
--             newGetFlowTemplate
--
--         , requestUpdateSystemTemplate $
--             newUpdateSystemTemplate
--
--         , requestDeleteSystemTemplate $
--             newDeleteSystemTemplate
--
--         , requestDeprecateFlowTemplate $
--             newDeprecateFlowTemplate
--
--         , requestDeploySystemInstance $
--             newDeploySystemInstance
--
--         , requestSearchFlowTemplates $
--             newSearchFlowTemplates
--
--         , requestDeleteNamespace $
--             newDeleteNamespace
--
--         , requestGetSystemInstance $
--             newGetSystemInstance
--
--         , requestListFlowExecutionMessages $
--             newListFlowExecutionMessages
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestSearchFlowExecutions $
--             newSearchFlowExecutions
--
--         , requestDeleteSystemInstance $
--             newDeleteSystemInstance
--
--         , requestCreateSystemInstance $
--             newCreateSystemInstance
--
--         , requestDeprecateSystemTemplate $
--             newDeprecateSystemTemplate
--
--         , requestGetSystemTemplateRevisions $
--             newGetSystemTemplateRevisions
--
--         , requestSearchEntities $
--             newSearchEntities
--
--         , requestDeleteFlowTemplate $
--             newDeleteFlowTemplate
--
--         , requestUpdateFlowTemplate $
--             newUpdateFlowTemplate
--
--         , requestGetSystemTemplate $
--             newGetSystemTemplate
--
--         , requestSearchSystemInstances $
--             newSearchSystemInstances
--
--         , requestGetUploadStatus $
--             newGetUploadStatus
--
--         , requestCreateSystemTemplate $
--             newCreateSystemTemplate
--
--         , requestUndeploySystemInstance $
--             newUndeploySystemInstance
--
--         , requestGetFlowTemplateRevisions $
--             newGetFlowTemplateRevisions
--
--         , requestGetNamespaceDeletionStatus $
--             newGetNamespaceDeletionStatus
--
--         , requestAssociateEntityToThing $
--             newAssociateEntityToThing
--
--         , requestSearchSystemTemplates $
--             newSearchSystemTemplates
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestGetEntities $
--             newGetEntities
--
--         , requestDescribeNamespace $
--             newDescribeNamespace
--
--         , requestCreateFlowTemplate $
--             newCreateFlowTemplate
--
--         , requestUploadEntityDefinitions $
--             newUploadEntityDefinitions
--
--         , requestDissociateEntityFromThing $
--             newDissociateEntityFromThing
--
--         , requestSearchThings $
--             newSearchThings
--
--           ]

--     , testGroup "response"
--         [ responseGetFlowTemplate $
--             newGetFlowTemplateResponse
--
--         , responseUpdateSystemTemplate $
--             newUpdateSystemTemplateResponse
--
--         , responseDeleteSystemTemplate $
--             newDeleteSystemTemplateResponse
--
--         , responseDeprecateFlowTemplate $
--             newDeprecateFlowTemplateResponse
--
--         , responseDeploySystemInstance $
--             newDeploySystemInstanceResponse
--
--         , responseSearchFlowTemplates $
--             newSearchFlowTemplatesResponse
--
--         , responseDeleteNamespace $
--             newDeleteNamespaceResponse
--
--         , responseGetSystemInstance $
--             newGetSystemInstanceResponse
--
--         , responseListFlowExecutionMessages $
--             newListFlowExecutionMessagesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseSearchFlowExecutions $
--             newSearchFlowExecutionsResponse
--
--         , responseDeleteSystemInstance $
--             newDeleteSystemInstanceResponse
--
--         , responseCreateSystemInstance $
--             newCreateSystemInstanceResponse
--
--         , responseDeprecateSystemTemplate $
--             newDeprecateSystemTemplateResponse
--
--         , responseGetSystemTemplateRevisions $
--             newGetSystemTemplateRevisionsResponse
--
--         , responseSearchEntities $
--             newSearchEntitiesResponse
--
--         , responseDeleteFlowTemplate $
--             newDeleteFlowTemplateResponse
--
--         , responseUpdateFlowTemplate $
--             newUpdateFlowTemplateResponse
--
--         , responseGetSystemTemplate $
--             newGetSystemTemplateResponse
--
--         , responseSearchSystemInstances $
--             newSearchSystemInstancesResponse
--
--         , responseGetUploadStatus $
--             newGetUploadStatusResponse
--
--         , responseCreateSystemTemplate $
--             newCreateSystemTemplateResponse
--
--         , responseUndeploySystemInstance $
--             newUndeploySystemInstanceResponse
--
--         , responseGetFlowTemplateRevisions $
--             newGetFlowTemplateRevisionsResponse
--
--         , responseGetNamespaceDeletionStatus $
--             newGetNamespaceDeletionStatusResponse
--
--         , responseAssociateEntityToThing $
--             newAssociateEntityToThingResponse
--
--         , responseSearchSystemTemplates $
--             newSearchSystemTemplatesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseGetEntities $
--             newGetEntitiesResponse
--
--         , responseDescribeNamespace $
--             newDescribeNamespaceResponse
--
--         , responseCreateFlowTemplate $
--             newCreateFlowTemplateResponse
--
--         , responseUploadEntityDefinitions $
--             newUploadEntityDefinitionsResponse
--
--         , responseDissociateEntityFromThing $
--             newDissociateEntityFromThingResponse
--
--         , responseSearchThings $
--             newSearchThingsResponse
--
--           ]
--     ]

-- Requests

requestGetFlowTemplate :: GetFlowTemplate -> TestTree
requestGetFlowTemplate =
  req
    "GetFlowTemplate"
    "fixture/GetFlowTemplate.yaml"

requestUpdateSystemTemplate :: UpdateSystemTemplate -> TestTree
requestUpdateSystemTemplate =
  req
    "UpdateSystemTemplate"
    "fixture/UpdateSystemTemplate.yaml"

requestDeleteSystemTemplate :: DeleteSystemTemplate -> TestTree
requestDeleteSystemTemplate =
  req
    "DeleteSystemTemplate"
    "fixture/DeleteSystemTemplate.yaml"

requestDeprecateFlowTemplate :: DeprecateFlowTemplate -> TestTree
requestDeprecateFlowTemplate =
  req
    "DeprecateFlowTemplate"
    "fixture/DeprecateFlowTemplate.yaml"

requestDeploySystemInstance :: DeploySystemInstance -> TestTree
requestDeploySystemInstance =
  req
    "DeploySystemInstance"
    "fixture/DeploySystemInstance.yaml"

requestSearchFlowTemplates :: SearchFlowTemplates -> TestTree
requestSearchFlowTemplates =
  req
    "SearchFlowTemplates"
    "fixture/SearchFlowTemplates.yaml"

requestDeleteNamespace :: DeleteNamespace -> TestTree
requestDeleteNamespace =
  req
    "DeleteNamespace"
    "fixture/DeleteNamespace.yaml"

requestGetSystemInstance :: GetSystemInstance -> TestTree
requestGetSystemInstance =
  req
    "GetSystemInstance"
    "fixture/GetSystemInstance.yaml"

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

requestSearchFlowExecutions :: SearchFlowExecutions -> TestTree
requestSearchFlowExecutions =
  req
    "SearchFlowExecutions"
    "fixture/SearchFlowExecutions.yaml"

requestDeleteSystemInstance :: DeleteSystemInstance -> TestTree
requestDeleteSystemInstance =
  req
    "DeleteSystemInstance"
    "fixture/DeleteSystemInstance.yaml"

requestCreateSystemInstance :: CreateSystemInstance -> TestTree
requestCreateSystemInstance =
  req
    "CreateSystemInstance"
    "fixture/CreateSystemInstance.yaml"

requestDeprecateSystemTemplate :: DeprecateSystemTemplate -> TestTree
requestDeprecateSystemTemplate =
  req
    "DeprecateSystemTemplate"
    "fixture/DeprecateSystemTemplate.yaml"

requestGetSystemTemplateRevisions :: GetSystemTemplateRevisions -> TestTree
requestGetSystemTemplateRevisions =
  req
    "GetSystemTemplateRevisions"
    "fixture/GetSystemTemplateRevisions.yaml"

requestSearchEntities :: SearchEntities -> TestTree
requestSearchEntities =
  req
    "SearchEntities"
    "fixture/SearchEntities.yaml"

requestDeleteFlowTemplate :: DeleteFlowTemplate -> TestTree
requestDeleteFlowTemplate =
  req
    "DeleteFlowTemplate"
    "fixture/DeleteFlowTemplate.yaml"

requestUpdateFlowTemplate :: UpdateFlowTemplate -> TestTree
requestUpdateFlowTemplate =
  req
    "UpdateFlowTemplate"
    "fixture/UpdateFlowTemplate.yaml"

requestGetSystemTemplate :: GetSystemTemplate -> TestTree
requestGetSystemTemplate =
  req
    "GetSystemTemplate"
    "fixture/GetSystemTemplate.yaml"

requestSearchSystemInstances :: SearchSystemInstances -> TestTree
requestSearchSystemInstances =
  req
    "SearchSystemInstances"
    "fixture/SearchSystemInstances.yaml"

requestGetUploadStatus :: GetUploadStatus -> TestTree
requestGetUploadStatus =
  req
    "GetUploadStatus"
    "fixture/GetUploadStatus.yaml"

requestCreateSystemTemplate :: CreateSystemTemplate -> TestTree
requestCreateSystemTemplate =
  req
    "CreateSystemTemplate"
    "fixture/CreateSystemTemplate.yaml"

requestUndeploySystemInstance :: UndeploySystemInstance -> TestTree
requestUndeploySystemInstance =
  req
    "UndeploySystemInstance"
    "fixture/UndeploySystemInstance.yaml"

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

requestAssociateEntityToThing :: AssociateEntityToThing -> TestTree
requestAssociateEntityToThing =
  req
    "AssociateEntityToThing"
    "fixture/AssociateEntityToThing.yaml"

requestSearchSystemTemplates :: SearchSystemTemplates -> TestTree
requestSearchSystemTemplates =
  req
    "SearchSystemTemplates"
    "fixture/SearchSystemTemplates.yaml"

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

requestGetEntities :: GetEntities -> TestTree
requestGetEntities =
  req
    "GetEntities"
    "fixture/GetEntities.yaml"

requestDescribeNamespace :: DescribeNamespace -> TestTree
requestDescribeNamespace =
  req
    "DescribeNamespace"
    "fixture/DescribeNamespace.yaml"

requestCreateFlowTemplate :: CreateFlowTemplate -> TestTree
requestCreateFlowTemplate =
  req
    "CreateFlowTemplate"
    "fixture/CreateFlowTemplate.yaml"

requestUploadEntityDefinitions :: UploadEntityDefinitions -> TestTree
requestUploadEntityDefinitions =
  req
    "UploadEntityDefinitions"
    "fixture/UploadEntityDefinitions.yaml"

requestDissociateEntityFromThing :: DissociateEntityFromThing -> TestTree
requestDissociateEntityFromThing =
  req
    "DissociateEntityFromThing"
    "fixture/DissociateEntityFromThing.yaml"

requestSearchThings :: SearchThings -> TestTree
requestSearchThings =
  req
    "SearchThings"
    "fixture/SearchThings.yaml"

-- Responses

responseGetFlowTemplate :: GetFlowTemplateResponse -> TestTree
responseGetFlowTemplate =
  res
    "GetFlowTemplateResponse"
    "fixture/GetFlowTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetFlowTemplate)

responseUpdateSystemTemplate :: UpdateSystemTemplateResponse -> TestTree
responseUpdateSystemTemplate =
  res
    "UpdateSystemTemplateResponse"
    "fixture/UpdateSystemTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateSystemTemplate)

responseDeleteSystemTemplate :: DeleteSystemTemplateResponse -> TestTree
responseDeleteSystemTemplate =
  res
    "DeleteSystemTemplateResponse"
    "fixture/DeleteSystemTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSystemTemplate)

responseDeprecateFlowTemplate :: DeprecateFlowTemplateResponse -> TestTree
responseDeprecateFlowTemplate =
  res
    "DeprecateFlowTemplateResponse"
    "fixture/DeprecateFlowTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeprecateFlowTemplate)

responseDeploySystemInstance :: DeploySystemInstanceResponse -> TestTree
responseDeploySystemInstance =
  res
    "DeploySystemInstanceResponse"
    "fixture/DeploySystemInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeploySystemInstance)

responseSearchFlowTemplates :: SearchFlowTemplatesResponse -> TestTree
responseSearchFlowTemplates =
  res
    "SearchFlowTemplatesResponse"
    "fixture/SearchFlowTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchFlowTemplates)

responseDeleteNamespace :: DeleteNamespaceResponse -> TestTree
responseDeleteNamespace =
  res
    "DeleteNamespaceResponse"
    "fixture/DeleteNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteNamespace)

responseGetSystemInstance :: GetSystemInstanceResponse -> TestTree
responseGetSystemInstance =
  res
    "GetSystemInstanceResponse"
    "fixture/GetSystemInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSystemInstance)

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

responseSearchFlowExecutions :: SearchFlowExecutionsResponse -> TestTree
responseSearchFlowExecutions =
  res
    "SearchFlowExecutionsResponse"
    "fixture/SearchFlowExecutionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchFlowExecutions)

responseDeleteSystemInstance :: DeleteSystemInstanceResponse -> TestTree
responseDeleteSystemInstance =
  res
    "DeleteSystemInstanceResponse"
    "fixture/DeleteSystemInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteSystemInstance)

responseCreateSystemInstance :: CreateSystemInstanceResponse -> TestTree
responseCreateSystemInstance =
  res
    "CreateSystemInstanceResponse"
    "fixture/CreateSystemInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSystemInstance)

responseDeprecateSystemTemplate :: DeprecateSystemTemplateResponse -> TestTree
responseDeprecateSystemTemplate =
  res
    "DeprecateSystemTemplateResponse"
    "fixture/DeprecateSystemTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeprecateSystemTemplate)

responseGetSystemTemplateRevisions :: GetSystemTemplateRevisionsResponse -> TestTree
responseGetSystemTemplateRevisions =
  res
    "GetSystemTemplateRevisionsResponse"
    "fixture/GetSystemTemplateRevisionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSystemTemplateRevisions)

responseSearchEntities :: SearchEntitiesResponse -> TestTree
responseSearchEntities =
  res
    "SearchEntitiesResponse"
    "fixture/SearchEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchEntities)

responseDeleteFlowTemplate :: DeleteFlowTemplateResponse -> TestTree
responseDeleteFlowTemplate =
  res
    "DeleteFlowTemplateResponse"
    "fixture/DeleteFlowTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteFlowTemplate)

responseUpdateFlowTemplate :: UpdateFlowTemplateResponse -> TestTree
responseUpdateFlowTemplate =
  res
    "UpdateFlowTemplateResponse"
    "fixture/UpdateFlowTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateFlowTemplate)

responseGetSystemTemplate :: GetSystemTemplateResponse -> TestTree
responseGetSystemTemplate =
  res
    "GetSystemTemplateResponse"
    "fixture/GetSystemTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetSystemTemplate)

responseSearchSystemInstances :: SearchSystemInstancesResponse -> TestTree
responseSearchSystemInstances =
  res
    "SearchSystemInstancesResponse"
    "fixture/SearchSystemInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchSystemInstances)

responseGetUploadStatus :: GetUploadStatusResponse -> TestTree
responseGetUploadStatus =
  res
    "GetUploadStatusResponse"
    "fixture/GetUploadStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetUploadStatus)

responseCreateSystemTemplate :: CreateSystemTemplateResponse -> TestTree
responseCreateSystemTemplate =
  res
    "CreateSystemTemplateResponse"
    "fixture/CreateSystemTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateSystemTemplate)

responseUndeploySystemInstance :: UndeploySystemInstanceResponse -> TestTree
responseUndeploySystemInstance =
  res
    "UndeploySystemInstanceResponse"
    "fixture/UndeploySystemInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UndeploySystemInstance)

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

responseAssociateEntityToThing :: AssociateEntityToThingResponse -> TestTree
responseAssociateEntityToThing =
  res
    "AssociateEntityToThingResponse"
    "fixture/AssociateEntityToThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy AssociateEntityToThing)

responseSearchSystemTemplates :: SearchSystemTemplatesResponse -> TestTree
responseSearchSystemTemplates =
  res
    "SearchSystemTemplatesResponse"
    "fixture/SearchSystemTemplatesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchSystemTemplates)

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

responseGetEntities :: GetEntitiesResponse -> TestTree
responseGetEntities =
  res
    "GetEntitiesResponse"
    "fixture/GetEntitiesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEntities)

responseDescribeNamespace :: DescribeNamespaceResponse -> TestTree
responseDescribeNamespace =
  res
    "DescribeNamespaceResponse"
    "fixture/DescribeNamespaceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNamespace)

responseCreateFlowTemplate :: CreateFlowTemplateResponse -> TestTree
responseCreateFlowTemplate =
  res
    "CreateFlowTemplateResponse"
    "fixture/CreateFlowTemplateResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateFlowTemplate)

responseUploadEntityDefinitions :: UploadEntityDefinitionsResponse -> TestTree
responseUploadEntityDefinitions =
  res
    "UploadEntityDefinitionsResponse"
    "fixture/UploadEntityDefinitionsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UploadEntityDefinitions)

responseDissociateEntityFromThing :: DissociateEntityFromThingResponse -> TestTree
responseDissociateEntityFromThing =
  res
    "DissociateEntityFromThingResponse"
    "fixture/DissociateEntityFromThingResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DissociateEntityFromThing)

responseSearchThings :: SearchThingsResponse -> TestTree
responseSearchThings =
  res
    "SearchThingsResponse"
    "fixture/SearchThingsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SearchThings)
