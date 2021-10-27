{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.Panorama
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.Panorama where

import Data.Proxy
import Network.AWS.Panorama
import Test.AWS.Fixture
import Test.AWS.Panorama.Internal
import Test.AWS.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestUpdateDeviceMetadata $
--             newUpdateDeviceMetadata
--
--         , requestDescribeApplicationInstanceDetails $
--             newDescribeApplicationInstanceDetails
--
--         , requestCreateApplicationInstance $
--             newCreateApplicationInstance
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestRemoveApplicationInstance $
--             newRemoveApplicationInstance
--
--         , requestListDevicesJobs $
--             newListDevicesJobs
--
--         , requestCreateNodeFromTemplateJob $
--             newCreateNodeFromTemplateJob
--
--         , requestDeregisterPackageVersion $
--             newDeregisterPackageVersion
--
--         , requestListPackages $
--             newListPackages
--
--         , requestDescribeApplicationInstance $
--             newDescribeApplicationInstance
--
--         , requestRegisterPackageVersion $
--             newRegisterPackageVersion
--
--         , requestDescribeNodeFromTemplateJob $
--             newDescribeNodeFromTemplateJob
--
--         , requestCreatePackageImportJob $
--             newCreatePackageImportJob
--
--         , requestDescribePackage $
--             newDescribePackage
--
--         , requestListApplicationInstances $
--             newListApplicationInstances
--
--         , requestDescribeDeviceJob $
--             newDescribeDeviceJob
--
--         , requestDescribePackageImportJob $
--             newDescribePackageImportJob
--
--         , requestDescribeDevice $
--             newDescribeDevice
--
--         , requestDescribePackageVersion $
--             newDescribePackageVersion
--
--         , requestDescribeNode $
--             newDescribeNode
--
--         , requestListNodeFromTemplateJobs $
--             newListNodeFromTemplateJobs
--
--         , requestTagResource $
--             newTagResource
--
--         , requestListApplicationInstanceDependencies $
--             newListApplicationInstanceDependencies
--
--         , requestListApplicationInstanceNodeInstances $
--             newListApplicationInstanceNodeInstances
--
--         , requestCreateJobForDevices $
--             newCreateJobForDevices
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestDeletePackage $
--             newDeletePackage
--
--         , requestCreatePackage $
--             newCreatePackage
--
--         , requestProvisionDevice $
--             newProvisionDevice
--
--         , requestDeleteDevice $
--             newDeleteDevice
--
--         , requestListNodes $
--             newListNodes
--
--         , requestListDevices $
--             newListDevices
--
--         , requestListPackageImportJobs $
--             newListPackageImportJobs
--
--           ]

--     , testGroup "response"
--         [ responseUpdateDeviceMetadata $
--             newUpdateDeviceMetadataResponse
--
--         , responseDescribeApplicationInstanceDetails $
--             newDescribeApplicationInstanceDetailsResponse
--
--         , responseCreateApplicationInstance $
--             newCreateApplicationInstanceResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseRemoveApplicationInstance $
--             newRemoveApplicationInstanceResponse
--
--         , responseListDevicesJobs $
--             newListDevicesJobsResponse
--
--         , responseCreateNodeFromTemplateJob $
--             newCreateNodeFromTemplateJobResponse
--
--         , responseDeregisterPackageVersion $
--             newDeregisterPackageVersionResponse
--
--         , responseListPackages $
--             newListPackagesResponse
--
--         , responseDescribeApplicationInstance $
--             newDescribeApplicationInstanceResponse
--
--         , responseRegisterPackageVersion $
--             newRegisterPackageVersionResponse
--
--         , responseDescribeNodeFromTemplateJob $
--             newDescribeNodeFromTemplateJobResponse
--
--         , responseCreatePackageImportJob $
--             newCreatePackageImportJobResponse
--
--         , responseDescribePackage $
--             newDescribePackageResponse
--
--         , responseListApplicationInstances $
--             newListApplicationInstancesResponse
--
--         , responseDescribeDeviceJob $
--             newDescribeDeviceJobResponse
--
--         , responseDescribePackageImportJob $
--             newDescribePackageImportJobResponse
--
--         , responseDescribeDevice $
--             newDescribeDeviceResponse
--
--         , responseDescribePackageVersion $
--             newDescribePackageVersionResponse
--
--         , responseDescribeNode $
--             newDescribeNodeResponse
--
--         , responseListNodeFromTemplateJobs $
--             newListNodeFromTemplateJobsResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseListApplicationInstanceDependencies $
--             newListApplicationInstanceDependenciesResponse
--
--         , responseListApplicationInstanceNodeInstances $
--             newListApplicationInstanceNodeInstancesResponse
--
--         , responseCreateJobForDevices $
--             newCreateJobForDevicesResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseDeletePackage $
--             newDeletePackageResponse
--
--         , responseCreatePackage $
--             newCreatePackageResponse
--
--         , responseProvisionDevice $
--             newProvisionDeviceResponse
--
--         , responseDeleteDevice $
--             newDeleteDeviceResponse
--
--         , responseListNodes $
--             newListNodesResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--         , responseListPackageImportJobs $
--             newListPackageImportJobsResponse
--
--           ]
--     ]

-- Requests

requestUpdateDeviceMetadata :: UpdateDeviceMetadata -> TestTree
requestUpdateDeviceMetadata =
  req
    "UpdateDeviceMetadata"
    "fixture/UpdateDeviceMetadata.yaml"

requestDescribeApplicationInstanceDetails :: DescribeApplicationInstanceDetails -> TestTree
requestDescribeApplicationInstanceDetails =
  req
    "DescribeApplicationInstanceDetails"
    "fixture/DescribeApplicationInstanceDetails.yaml"

requestCreateApplicationInstance :: CreateApplicationInstance -> TestTree
requestCreateApplicationInstance =
  req
    "CreateApplicationInstance"
    "fixture/CreateApplicationInstance.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestRemoveApplicationInstance :: RemoveApplicationInstance -> TestTree
requestRemoveApplicationInstance =
  req
    "RemoveApplicationInstance"
    "fixture/RemoveApplicationInstance.yaml"

requestListDevicesJobs :: ListDevicesJobs -> TestTree
requestListDevicesJobs =
  req
    "ListDevicesJobs"
    "fixture/ListDevicesJobs.yaml"

requestCreateNodeFromTemplateJob :: CreateNodeFromTemplateJob -> TestTree
requestCreateNodeFromTemplateJob =
  req
    "CreateNodeFromTemplateJob"
    "fixture/CreateNodeFromTemplateJob.yaml"

requestDeregisterPackageVersion :: DeregisterPackageVersion -> TestTree
requestDeregisterPackageVersion =
  req
    "DeregisterPackageVersion"
    "fixture/DeregisterPackageVersion.yaml"

requestListPackages :: ListPackages -> TestTree
requestListPackages =
  req
    "ListPackages"
    "fixture/ListPackages.yaml"

requestDescribeApplicationInstance :: DescribeApplicationInstance -> TestTree
requestDescribeApplicationInstance =
  req
    "DescribeApplicationInstance"
    "fixture/DescribeApplicationInstance.yaml"

requestRegisterPackageVersion :: RegisterPackageVersion -> TestTree
requestRegisterPackageVersion =
  req
    "RegisterPackageVersion"
    "fixture/RegisterPackageVersion.yaml"

requestDescribeNodeFromTemplateJob :: DescribeNodeFromTemplateJob -> TestTree
requestDescribeNodeFromTemplateJob =
  req
    "DescribeNodeFromTemplateJob"
    "fixture/DescribeNodeFromTemplateJob.yaml"

requestCreatePackageImportJob :: CreatePackageImportJob -> TestTree
requestCreatePackageImportJob =
  req
    "CreatePackageImportJob"
    "fixture/CreatePackageImportJob.yaml"

requestDescribePackage :: DescribePackage -> TestTree
requestDescribePackage =
  req
    "DescribePackage"
    "fixture/DescribePackage.yaml"

requestListApplicationInstances :: ListApplicationInstances -> TestTree
requestListApplicationInstances =
  req
    "ListApplicationInstances"
    "fixture/ListApplicationInstances.yaml"

requestDescribeDeviceJob :: DescribeDeviceJob -> TestTree
requestDescribeDeviceJob =
  req
    "DescribeDeviceJob"
    "fixture/DescribeDeviceJob.yaml"

requestDescribePackageImportJob :: DescribePackageImportJob -> TestTree
requestDescribePackageImportJob =
  req
    "DescribePackageImportJob"
    "fixture/DescribePackageImportJob.yaml"

requestDescribeDevice :: DescribeDevice -> TestTree
requestDescribeDevice =
  req
    "DescribeDevice"
    "fixture/DescribeDevice.yaml"

requestDescribePackageVersion :: DescribePackageVersion -> TestTree
requestDescribePackageVersion =
  req
    "DescribePackageVersion"
    "fixture/DescribePackageVersion.yaml"

requestDescribeNode :: DescribeNode -> TestTree
requestDescribeNode =
  req
    "DescribeNode"
    "fixture/DescribeNode.yaml"

requestListNodeFromTemplateJobs :: ListNodeFromTemplateJobs -> TestTree
requestListNodeFromTemplateJobs =
  req
    "ListNodeFromTemplateJobs"
    "fixture/ListNodeFromTemplateJobs.yaml"

requestTagResource :: TagResource -> TestTree
requestTagResource =
  req
    "TagResource"
    "fixture/TagResource.yaml"

requestListApplicationInstanceDependencies :: ListApplicationInstanceDependencies -> TestTree
requestListApplicationInstanceDependencies =
  req
    "ListApplicationInstanceDependencies"
    "fixture/ListApplicationInstanceDependencies.yaml"

requestListApplicationInstanceNodeInstances :: ListApplicationInstanceNodeInstances -> TestTree
requestListApplicationInstanceNodeInstances =
  req
    "ListApplicationInstanceNodeInstances"
    "fixture/ListApplicationInstanceNodeInstances.yaml"

requestCreateJobForDevices :: CreateJobForDevices -> TestTree
requestCreateJobForDevices =
  req
    "CreateJobForDevices"
    "fixture/CreateJobForDevices.yaml"

requestUntagResource :: UntagResource -> TestTree
requestUntagResource =
  req
    "UntagResource"
    "fixture/UntagResource.yaml"

requestDeletePackage :: DeletePackage -> TestTree
requestDeletePackage =
  req
    "DeletePackage"
    "fixture/DeletePackage.yaml"

requestCreatePackage :: CreatePackage -> TestTree
requestCreatePackage =
  req
    "CreatePackage"
    "fixture/CreatePackage.yaml"

requestProvisionDevice :: ProvisionDevice -> TestTree
requestProvisionDevice =
  req
    "ProvisionDevice"
    "fixture/ProvisionDevice.yaml"

requestDeleteDevice :: DeleteDevice -> TestTree
requestDeleteDevice =
  req
    "DeleteDevice"
    "fixture/DeleteDevice.yaml"

requestListNodes :: ListNodes -> TestTree
requestListNodes =
  req
    "ListNodes"
    "fixture/ListNodes.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

requestListPackageImportJobs :: ListPackageImportJobs -> TestTree
requestListPackageImportJobs =
  req
    "ListPackageImportJobs"
    "fixture/ListPackageImportJobs.yaml"

-- Responses

responseUpdateDeviceMetadata :: UpdateDeviceMetadataResponse -> TestTree
responseUpdateDeviceMetadata =
  res
    "UpdateDeviceMetadataResponse"
    "fixture/UpdateDeviceMetadataResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateDeviceMetadata)

responseDescribeApplicationInstanceDetails :: DescribeApplicationInstanceDetailsResponse -> TestTree
responseDescribeApplicationInstanceDetails =
  res
    "DescribeApplicationInstanceDetailsResponse"
    "fixture/DescribeApplicationInstanceDetailsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeApplicationInstanceDetails)

responseCreateApplicationInstance :: CreateApplicationInstanceResponse -> TestTree
responseCreateApplicationInstance =
  res
    "CreateApplicationInstanceResponse"
    "fixture/CreateApplicationInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy CreateApplicationInstance)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy :: Proxy ListTagsForResource)

responseRemoveApplicationInstance :: RemoveApplicationInstanceResponse -> TestTree
responseRemoveApplicationInstance =
  res
    "RemoveApplicationInstanceResponse"
    "fixture/RemoveApplicationInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy RemoveApplicationInstance)

responseListDevicesJobs :: ListDevicesJobsResponse -> TestTree
responseListDevicesJobs =
  res
    "ListDevicesJobsResponse"
    "fixture/ListDevicesJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListDevicesJobs)

responseCreateNodeFromTemplateJob :: CreateNodeFromTemplateJobResponse -> TestTree
responseCreateNodeFromTemplateJob =
  res
    "CreateNodeFromTemplateJobResponse"
    "fixture/CreateNodeFromTemplateJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreateNodeFromTemplateJob)

responseDeregisterPackageVersion :: DeregisterPackageVersionResponse -> TestTree
responseDeregisterPackageVersion =
  res
    "DeregisterPackageVersionResponse"
    "fixture/DeregisterPackageVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DeregisterPackageVersion)

responseListPackages :: ListPackagesResponse -> TestTree
responseListPackages =
  res
    "ListPackagesResponse"
    "fixture/ListPackagesResponse.proto"
    defaultService
    (Proxy :: Proxy ListPackages)

responseDescribeApplicationInstance :: DescribeApplicationInstanceResponse -> TestTree
responseDescribeApplicationInstance =
  res
    "DescribeApplicationInstanceResponse"
    "fixture/DescribeApplicationInstanceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeApplicationInstance)

responseRegisterPackageVersion :: RegisterPackageVersionResponse -> TestTree
responseRegisterPackageVersion =
  res
    "RegisterPackageVersionResponse"
    "fixture/RegisterPackageVersionResponse.proto"
    defaultService
    (Proxy :: Proxy RegisterPackageVersion)

responseDescribeNodeFromTemplateJob :: DescribeNodeFromTemplateJobResponse -> TestTree
responseDescribeNodeFromTemplateJob =
  res
    "DescribeNodeFromTemplateJobResponse"
    "fixture/DescribeNodeFromTemplateJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNodeFromTemplateJob)

responseCreatePackageImportJob :: CreatePackageImportJobResponse -> TestTree
responseCreatePackageImportJob =
  res
    "CreatePackageImportJobResponse"
    "fixture/CreatePackageImportJobResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePackageImportJob)

responseDescribePackage :: DescribePackageResponse -> TestTree
responseDescribePackage =
  res
    "DescribePackageResponse"
    "fixture/DescribePackageResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePackage)

responseListApplicationInstances :: ListApplicationInstancesResponse -> TestTree
responseListApplicationInstances =
  res
    "ListApplicationInstancesResponse"
    "fixture/ListApplicationInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy ListApplicationInstances)

responseDescribeDeviceJob :: DescribeDeviceJobResponse -> TestTree
responseDescribeDeviceJob =
  res
    "DescribeDeviceJobResponse"
    "fixture/DescribeDeviceJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDeviceJob)

responseDescribePackageImportJob :: DescribePackageImportJobResponse -> TestTree
responseDescribePackageImportJob =
  res
    "DescribePackageImportJobResponse"
    "fixture/DescribePackageImportJobResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePackageImportJob)

responseDescribeDevice :: DescribeDeviceResponse -> TestTree
responseDescribeDevice =
  res
    "DescribeDeviceResponse"
    "fixture/DescribeDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeDevice)

responseDescribePackageVersion :: DescribePackageVersionResponse -> TestTree
responseDescribePackageVersion =
  res
    "DescribePackageVersionResponse"
    "fixture/DescribePackageVersionResponse.proto"
    defaultService
    (Proxy :: Proxy DescribePackageVersion)

responseDescribeNode :: DescribeNodeResponse -> TestTree
responseDescribeNode =
  res
    "DescribeNodeResponse"
    "fixture/DescribeNodeResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeNode)

responseListNodeFromTemplateJobs :: ListNodeFromTemplateJobsResponse -> TestTree
responseListNodeFromTemplateJobs =
  res
    "ListNodeFromTemplateJobsResponse"
    "fixture/ListNodeFromTemplateJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListNodeFromTemplateJobs)

responseTagResource :: TagResourceResponse -> TestTree
responseTagResource =
  res
    "TagResourceResponse"
    "fixture/TagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy TagResource)

responseListApplicationInstanceDependencies :: ListApplicationInstanceDependenciesResponse -> TestTree
responseListApplicationInstanceDependencies =
  res
    "ListApplicationInstanceDependenciesResponse"
    "fixture/ListApplicationInstanceDependenciesResponse.proto"
    defaultService
    (Proxy :: Proxy ListApplicationInstanceDependencies)

responseListApplicationInstanceNodeInstances :: ListApplicationInstanceNodeInstancesResponse -> TestTree
responseListApplicationInstanceNodeInstances =
  res
    "ListApplicationInstanceNodeInstancesResponse"
    "fixture/ListApplicationInstanceNodeInstancesResponse.proto"
    defaultService
    (Proxy :: Proxy ListApplicationInstanceNodeInstances)

responseCreateJobForDevices :: CreateJobForDevicesResponse -> TestTree
responseCreateJobForDevices =
  res
    "CreateJobForDevicesResponse"
    "fixture/CreateJobForDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy CreateJobForDevices)

responseUntagResource :: UntagResourceResponse -> TestTree
responseUntagResource =
  res
    "UntagResourceResponse"
    "fixture/UntagResourceResponse.proto"
    defaultService
    (Proxy :: Proxy UntagResource)

responseDeletePackage :: DeletePackageResponse -> TestTree
responseDeletePackage =
  res
    "DeletePackageResponse"
    "fixture/DeletePackageResponse.proto"
    defaultService
    (Proxy :: Proxy DeletePackage)

responseCreatePackage :: CreatePackageResponse -> TestTree
responseCreatePackage =
  res
    "CreatePackageResponse"
    "fixture/CreatePackageResponse.proto"
    defaultService
    (Proxy :: Proxy CreatePackage)

responseProvisionDevice :: ProvisionDeviceResponse -> TestTree
responseProvisionDevice =
  res
    "ProvisionDeviceResponse"
    "fixture/ProvisionDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy ProvisionDevice)

responseDeleteDevice :: DeleteDeviceResponse -> TestTree
responseDeleteDevice =
  res
    "DeleteDeviceResponse"
    "fixture/DeleteDeviceResponse.proto"
    defaultService
    (Proxy :: Proxy DeleteDevice)

responseListNodes :: ListNodesResponse -> TestTree
responseListNodes =
  res
    "ListNodesResponse"
    "fixture/ListNodesResponse.proto"
    defaultService
    (Proxy :: Proxy ListNodes)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy :: Proxy ListDevices)

responseListPackageImportJobs :: ListPackageImportJobsResponse -> TestTree
responseListPackageImportJobs =
  res
    "ListPackageImportJobsResponse"
    "fixture/ListPackageImportJobsResponse.proto"
    defaultService
    (Proxy :: Proxy ListPackageImportJobs)
