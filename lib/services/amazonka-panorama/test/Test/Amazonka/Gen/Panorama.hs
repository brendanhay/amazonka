{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.Panorama
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.Panorama where

import Amazonka.Panorama
import qualified Data.Proxy as Proxy
import Test.Amazonka.Fixture
import Test.Amazonka.Panorama.Internal
import Test.Amazonka.Prelude
import Test.Tasty

-- Auto-generated: the actual test selection needs to be manually placed into
-- the top-level so that real test data can be incrementally added.
--
-- This commented snippet is what the entire set should look like:

-- fixtures :: TestTree
-- fixtures =
--     [ testGroup "request"
--         [ requestCreateApplicationInstance $
--             newCreateApplicationInstance
--
--         , requestCreateJobForDevices $
--             newCreateJobForDevices
--
--         , requestCreateNodeFromTemplateJob $
--             newCreateNodeFromTemplateJob
--
--         , requestCreatePackage $
--             newCreatePackage
--
--         , requestCreatePackageImportJob $
--             newCreatePackageImportJob
--
--         , requestDeleteDevice $
--             newDeleteDevice
--
--         , requestDeletePackage $
--             newDeletePackage
--
--         , requestDeregisterPackageVersion $
--             newDeregisterPackageVersion
--
--         , requestDescribeApplicationInstance $
--             newDescribeApplicationInstance
--
--         , requestDescribeApplicationInstanceDetails $
--             newDescribeApplicationInstanceDetails
--
--         , requestDescribeDevice $
--             newDescribeDevice
--
--         , requestDescribeDeviceJob $
--             newDescribeDeviceJob
--
--         , requestDescribeNode $
--             newDescribeNode
--
--         , requestDescribeNodeFromTemplateJob $
--             newDescribeNodeFromTemplateJob
--
--         , requestDescribePackage $
--             newDescribePackage
--
--         , requestDescribePackageImportJob $
--             newDescribePackageImportJob
--
--         , requestDescribePackageVersion $
--             newDescribePackageVersion
--
--         , requestListApplicationInstanceDependencies $
--             newListApplicationInstanceDependencies
--
--         , requestListApplicationInstanceNodeInstances $
--             newListApplicationInstanceNodeInstances
--
--         , requestListApplicationInstances $
--             newListApplicationInstances
--
--         , requestListDevices $
--             newListDevices
--
--         , requestListDevicesJobs $
--             newListDevicesJobs
--
--         , requestListNodeFromTemplateJobs $
--             newListNodeFromTemplateJobs
--
--         , requestListNodes $
--             newListNodes
--
--         , requestListPackageImportJobs $
--             newListPackageImportJobs
--
--         , requestListPackages $
--             newListPackages
--
--         , requestListTagsForResource $
--             newListTagsForResource
--
--         , requestProvisionDevice $
--             newProvisionDevice
--
--         , requestRegisterPackageVersion $
--             newRegisterPackageVersion
--
--         , requestRemoveApplicationInstance $
--             newRemoveApplicationInstance
--
--         , requestSignalApplicationInstanceNodeInstances $
--             newSignalApplicationInstanceNodeInstances
--
--         , requestTagResource $
--             newTagResource
--
--         , requestUntagResource $
--             newUntagResource
--
--         , requestUpdateDeviceMetadata $
--             newUpdateDeviceMetadata
--
--           ]

--     , testGroup "response"
--         [ responseCreateApplicationInstance $
--             newCreateApplicationInstanceResponse
--
--         , responseCreateJobForDevices $
--             newCreateJobForDevicesResponse
--
--         , responseCreateNodeFromTemplateJob $
--             newCreateNodeFromTemplateJobResponse
--
--         , responseCreatePackage $
--             newCreatePackageResponse
--
--         , responseCreatePackageImportJob $
--             newCreatePackageImportJobResponse
--
--         , responseDeleteDevice $
--             newDeleteDeviceResponse
--
--         , responseDeletePackage $
--             newDeletePackageResponse
--
--         , responseDeregisterPackageVersion $
--             newDeregisterPackageVersionResponse
--
--         , responseDescribeApplicationInstance $
--             newDescribeApplicationInstanceResponse
--
--         , responseDescribeApplicationInstanceDetails $
--             newDescribeApplicationInstanceDetailsResponse
--
--         , responseDescribeDevice $
--             newDescribeDeviceResponse
--
--         , responseDescribeDeviceJob $
--             newDescribeDeviceJobResponse
--
--         , responseDescribeNode $
--             newDescribeNodeResponse
--
--         , responseDescribeNodeFromTemplateJob $
--             newDescribeNodeFromTemplateJobResponse
--
--         , responseDescribePackage $
--             newDescribePackageResponse
--
--         , responseDescribePackageImportJob $
--             newDescribePackageImportJobResponse
--
--         , responseDescribePackageVersion $
--             newDescribePackageVersionResponse
--
--         , responseListApplicationInstanceDependencies $
--             newListApplicationInstanceDependenciesResponse
--
--         , responseListApplicationInstanceNodeInstances $
--             newListApplicationInstanceNodeInstancesResponse
--
--         , responseListApplicationInstances $
--             newListApplicationInstancesResponse
--
--         , responseListDevices $
--             newListDevicesResponse
--
--         , responseListDevicesJobs $
--             newListDevicesJobsResponse
--
--         , responseListNodeFromTemplateJobs $
--             newListNodeFromTemplateJobsResponse
--
--         , responseListNodes $
--             newListNodesResponse
--
--         , responseListPackageImportJobs $
--             newListPackageImportJobsResponse
--
--         , responseListPackages $
--             newListPackagesResponse
--
--         , responseListTagsForResource $
--             newListTagsForResourceResponse
--
--         , responseProvisionDevice $
--             newProvisionDeviceResponse
--
--         , responseRegisterPackageVersion $
--             newRegisterPackageVersionResponse
--
--         , responseRemoveApplicationInstance $
--             newRemoveApplicationInstanceResponse
--
--         , responseSignalApplicationInstanceNodeInstances $
--             newSignalApplicationInstanceNodeInstancesResponse
--
--         , responseTagResource $
--             newTagResourceResponse
--
--         , responseUntagResource $
--             newUntagResourceResponse
--
--         , responseUpdateDeviceMetadata $
--             newUpdateDeviceMetadataResponse
--
--           ]
--     ]

-- Requests

requestCreateApplicationInstance :: CreateApplicationInstance -> TestTree
requestCreateApplicationInstance =
  req
    "CreateApplicationInstance"
    "fixture/CreateApplicationInstance.yaml"

requestCreateJobForDevices :: CreateJobForDevices -> TestTree
requestCreateJobForDevices =
  req
    "CreateJobForDevices"
    "fixture/CreateJobForDevices.yaml"

requestCreateNodeFromTemplateJob :: CreateNodeFromTemplateJob -> TestTree
requestCreateNodeFromTemplateJob =
  req
    "CreateNodeFromTemplateJob"
    "fixture/CreateNodeFromTemplateJob.yaml"

requestCreatePackage :: CreatePackage -> TestTree
requestCreatePackage =
  req
    "CreatePackage"
    "fixture/CreatePackage.yaml"

requestCreatePackageImportJob :: CreatePackageImportJob -> TestTree
requestCreatePackageImportJob =
  req
    "CreatePackageImportJob"
    "fixture/CreatePackageImportJob.yaml"

requestDeleteDevice :: DeleteDevice -> TestTree
requestDeleteDevice =
  req
    "DeleteDevice"
    "fixture/DeleteDevice.yaml"

requestDeletePackage :: DeletePackage -> TestTree
requestDeletePackage =
  req
    "DeletePackage"
    "fixture/DeletePackage.yaml"

requestDeregisterPackageVersion :: DeregisterPackageVersion -> TestTree
requestDeregisterPackageVersion =
  req
    "DeregisterPackageVersion"
    "fixture/DeregisterPackageVersion.yaml"

requestDescribeApplicationInstance :: DescribeApplicationInstance -> TestTree
requestDescribeApplicationInstance =
  req
    "DescribeApplicationInstance"
    "fixture/DescribeApplicationInstance.yaml"

requestDescribeApplicationInstanceDetails :: DescribeApplicationInstanceDetails -> TestTree
requestDescribeApplicationInstanceDetails =
  req
    "DescribeApplicationInstanceDetails"
    "fixture/DescribeApplicationInstanceDetails.yaml"

requestDescribeDevice :: DescribeDevice -> TestTree
requestDescribeDevice =
  req
    "DescribeDevice"
    "fixture/DescribeDevice.yaml"

requestDescribeDeviceJob :: DescribeDeviceJob -> TestTree
requestDescribeDeviceJob =
  req
    "DescribeDeviceJob"
    "fixture/DescribeDeviceJob.yaml"

requestDescribeNode :: DescribeNode -> TestTree
requestDescribeNode =
  req
    "DescribeNode"
    "fixture/DescribeNode.yaml"

requestDescribeNodeFromTemplateJob :: DescribeNodeFromTemplateJob -> TestTree
requestDescribeNodeFromTemplateJob =
  req
    "DescribeNodeFromTemplateJob"
    "fixture/DescribeNodeFromTemplateJob.yaml"

requestDescribePackage :: DescribePackage -> TestTree
requestDescribePackage =
  req
    "DescribePackage"
    "fixture/DescribePackage.yaml"

requestDescribePackageImportJob :: DescribePackageImportJob -> TestTree
requestDescribePackageImportJob =
  req
    "DescribePackageImportJob"
    "fixture/DescribePackageImportJob.yaml"

requestDescribePackageVersion :: DescribePackageVersion -> TestTree
requestDescribePackageVersion =
  req
    "DescribePackageVersion"
    "fixture/DescribePackageVersion.yaml"

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

requestListApplicationInstances :: ListApplicationInstances -> TestTree
requestListApplicationInstances =
  req
    "ListApplicationInstances"
    "fixture/ListApplicationInstances.yaml"

requestListDevices :: ListDevices -> TestTree
requestListDevices =
  req
    "ListDevices"
    "fixture/ListDevices.yaml"

requestListDevicesJobs :: ListDevicesJobs -> TestTree
requestListDevicesJobs =
  req
    "ListDevicesJobs"
    "fixture/ListDevicesJobs.yaml"

requestListNodeFromTemplateJobs :: ListNodeFromTemplateJobs -> TestTree
requestListNodeFromTemplateJobs =
  req
    "ListNodeFromTemplateJobs"
    "fixture/ListNodeFromTemplateJobs.yaml"

requestListNodes :: ListNodes -> TestTree
requestListNodes =
  req
    "ListNodes"
    "fixture/ListNodes.yaml"

requestListPackageImportJobs :: ListPackageImportJobs -> TestTree
requestListPackageImportJobs =
  req
    "ListPackageImportJobs"
    "fixture/ListPackageImportJobs.yaml"

requestListPackages :: ListPackages -> TestTree
requestListPackages =
  req
    "ListPackages"
    "fixture/ListPackages.yaml"

requestListTagsForResource :: ListTagsForResource -> TestTree
requestListTagsForResource =
  req
    "ListTagsForResource"
    "fixture/ListTagsForResource.yaml"

requestProvisionDevice :: ProvisionDevice -> TestTree
requestProvisionDevice =
  req
    "ProvisionDevice"
    "fixture/ProvisionDevice.yaml"

requestRegisterPackageVersion :: RegisterPackageVersion -> TestTree
requestRegisterPackageVersion =
  req
    "RegisterPackageVersion"
    "fixture/RegisterPackageVersion.yaml"

requestRemoveApplicationInstance :: RemoveApplicationInstance -> TestTree
requestRemoveApplicationInstance =
  req
    "RemoveApplicationInstance"
    "fixture/RemoveApplicationInstance.yaml"

requestSignalApplicationInstanceNodeInstances :: SignalApplicationInstanceNodeInstances -> TestTree
requestSignalApplicationInstanceNodeInstances =
  req
    "SignalApplicationInstanceNodeInstances"
    "fixture/SignalApplicationInstanceNodeInstances.yaml"

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

requestUpdateDeviceMetadata :: UpdateDeviceMetadata -> TestTree
requestUpdateDeviceMetadata =
  req
    "UpdateDeviceMetadata"
    "fixture/UpdateDeviceMetadata.yaml"

-- Responses

responseCreateApplicationInstance :: CreateApplicationInstanceResponse -> TestTree
responseCreateApplicationInstance =
  res
    "CreateApplicationInstanceResponse"
    "fixture/CreateApplicationInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateApplicationInstance)

responseCreateJobForDevices :: CreateJobForDevicesResponse -> TestTree
responseCreateJobForDevices =
  res
    "CreateJobForDevicesResponse"
    "fixture/CreateJobForDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateJobForDevices)

responseCreateNodeFromTemplateJob :: CreateNodeFromTemplateJobResponse -> TestTree
responseCreateNodeFromTemplateJob =
  res
    "CreateNodeFromTemplateJobResponse"
    "fixture/CreateNodeFromTemplateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreateNodeFromTemplateJob)

responseCreatePackage :: CreatePackageResponse -> TestTree
responseCreatePackage =
  res
    "CreatePackageResponse"
    "fixture/CreatePackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePackage)

responseCreatePackageImportJob :: CreatePackageImportJobResponse -> TestTree
responseCreatePackageImportJob =
  res
    "CreatePackageImportJobResponse"
    "fixture/CreatePackageImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy CreatePackageImportJob)

responseDeleteDevice :: DeleteDeviceResponse -> TestTree
responseDeleteDevice =
  res
    "DeleteDeviceResponse"
    "fixture/DeleteDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeleteDevice)

responseDeletePackage :: DeletePackageResponse -> TestTree
responseDeletePackage =
  res
    "DeletePackageResponse"
    "fixture/DeletePackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeletePackage)

responseDeregisterPackageVersion :: DeregisterPackageVersionResponse -> TestTree
responseDeregisterPackageVersion =
  res
    "DeregisterPackageVersionResponse"
    "fixture/DeregisterPackageVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DeregisterPackageVersion)

responseDescribeApplicationInstance :: DescribeApplicationInstanceResponse -> TestTree
responseDescribeApplicationInstance =
  res
    "DescribeApplicationInstanceResponse"
    "fixture/DescribeApplicationInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApplicationInstance)

responseDescribeApplicationInstanceDetails :: DescribeApplicationInstanceDetailsResponse -> TestTree
responseDescribeApplicationInstanceDetails =
  res
    "DescribeApplicationInstanceDetailsResponse"
    "fixture/DescribeApplicationInstanceDetailsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeApplicationInstanceDetails)

responseDescribeDevice :: DescribeDeviceResponse -> TestTree
responseDescribeDevice =
  res
    "DescribeDeviceResponse"
    "fixture/DescribeDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDevice)

responseDescribeDeviceJob :: DescribeDeviceJobResponse -> TestTree
responseDescribeDeviceJob =
  res
    "DescribeDeviceJobResponse"
    "fixture/DescribeDeviceJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeDeviceJob)

responseDescribeNode :: DescribeNodeResponse -> TestTree
responseDescribeNode =
  res
    "DescribeNodeResponse"
    "fixture/DescribeNodeResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNode)

responseDescribeNodeFromTemplateJob :: DescribeNodeFromTemplateJobResponse -> TestTree
responseDescribeNodeFromTemplateJob =
  res
    "DescribeNodeFromTemplateJobResponse"
    "fixture/DescribeNodeFromTemplateJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeNodeFromTemplateJob)

responseDescribePackage :: DescribePackageResponse -> TestTree
responseDescribePackage =
  res
    "DescribePackageResponse"
    "fixture/DescribePackageResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePackage)

responseDescribePackageImportJob :: DescribePackageImportJobResponse -> TestTree
responseDescribePackageImportJob =
  res
    "DescribePackageImportJobResponse"
    "fixture/DescribePackageImportJobResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePackageImportJob)

responseDescribePackageVersion :: DescribePackageVersionResponse -> TestTree
responseDescribePackageVersion =
  res
    "DescribePackageVersionResponse"
    "fixture/DescribePackageVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribePackageVersion)

responseListApplicationInstanceDependencies :: ListApplicationInstanceDependenciesResponse -> TestTree
responseListApplicationInstanceDependencies =
  res
    "ListApplicationInstanceDependenciesResponse"
    "fixture/ListApplicationInstanceDependenciesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplicationInstanceDependencies)

responseListApplicationInstanceNodeInstances :: ListApplicationInstanceNodeInstancesResponse -> TestTree
responseListApplicationInstanceNodeInstances =
  res
    "ListApplicationInstanceNodeInstancesResponse"
    "fixture/ListApplicationInstanceNodeInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplicationInstanceNodeInstances)

responseListApplicationInstances :: ListApplicationInstancesResponse -> TestTree
responseListApplicationInstances =
  res
    "ListApplicationInstancesResponse"
    "fixture/ListApplicationInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListApplicationInstances)

responseListDevices :: ListDevicesResponse -> TestTree
responseListDevices =
  res
    "ListDevicesResponse"
    "fixture/ListDevicesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevices)

responseListDevicesJobs :: ListDevicesJobsResponse -> TestTree
responseListDevicesJobs =
  res
    "ListDevicesJobsResponse"
    "fixture/ListDevicesJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListDevicesJobs)

responseListNodeFromTemplateJobs :: ListNodeFromTemplateJobsResponse -> TestTree
responseListNodeFromTemplateJobs =
  res
    "ListNodeFromTemplateJobsResponse"
    "fixture/ListNodeFromTemplateJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNodeFromTemplateJobs)

responseListNodes :: ListNodesResponse -> TestTree
responseListNodes =
  res
    "ListNodesResponse"
    "fixture/ListNodesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListNodes)

responseListPackageImportJobs :: ListPackageImportJobsResponse -> TestTree
responseListPackageImportJobs =
  res
    "ListPackageImportJobsResponse"
    "fixture/ListPackageImportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPackageImportJobs)

responseListPackages :: ListPackagesResponse -> TestTree
responseListPackages =
  res
    "ListPackagesResponse"
    "fixture/ListPackagesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListPackages)

responseListTagsForResource :: ListTagsForResourceResponse -> TestTree
responseListTagsForResource =
  res
    "ListTagsForResourceResponse"
    "fixture/ListTagsForResourceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ListTagsForResource)

responseProvisionDevice :: ProvisionDeviceResponse -> TestTree
responseProvisionDevice =
  res
    "ProvisionDeviceResponse"
    "fixture/ProvisionDeviceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ProvisionDevice)

responseRegisterPackageVersion :: RegisterPackageVersionResponse -> TestTree
responseRegisterPackageVersion =
  res
    "RegisterPackageVersionResponse"
    "fixture/RegisterPackageVersionResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RegisterPackageVersion)

responseRemoveApplicationInstance :: RemoveApplicationInstanceResponse -> TestTree
responseRemoveApplicationInstance =
  res
    "RemoveApplicationInstanceResponse"
    "fixture/RemoveApplicationInstanceResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy RemoveApplicationInstance)

responseSignalApplicationInstanceNodeInstances :: SignalApplicationInstanceNodeInstancesResponse -> TestTree
responseSignalApplicationInstanceNodeInstances =
  res
    "SignalApplicationInstanceNodeInstancesResponse"
    "fixture/SignalApplicationInstanceNodeInstancesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy SignalApplicationInstanceNodeInstances)

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

responseUpdateDeviceMetadata :: UpdateDeviceMetadataResponse -> TestTree
responseUpdateDeviceMetadata =
  res
    "UpdateDeviceMetadataResponse"
    "fixture/UpdateDeviceMetadataResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateDeviceMetadata)
