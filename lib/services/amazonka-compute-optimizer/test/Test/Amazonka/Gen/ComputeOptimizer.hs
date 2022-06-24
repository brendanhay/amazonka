{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.Amazonka.Gen.ComputeOptimizer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Gen.ComputeOptimizer where

import Amazonka.ComputeOptimizer
import qualified Data.Proxy as Proxy
import Test.Amazonka.ComputeOptimizer.Internal
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
--         [ requestDescribeRecommendationExportJobs $
--             newDescribeRecommendationExportJobs
--
--         , requestExportAutoScalingGroupRecommendations $
--             newExportAutoScalingGroupRecommendations
--
--         , requestExportEBSVolumeRecommendations $
--             newExportEBSVolumeRecommendations
--
--         , requestExportEC2InstanceRecommendations $
--             newExportEC2InstanceRecommendations
--
--         , requestExportLambdaFunctionRecommendations $
--             newExportLambdaFunctionRecommendations
--
--         , requestGetAutoScalingGroupRecommendations $
--             newGetAutoScalingGroupRecommendations
--
--         , requestGetEBSVolumeRecommendations $
--             newGetEBSVolumeRecommendations
--
--         , requestGetEC2InstanceRecommendations $
--             newGetEC2InstanceRecommendations
--
--         , requestGetEC2RecommendationProjectedMetrics $
--             newGetEC2RecommendationProjectedMetrics
--
--         , requestGetEnrollmentStatus $
--             newGetEnrollmentStatus
--
--         , requestGetEnrollmentStatusesForOrganization $
--             newGetEnrollmentStatusesForOrganization
--
--         , requestGetLambdaFunctionRecommendations $
--             newGetLambdaFunctionRecommendations
--
--         , requestGetRecommendationSummaries $
--             newGetRecommendationSummaries
--
--         , requestUpdateEnrollmentStatus $
--             newUpdateEnrollmentStatus
--
--           ]

--     , testGroup "response"
--         [ responseDescribeRecommendationExportJobs $
--             newDescribeRecommendationExportJobsResponse
--
--         , responseExportAutoScalingGroupRecommendations $
--             newExportAutoScalingGroupRecommendationsResponse
--
--         , responseExportEBSVolumeRecommendations $
--             newExportEBSVolumeRecommendationsResponse
--
--         , responseExportEC2InstanceRecommendations $
--             newExportEC2InstanceRecommendationsResponse
--
--         , responseExportLambdaFunctionRecommendations $
--             newExportLambdaFunctionRecommendationsResponse
--
--         , responseGetAutoScalingGroupRecommendations $
--             newGetAutoScalingGroupRecommendationsResponse
--
--         , responseGetEBSVolumeRecommendations $
--             newGetEBSVolumeRecommendationsResponse
--
--         , responseGetEC2InstanceRecommendations $
--             newGetEC2InstanceRecommendationsResponse
--
--         , responseGetEC2RecommendationProjectedMetrics $
--             newGetEC2RecommendationProjectedMetricsResponse
--
--         , responseGetEnrollmentStatus $
--             newGetEnrollmentStatusResponse
--
--         , responseGetEnrollmentStatusesForOrganization $
--             newGetEnrollmentStatusesForOrganizationResponse
--
--         , responseGetLambdaFunctionRecommendations $
--             newGetLambdaFunctionRecommendationsResponse
--
--         , responseGetRecommendationSummaries $
--             newGetRecommendationSummariesResponse
--
--         , responseUpdateEnrollmentStatus $
--             newUpdateEnrollmentStatusResponse
--
--           ]
--     ]

-- Requests

requestDescribeRecommendationExportJobs :: DescribeRecommendationExportJobs -> TestTree
requestDescribeRecommendationExportJobs =
  req
    "DescribeRecommendationExportJobs"
    "fixture/DescribeRecommendationExportJobs.yaml"

requestExportAutoScalingGroupRecommendations :: ExportAutoScalingGroupRecommendations -> TestTree
requestExportAutoScalingGroupRecommendations =
  req
    "ExportAutoScalingGroupRecommendations"
    "fixture/ExportAutoScalingGroupRecommendations.yaml"

requestExportEBSVolumeRecommendations :: ExportEBSVolumeRecommendations -> TestTree
requestExportEBSVolumeRecommendations =
  req
    "ExportEBSVolumeRecommendations"
    "fixture/ExportEBSVolumeRecommendations.yaml"

requestExportEC2InstanceRecommendations :: ExportEC2InstanceRecommendations -> TestTree
requestExportEC2InstanceRecommendations =
  req
    "ExportEC2InstanceRecommendations"
    "fixture/ExportEC2InstanceRecommendations.yaml"

requestExportLambdaFunctionRecommendations :: ExportLambdaFunctionRecommendations -> TestTree
requestExportLambdaFunctionRecommendations =
  req
    "ExportLambdaFunctionRecommendations"
    "fixture/ExportLambdaFunctionRecommendations.yaml"

requestGetAutoScalingGroupRecommendations :: GetAutoScalingGroupRecommendations -> TestTree
requestGetAutoScalingGroupRecommendations =
  req
    "GetAutoScalingGroupRecommendations"
    "fixture/GetAutoScalingGroupRecommendations.yaml"

requestGetEBSVolumeRecommendations :: GetEBSVolumeRecommendations -> TestTree
requestGetEBSVolumeRecommendations =
  req
    "GetEBSVolumeRecommendations"
    "fixture/GetEBSVolumeRecommendations.yaml"

requestGetEC2InstanceRecommendations :: GetEC2InstanceRecommendations -> TestTree
requestGetEC2InstanceRecommendations =
  req
    "GetEC2InstanceRecommendations"
    "fixture/GetEC2InstanceRecommendations.yaml"

requestGetEC2RecommendationProjectedMetrics :: GetEC2RecommendationProjectedMetrics -> TestTree
requestGetEC2RecommendationProjectedMetrics =
  req
    "GetEC2RecommendationProjectedMetrics"
    "fixture/GetEC2RecommendationProjectedMetrics.yaml"

requestGetEnrollmentStatus :: GetEnrollmentStatus -> TestTree
requestGetEnrollmentStatus =
  req
    "GetEnrollmentStatus"
    "fixture/GetEnrollmentStatus.yaml"

requestGetEnrollmentStatusesForOrganization :: GetEnrollmentStatusesForOrganization -> TestTree
requestGetEnrollmentStatusesForOrganization =
  req
    "GetEnrollmentStatusesForOrganization"
    "fixture/GetEnrollmentStatusesForOrganization.yaml"

requestGetLambdaFunctionRecommendations :: GetLambdaFunctionRecommendations -> TestTree
requestGetLambdaFunctionRecommendations =
  req
    "GetLambdaFunctionRecommendations"
    "fixture/GetLambdaFunctionRecommendations.yaml"

requestGetRecommendationSummaries :: GetRecommendationSummaries -> TestTree
requestGetRecommendationSummaries =
  req
    "GetRecommendationSummaries"
    "fixture/GetRecommendationSummaries.yaml"

requestUpdateEnrollmentStatus :: UpdateEnrollmentStatus -> TestTree
requestUpdateEnrollmentStatus =
  req
    "UpdateEnrollmentStatus"
    "fixture/UpdateEnrollmentStatus.yaml"

-- Responses

responseDescribeRecommendationExportJobs :: DescribeRecommendationExportJobsResponse -> TestTree
responseDescribeRecommendationExportJobs =
  res
    "DescribeRecommendationExportJobsResponse"
    "fixture/DescribeRecommendationExportJobsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy DescribeRecommendationExportJobs)

responseExportAutoScalingGroupRecommendations :: ExportAutoScalingGroupRecommendationsResponse -> TestTree
responseExportAutoScalingGroupRecommendations =
  res
    "ExportAutoScalingGroupRecommendationsResponse"
    "fixture/ExportAutoScalingGroupRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportAutoScalingGroupRecommendations)

responseExportEBSVolumeRecommendations :: ExportEBSVolumeRecommendationsResponse -> TestTree
responseExportEBSVolumeRecommendations =
  res
    "ExportEBSVolumeRecommendationsResponse"
    "fixture/ExportEBSVolumeRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportEBSVolumeRecommendations)

responseExportEC2InstanceRecommendations :: ExportEC2InstanceRecommendationsResponse -> TestTree
responseExportEC2InstanceRecommendations =
  res
    "ExportEC2InstanceRecommendationsResponse"
    "fixture/ExportEC2InstanceRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportEC2InstanceRecommendations)

responseExportLambdaFunctionRecommendations :: ExportLambdaFunctionRecommendationsResponse -> TestTree
responseExportLambdaFunctionRecommendations =
  res
    "ExportLambdaFunctionRecommendationsResponse"
    "fixture/ExportLambdaFunctionRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy ExportLambdaFunctionRecommendations)

responseGetAutoScalingGroupRecommendations :: GetAutoScalingGroupRecommendationsResponse -> TestTree
responseGetAutoScalingGroupRecommendations =
  res
    "GetAutoScalingGroupRecommendationsResponse"
    "fixture/GetAutoScalingGroupRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetAutoScalingGroupRecommendations)

responseGetEBSVolumeRecommendations :: GetEBSVolumeRecommendationsResponse -> TestTree
responseGetEBSVolumeRecommendations =
  res
    "GetEBSVolumeRecommendationsResponse"
    "fixture/GetEBSVolumeRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEBSVolumeRecommendations)

responseGetEC2InstanceRecommendations :: GetEC2InstanceRecommendationsResponse -> TestTree
responseGetEC2InstanceRecommendations =
  res
    "GetEC2InstanceRecommendationsResponse"
    "fixture/GetEC2InstanceRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEC2InstanceRecommendations)

responseGetEC2RecommendationProjectedMetrics :: GetEC2RecommendationProjectedMetricsResponse -> TestTree
responseGetEC2RecommendationProjectedMetrics =
  res
    "GetEC2RecommendationProjectedMetricsResponse"
    "fixture/GetEC2RecommendationProjectedMetricsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEC2RecommendationProjectedMetrics)

responseGetEnrollmentStatus :: GetEnrollmentStatusResponse -> TestTree
responseGetEnrollmentStatus =
  res
    "GetEnrollmentStatusResponse"
    "fixture/GetEnrollmentStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEnrollmentStatus)

responseGetEnrollmentStatusesForOrganization :: GetEnrollmentStatusesForOrganizationResponse -> TestTree
responseGetEnrollmentStatusesForOrganization =
  res
    "GetEnrollmentStatusesForOrganizationResponse"
    "fixture/GetEnrollmentStatusesForOrganizationResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetEnrollmentStatusesForOrganization)

responseGetLambdaFunctionRecommendations :: GetLambdaFunctionRecommendationsResponse -> TestTree
responseGetLambdaFunctionRecommendations =
  res
    "GetLambdaFunctionRecommendationsResponse"
    "fixture/GetLambdaFunctionRecommendationsResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetLambdaFunctionRecommendations)

responseGetRecommendationSummaries :: GetRecommendationSummariesResponse -> TestTree
responseGetRecommendationSummaries =
  res
    "GetRecommendationSummariesResponse"
    "fixture/GetRecommendationSummariesResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy GetRecommendationSummaries)

responseUpdateEnrollmentStatus :: UpdateEnrollmentStatusResponse -> TestTree
responseUpdateEnrollmentStatus =
  res
    "UpdateEnrollmentStatusResponse"
    "fixture/UpdateEnrollmentStatusResponse.proto"
    defaultService
    (Proxy.Proxy :: Proxy.Proxy UpdateEnrollmentStatus)
