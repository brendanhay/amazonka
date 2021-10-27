{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Test.AWS.Gen.ComputeOptimizer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Test.AWS.Gen.ComputeOptimizer where

import Data.Proxy
import Network.AWS.ComputeOptimizer
import Test.AWS.ComputeOptimizer.Internal
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
--         [ requestExportEBSVolumeRecommendations $
--             newExportEBSVolumeRecommendations
--
--         , requestGetRecommendationSummaries $
--             newGetRecommendationSummaries
--
--         , requestExportAutoScalingGroupRecommendations $
--             newExportAutoScalingGroupRecommendations
--
--         , requestGetEC2InstanceRecommendations $
--             newGetEC2InstanceRecommendations
--
--         , requestGetLambdaFunctionRecommendations $
--             newGetLambdaFunctionRecommendations
--
--         , requestUpdateEnrollmentStatus $
--             newUpdateEnrollmentStatus
--
--         , requestDescribeRecommendationExportJobs $
--             newDescribeRecommendationExportJobs
--
--         , requestGetEC2RecommendationProjectedMetrics $
--             newGetEC2RecommendationProjectedMetrics
--
--         , requestGetEnrollmentStatusesForOrganization $
--             newGetEnrollmentStatusesForOrganization
--
--         , requestGetEBSVolumeRecommendations $
--             newGetEBSVolumeRecommendations
--
--         , requestExportLambdaFunctionRecommendations $
--             newExportLambdaFunctionRecommendations
--
--         , requestExportEC2InstanceRecommendations $
--             newExportEC2InstanceRecommendations
--
--         , requestGetEnrollmentStatus $
--             newGetEnrollmentStatus
--
--         , requestGetAutoScalingGroupRecommendations $
--             newGetAutoScalingGroupRecommendations
--
--           ]

--     , testGroup "response"
--         [ responseExportEBSVolumeRecommendations $
--             newExportEBSVolumeRecommendationsResponse
--
--         , responseGetRecommendationSummaries $
--             newGetRecommendationSummariesResponse
--
--         , responseExportAutoScalingGroupRecommendations $
--             newExportAutoScalingGroupRecommendationsResponse
--
--         , responseGetEC2InstanceRecommendations $
--             newGetEC2InstanceRecommendationsResponse
--
--         , responseGetLambdaFunctionRecommendations $
--             newGetLambdaFunctionRecommendationsResponse
--
--         , responseUpdateEnrollmentStatus $
--             newUpdateEnrollmentStatusResponse
--
--         , responseDescribeRecommendationExportJobs $
--             newDescribeRecommendationExportJobsResponse
--
--         , responseGetEC2RecommendationProjectedMetrics $
--             newGetEC2RecommendationProjectedMetricsResponse
--
--         , responseGetEnrollmentStatusesForOrganization $
--             newGetEnrollmentStatusesForOrganizationResponse
--
--         , responseGetEBSVolumeRecommendations $
--             newGetEBSVolumeRecommendationsResponse
--
--         , responseExportLambdaFunctionRecommendations $
--             newExportLambdaFunctionRecommendationsResponse
--
--         , responseExportEC2InstanceRecommendations $
--             newExportEC2InstanceRecommendationsResponse
--
--         , responseGetEnrollmentStatus $
--             newGetEnrollmentStatusResponse
--
--         , responseGetAutoScalingGroupRecommendations $
--             newGetAutoScalingGroupRecommendationsResponse
--
--           ]
--     ]

-- Requests

requestExportEBSVolumeRecommendations :: ExportEBSVolumeRecommendations -> TestTree
requestExportEBSVolumeRecommendations =
  req
    "ExportEBSVolumeRecommendations"
    "fixture/ExportEBSVolumeRecommendations.yaml"

requestGetRecommendationSummaries :: GetRecommendationSummaries -> TestTree
requestGetRecommendationSummaries =
  req
    "GetRecommendationSummaries"
    "fixture/GetRecommendationSummaries.yaml"

requestExportAutoScalingGroupRecommendations :: ExportAutoScalingGroupRecommendations -> TestTree
requestExportAutoScalingGroupRecommendations =
  req
    "ExportAutoScalingGroupRecommendations"
    "fixture/ExportAutoScalingGroupRecommendations.yaml"

requestGetEC2InstanceRecommendations :: GetEC2InstanceRecommendations -> TestTree
requestGetEC2InstanceRecommendations =
  req
    "GetEC2InstanceRecommendations"
    "fixture/GetEC2InstanceRecommendations.yaml"

requestGetLambdaFunctionRecommendations :: GetLambdaFunctionRecommendations -> TestTree
requestGetLambdaFunctionRecommendations =
  req
    "GetLambdaFunctionRecommendations"
    "fixture/GetLambdaFunctionRecommendations.yaml"

requestUpdateEnrollmentStatus :: UpdateEnrollmentStatus -> TestTree
requestUpdateEnrollmentStatus =
  req
    "UpdateEnrollmentStatus"
    "fixture/UpdateEnrollmentStatus.yaml"

requestDescribeRecommendationExportJobs :: DescribeRecommendationExportJobs -> TestTree
requestDescribeRecommendationExportJobs =
  req
    "DescribeRecommendationExportJobs"
    "fixture/DescribeRecommendationExportJobs.yaml"

requestGetEC2RecommendationProjectedMetrics :: GetEC2RecommendationProjectedMetrics -> TestTree
requestGetEC2RecommendationProjectedMetrics =
  req
    "GetEC2RecommendationProjectedMetrics"
    "fixture/GetEC2RecommendationProjectedMetrics.yaml"

requestGetEnrollmentStatusesForOrganization :: GetEnrollmentStatusesForOrganization -> TestTree
requestGetEnrollmentStatusesForOrganization =
  req
    "GetEnrollmentStatusesForOrganization"
    "fixture/GetEnrollmentStatusesForOrganization.yaml"

requestGetEBSVolumeRecommendations :: GetEBSVolumeRecommendations -> TestTree
requestGetEBSVolumeRecommendations =
  req
    "GetEBSVolumeRecommendations"
    "fixture/GetEBSVolumeRecommendations.yaml"

requestExportLambdaFunctionRecommendations :: ExportLambdaFunctionRecommendations -> TestTree
requestExportLambdaFunctionRecommendations =
  req
    "ExportLambdaFunctionRecommendations"
    "fixture/ExportLambdaFunctionRecommendations.yaml"

requestExportEC2InstanceRecommendations :: ExportEC2InstanceRecommendations -> TestTree
requestExportEC2InstanceRecommendations =
  req
    "ExportEC2InstanceRecommendations"
    "fixture/ExportEC2InstanceRecommendations.yaml"

requestGetEnrollmentStatus :: GetEnrollmentStatus -> TestTree
requestGetEnrollmentStatus =
  req
    "GetEnrollmentStatus"
    "fixture/GetEnrollmentStatus.yaml"

requestGetAutoScalingGroupRecommendations :: GetAutoScalingGroupRecommendations -> TestTree
requestGetAutoScalingGroupRecommendations =
  req
    "GetAutoScalingGroupRecommendations"
    "fixture/GetAutoScalingGroupRecommendations.yaml"

-- Responses

responseExportEBSVolumeRecommendations :: ExportEBSVolumeRecommendationsResponse -> TestTree
responseExportEBSVolumeRecommendations =
  res
    "ExportEBSVolumeRecommendationsResponse"
    "fixture/ExportEBSVolumeRecommendationsResponse.proto"
    defaultService
    (Proxy :: Proxy ExportEBSVolumeRecommendations)

responseGetRecommendationSummaries :: GetRecommendationSummariesResponse -> TestTree
responseGetRecommendationSummaries =
  res
    "GetRecommendationSummariesResponse"
    "fixture/GetRecommendationSummariesResponse.proto"
    defaultService
    (Proxy :: Proxy GetRecommendationSummaries)

responseExportAutoScalingGroupRecommendations :: ExportAutoScalingGroupRecommendationsResponse -> TestTree
responseExportAutoScalingGroupRecommendations =
  res
    "ExportAutoScalingGroupRecommendationsResponse"
    "fixture/ExportAutoScalingGroupRecommendationsResponse.proto"
    defaultService
    (Proxy :: Proxy ExportAutoScalingGroupRecommendations)

responseGetEC2InstanceRecommendations :: GetEC2InstanceRecommendationsResponse -> TestTree
responseGetEC2InstanceRecommendations =
  res
    "GetEC2InstanceRecommendationsResponse"
    "fixture/GetEC2InstanceRecommendationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetEC2InstanceRecommendations)

responseGetLambdaFunctionRecommendations :: GetLambdaFunctionRecommendationsResponse -> TestTree
responseGetLambdaFunctionRecommendations =
  res
    "GetLambdaFunctionRecommendationsResponse"
    "fixture/GetLambdaFunctionRecommendationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetLambdaFunctionRecommendations)

responseUpdateEnrollmentStatus :: UpdateEnrollmentStatusResponse -> TestTree
responseUpdateEnrollmentStatus =
  res
    "UpdateEnrollmentStatusResponse"
    "fixture/UpdateEnrollmentStatusResponse.proto"
    defaultService
    (Proxy :: Proxy UpdateEnrollmentStatus)

responseDescribeRecommendationExportJobs :: DescribeRecommendationExportJobsResponse -> TestTree
responseDescribeRecommendationExportJobs =
  res
    "DescribeRecommendationExportJobsResponse"
    "fixture/DescribeRecommendationExportJobsResponse.proto"
    defaultService
    (Proxy :: Proxy DescribeRecommendationExportJobs)

responseGetEC2RecommendationProjectedMetrics :: GetEC2RecommendationProjectedMetricsResponse -> TestTree
responseGetEC2RecommendationProjectedMetrics =
  res
    "GetEC2RecommendationProjectedMetricsResponse"
    "fixture/GetEC2RecommendationProjectedMetricsResponse.proto"
    defaultService
    (Proxy :: Proxy GetEC2RecommendationProjectedMetrics)

responseGetEnrollmentStatusesForOrganization :: GetEnrollmentStatusesForOrganizationResponse -> TestTree
responseGetEnrollmentStatusesForOrganization =
  res
    "GetEnrollmentStatusesForOrganizationResponse"
    "fixture/GetEnrollmentStatusesForOrganizationResponse.proto"
    defaultService
    (Proxy :: Proxy GetEnrollmentStatusesForOrganization)

responseGetEBSVolumeRecommendations :: GetEBSVolumeRecommendationsResponse -> TestTree
responseGetEBSVolumeRecommendations =
  res
    "GetEBSVolumeRecommendationsResponse"
    "fixture/GetEBSVolumeRecommendationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetEBSVolumeRecommendations)

responseExportLambdaFunctionRecommendations :: ExportLambdaFunctionRecommendationsResponse -> TestTree
responseExportLambdaFunctionRecommendations =
  res
    "ExportLambdaFunctionRecommendationsResponse"
    "fixture/ExportLambdaFunctionRecommendationsResponse.proto"
    defaultService
    (Proxy :: Proxy ExportLambdaFunctionRecommendations)

responseExportEC2InstanceRecommendations :: ExportEC2InstanceRecommendationsResponse -> TestTree
responseExportEC2InstanceRecommendations =
  res
    "ExportEC2InstanceRecommendationsResponse"
    "fixture/ExportEC2InstanceRecommendationsResponse.proto"
    defaultService
    (Proxy :: Proxy ExportEC2InstanceRecommendations)

responseGetEnrollmentStatus :: GetEnrollmentStatusResponse -> TestTree
responseGetEnrollmentStatus =
  res
    "GetEnrollmentStatusResponse"
    "fixture/GetEnrollmentStatusResponse.proto"
    defaultService
    (Proxy :: Proxy GetEnrollmentStatus)

responseGetAutoScalingGroupRecommendations :: GetAutoScalingGroupRecommendationsResponse -> TestTree
responseGetAutoScalingGroupRecommendations =
  res
    "GetAutoScalingGroupRecommendationsResponse"
    "fixture/GetAutoScalingGroupRecommendationsResponse.proto"
    defaultService
    (Proxy :: Proxy GetAutoScalingGroupRecommendations)
