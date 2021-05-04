{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Lens
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Lens
  ( -- * Operations

    -- ** DisassociateTrialComponent
    disassociateTrialComponent_trialComponentName,
    disassociateTrialComponent_trialName,
    disassociateTrialComponentResponse_trialArn,
    disassociateTrialComponentResponse_trialComponentArn,
    disassociateTrialComponentResponse_httpStatus,

    -- ** DeleteArtifact
    deleteArtifact_artifactArn,
    deleteArtifact_source,
    deleteArtifactResponse_artifactArn,
    deleteArtifactResponse_httpStatus,

    -- ** CreateTransformJob
    createTransformJob_experimentConfig,
    createTransformJob_maxConcurrentTransforms,
    createTransformJob_environment,
    createTransformJob_maxPayloadInMB,
    createTransformJob_batchStrategy,
    createTransformJob_modelClientConfig,
    createTransformJob_tags,
    createTransformJob_dataProcessing,
    createTransformJob_transformJobName,
    createTransformJob_modelName,
    createTransformJob_transformInput,
    createTransformJob_transformOutput,
    createTransformJob_transformResources,
    createTransformJobResponse_httpStatus,
    createTransformJobResponse_transformJobArn,

    -- ** ListHumanTaskUis
    listHumanTaskUis_sortOrder,
    listHumanTaskUis_nextToken,
    listHumanTaskUis_maxResults,
    listHumanTaskUis_creationTimeBefore,
    listHumanTaskUis_creationTimeAfter,
    listHumanTaskUisResponse_nextToken,
    listHumanTaskUisResponse_httpStatus,
    listHumanTaskUisResponse_humanTaskUiSummaries,

    -- ** DeleteHumanTaskUi
    deleteHumanTaskUi_humanTaskUiName,
    deleteHumanTaskUiResponse_httpStatus,

    -- ** UpdateAction
    updateAction_status,
    updateAction_propertiesToRemove,
    updateAction_properties,
    updateAction_description,
    updateAction_actionName,
    updateActionResponse_actionArn,
    updateActionResponse_httpStatus,

    -- ** DescribePipeline
    describePipeline_pipelineName,
    describePipelineResponse_pipelineArn,
    describePipelineResponse_pipelineDescription,
    describePipelineResponse_creationTime,
    describePipelineResponse_roleArn,
    describePipelineResponse_lastRunTime,
    describePipelineResponse_pipelineDefinition,
    describePipelineResponse_pipelineDisplayName,
    describePipelineResponse_lastModifiedTime,
    describePipelineResponse_pipelineStatus,
    describePipelineResponse_createdBy,
    describePipelineResponse_lastModifiedBy,
    describePipelineResponse_pipelineName,
    describePipelineResponse_httpStatus,

    -- ** UpdateArtifact
    updateArtifact_propertiesToRemove,
    updateArtifact_artifactName,
    updateArtifact_properties,
    updateArtifact_artifactArn,
    updateArtifactResponse_artifactArn,
    updateArtifactResponse_httpStatus,

    -- ** DescribeUserProfile
    describeUserProfile_domainId,
    describeUserProfile_userProfileName,
    describeUserProfileResponse_status,
    describeUserProfileResponse_creationTime,
    describeUserProfileResponse_userSettings,
    describeUserProfileResponse_userProfileName,
    describeUserProfileResponse_domainId,
    describeUserProfileResponse_userProfileArn,
    describeUserProfileResponse_failureReason,
    describeUserProfileResponse_homeEfsFileSystemUid,
    describeUserProfileResponse_lastModifiedTime,
    describeUserProfileResponse_singleSignOnUserIdentifier,
    describeUserProfileResponse_singleSignOnUserValue,
    describeUserProfileResponse_httpStatus,

    -- ** StopTrainingJob
    stopTrainingJob_trainingJobName,

    -- ** CreateEndpoint
    createEndpoint_tags,
    createEndpoint_endpointName,
    createEndpoint_endpointConfigName,
    createEndpointResponse_httpStatus,
    createEndpointResponse_endpointArn,

    -- ** GetSearchSuggestions
    getSearchSuggestions_suggestionQuery,
    getSearchSuggestions_resource,
    getSearchSuggestionsResponse_propertyNameSuggestions,
    getSearchSuggestionsResponse_httpStatus,

    -- ** DeleteAction
    deleteAction_actionName,
    deleteActionResponse_actionArn,
    deleteActionResponse_httpStatus,

    -- ** CreateEdgePackagingJob
    createEdgePackagingJob_resourceKey,
    createEdgePackagingJob_tags,
    createEdgePackagingJob_edgePackagingJobName,
    createEdgePackagingJob_compilationJobName,
    createEdgePackagingJob_modelName,
    createEdgePackagingJob_modelVersion,
    createEdgePackagingJob_roleArn,
    createEdgePackagingJob_outputConfig,

    -- ** DescribeEndpointConfig
    describeEndpointConfig_endpointConfigName,
    describeEndpointConfigResponse_kmsKeyId,
    describeEndpointConfigResponse_dataCaptureConfig,
    describeEndpointConfigResponse_httpStatus,
    describeEndpointConfigResponse_endpointConfigName,
    describeEndpointConfigResponse_endpointConfigArn,
    describeEndpointConfigResponse_productionVariants,
    describeEndpointConfigResponse_creationTime,

    -- ** ListModelPackages
    listModelPackages_sortOrder,
    listModelPackages_nextToken,
    listModelPackages_nameContains,
    listModelPackages_maxResults,
    listModelPackages_creationTimeBefore,
    listModelPackages_modelApprovalStatus,
    listModelPackages_sortBy,
    listModelPackages_creationTimeAfter,
    listModelPackages_modelPackageGroupName,
    listModelPackages_modelPackageType,
    listModelPackagesResponse_nextToken,
    listModelPackagesResponse_httpStatus,
    listModelPackagesResponse_modelPackageSummaryList,

    -- ** GetModelPackageGroupPolicy
    getModelPackageGroupPolicy_modelPackageGroupName,
    getModelPackageGroupPolicyResponse_httpStatus,
    getModelPackageGroupPolicyResponse_resourcePolicy,

    -- ** DescribeMonitoringSchedule
    describeMonitoringSchedule_monitoringScheduleName,
    describeMonitoringScheduleResponse_endpointName,
    describeMonitoringScheduleResponse_monitoringType,
    describeMonitoringScheduleResponse_failureReason,
    describeMonitoringScheduleResponse_lastMonitoringExecutionSummary,
    describeMonitoringScheduleResponse_httpStatus,
    describeMonitoringScheduleResponse_monitoringScheduleArn,
    describeMonitoringScheduleResponse_monitoringScheduleName,
    describeMonitoringScheduleResponse_monitoringScheduleStatus,
    describeMonitoringScheduleResponse_creationTime,
    describeMonitoringScheduleResponse_lastModifiedTime,
    describeMonitoringScheduleResponse_monitoringScheduleConfig,

    -- ** CreateModelExplainabilityJobDefinition
    createModelExplainabilityJobDefinition_networkConfig,
    createModelExplainabilityJobDefinition_modelExplainabilityBaselineConfig,
    createModelExplainabilityJobDefinition_tags,
    createModelExplainabilityJobDefinition_stoppingCondition,
    createModelExplainabilityJobDefinition_jobDefinitionName,
    createModelExplainabilityJobDefinition_modelExplainabilityAppSpecification,
    createModelExplainabilityJobDefinition_modelExplainabilityJobInput,
    createModelExplainabilityJobDefinition_modelExplainabilityJobOutputConfig,
    createModelExplainabilityJobDefinition_jobResources,
    createModelExplainabilityJobDefinition_roleArn,
    createModelExplainabilityJobDefinitionResponse_httpStatus,
    createModelExplainabilityJobDefinitionResponse_jobDefinitionArn,

    -- ** DescribeLabelingJob
    describeLabelingJob_labelingJobName,
    describeLabelingJobResponse_stoppingConditions,
    describeLabelingJobResponse_labelAttributeName,
    describeLabelingJobResponse_labelCategoryConfigS3Uri,
    describeLabelingJobResponse_labelingJobAlgorithmsConfig,
    describeLabelingJobResponse_failureReason,
    describeLabelingJobResponse_tags,
    describeLabelingJobResponse_labelingJobOutput,
    describeLabelingJobResponse_httpStatus,
    describeLabelingJobResponse_labelingJobStatus,
    describeLabelingJobResponse_labelCounters,
    describeLabelingJobResponse_creationTime,
    describeLabelingJobResponse_lastModifiedTime,
    describeLabelingJobResponse_jobReferenceCode,
    describeLabelingJobResponse_labelingJobName,
    describeLabelingJobResponse_labelingJobArn,
    describeLabelingJobResponse_inputConfig,
    describeLabelingJobResponse_outputConfig,
    describeLabelingJobResponse_roleArn,
    describeLabelingJobResponse_humanTaskConfig,

    -- ** CreateNotebookInstance
    createNotebookInstance_securityGroupIds,
    createNotebookInstance_acceleratorTypes,
    createNotebookInstance_defaultCodeRepository,
    createNotebookInstance_additionalCodeRepositories,
    createNotebookInstance_kmsKeyId,
    createNotebookInstance_volumeSizeInGB,
    createNotebookInstance_tags,
    createNotebookInstance_subnetId,
    createNotebookInstance_lifecycleConfigName,
    createNotebookInstance_directInternetAccess,
    createNotebookInstance_rootAccess,
    createNotebookInstance_notebookInstanceName,
    createNotebookInstance_instanceType,
    createNotebookInstance_roleArn,
    createNotebookInstanceResponse_notebookInstanceArn,
    createNotebookInstanceResponse_httpStatus,

    -- ** UpdateModelPackage
    updateModelPackage_approvalDescription,
    updateModelPackage_modelPackageArn,
    updateModelPackage_modelApprovalStatus,
    updateModelPackageResponse_httpStatus,
    updateModelPackageResponse_modelPackageArn,

    -- ** CreateModelQualityJobDefinition
    createModelQualityJobDefinition_networkConfig,
    createModelQualityJobDefinition_modelQualityBaselineConfig,
    createModelQualityJobDefinition_tags,
    createModelQualityJobDefinition_stoppingCondition,
    createModelQualityJobDefinition_jobDefinitionName,
    createModelQualityJobDefinition_modelQualityAppSpecification,
    createModelQualityJobDefinition_modelQualityJobInput,
    createModelQualityJobDefinition_modelQualityJobOutputConfig,
    createModelQualityJobDefinition_jobResources,
    createModelQualityJobDefinition_roleArn,
    createModelQualityJobDefinitionResponse_httpStatus,
    createModelQualityJobDefinitionResponse_jobDefinitionArn,

    -- ** DeleteModelPackage
    deleteModelPackage_modelPackageName,

    -- ** ListProjects
    listProjects_sortOrder,
    listProjects_nextToken,
    listProjects_nameContains,
    listProjects_maxResults,
    listProjects_creationTimeBefore,
    listProjects_sortBy,
    listProjects_creationTimeAfter,
    listProjectsResponse_nextToken,
    listProjectsResponse_httpStatus,
    listProjectsResponse_projectSummaryList,

    -- ** ListSubscribedWorkteams
    listSubscribedWorkteams_nextToken,
    listSubscribedWorkteams_nameContains,
    listSubscribedWorkteams_maxResults,
    listSubscribedWorkteamsResponse_nextToken,
    listSubscribedWorkteamsResponse_httpStatus,
    listSubscribedWorkteamsResponse_subscribedWorkteams,

    -- ** DeleteNotebookInstance
    deleteNotebookInstance_notebookInstanceName,

    -- ** CreateProject
    createProject_tags,
    createProject_projectDescription,
    createProject_projectName,
    createProject_serviceCatalogProvisioningDetails,
    createProjectResponse_httpStatus,
    createProjectResponse_projectArn,
    createProjectResponse_projectId,

    -- ** DescribeProcessingJob
    describeProcessingJob_processingJobName,
    describeProcessingJobResponse_networkConfig,
    describeProcessingJobResponse_processingEndTime,
    describeProcessingJobResponse_roleArn,
    describeProcessingJobResponse_processingOutputConfig,
    describeProcessingJobResponse_exitMessage,
    describeProcessingJobResponse_experimentConfig,
    describeProcessingJobResponse_environment,
    describeProcessingJobResponse_autoMLJobArn,
    describeProcessingJobResponse_failureReason,
    describeProcessingJobResponse_monitoringScheduleArn,
    describeProcessingJobResponse_lastModifiedTime,
    describeProcessingJobResponse_processingInputs,
    describeProcessingJobResponse_processingStartTime,
    describeProcessingJobResponse_stoppingCondition,
    describeProcessingJobResponse_trainingJobArn,
    describeProcessingJobResponse_httpStatus,
    describeProcessingJobResponse_processingJobName,
    describeProcessingJobResponse_processingResources,
    describeProcessingJobResponse_appSpecification,
    describeProcessingJobResponse_processingJobArn,
    describeProcessingJobResponse_processingJobStatus,
    describeProcessingJobResponse_creationTime,

    -- ** ListDomains
    listDomains_nextToken,
    listDomains_maxResults,
    listDomainsResponse_nextToken,
    listDomainsResponse_domains,
    listDomainsResponse_httpStatus,

    -- ** DeleteModelExplainabilityJobDefinition
    deleteModelExplainabilityJobDefinition_jobDefinitionName,

    -- ** StopMonitoringSchedule
    stopMonitoringSchedule_monitoringScheduleName,

    -- ** ListDevices
    listDevices_latestHeartbeatAfter,
    listDevices_nextToken,
    listDevices_deviceFleetName,
    listDevices_maxResults,
    listDevices_modelName,
    listDevicesResponse_nextToken,
    listDevicesResponse_httpStatus,
    listDevicesResponse_deviceSummaries,

    -- ** CreateModelPackage
    createModelPackage_sourceAlgorithmSpecification,
    createModelPackage_metadataProperties,
    createModelPackage_validationSpecification,
    createModelPackage_modelMetrics,
    createModelPackage_certifyForMarketplace,
    createModelPackage_modelPackageName,
    createModelPackage_modelApprovalStatus,
    createModelPackage_tags,
    createModelPackage_inferenceSpecification,
    createModelPackage_modelPackageDescription,
    createModelPackage_modelPackageGroupName,
    createModelPackage_clientToken,
    createModelPackageResponse_httpStatus,
    createModelPackageResponse_modelPackageArn,

    -- ** UpdateNotebookInstance
    updateNotebookInstance_acceleratorTypes,
    updateNotebookInstance_defaultCodeRepository,
    updateNotebookInstance_roleArn,
    updateNotebookInstance_instanceType,
    updateNotebookInstance_disassociateDefaultCodeRepository,
    updateNotebookInstance_disassociateAcceleratorTypes,
    updateNotebookInstance_disassociateLifecycleConfig,
    updateNotebookInstance_additionalCodeRepositories,
    updateNotebookInstance_disassociateAdditionalCodeRepositories,
    updateNotebookInstance_volumeSizeInGB,
    updateNotebookInstance_lifecycleConfigName,
    updateNotebookInstance_rootAccess,
    updateNotebookInstance_notebookInstanceName,
    updateNotebookInstanceResponse_httpStatus,

    -- ** StopAutoMLJob
    stopAutoMLJob_autoMLJobName,

    -- ** DescribeAppImageConfig
    describeAppImageConfig_appImageConfigName,
    describeAppImageConfigResponse_creationTime,
    describeAppImageConfigResponse_appImageConfigArn,
    describeAppImageConfigResponse_kernelGatewayImageConfig,
    describeAppImageConfigResponse_appImageConfigName,
    describeAppImageConfigResponse_lastModifiedTime,
    describeAppImageConfigResponse_httpStatus,

    -- ** StartMonitoringSchedule
    startMonitoringSchedule_monitoringScheduleName,

    -- ** StopCompilationJob
    stopCompilationJob_compilationJobName,

    -- ** CreateTrial
    createTrial_metadataProperties,
    createTrial_tags,
    createTrial_displayName,
    createTrial_trialName,
    createTrial_experimentName,
    createTrialResponse_trialArn,
    createTrialResponse_httpStatus,

    -- ** GetSagemakerServicecatalogPortfolioStatus
    getSagemakerServicecatalogPortfolioStatusResponse_status,
    getSagemakerServicecatalogPortfolioStatusResponse_httpStatus,

    -- ** UpdateCodeRepository
    updateCodeRepository_gitConfig,
    updateCodeRepository_codeRepositoryName,
    updateCodeRepositoryResponse_httpStatus,
    updateCodeRepositoryResponse_codeRepositoryArn,

    -- ** Search
    search_sortOrder,
    search_nextToken,
    search_maxResults,
    search_searchExpression,
    search_sortBy,
    search_resource,
    searchResponse_nextToken,
    searchResponse_results,
    searchResponse_httpStatus,

    -- ** DeleteModel
    deleteModel_modelName,

    -- ** DeleteDataQualityJobDefinition
    deleteDataQualityJobDefinition_jobDefinitionName,

    -- ** ListImages
    listImages_lastModifiedTimeBefore,
    listImages_sortOrder,
    listImages_nextToken,
    listImages_nameContains,
    listImages_maxResults,
    listImages_creationTimeBefore,
    listImages_lastModifiedTimeAfter,
    listImages_sortBy,
    listImages_creationTimeAfter,
    listImagesResponse_nextToken,
    listImagesResponse_images,
    listImagesResponse_httpStatus,

    -- ** ListTrainingJobs
    listTrainingJobs_lastModifiedTimeBefore,
    listTrainingJobs_sortOrder,
    listTrainingJobs_nextToken,
    listTrainingJobs_nameContains,
    listTrainingJobs_maxResults,
    listTrainingJobs_creationTimeBefore,
    listTrainingJobs_lastModifiedTimeAfter,
    listTrainingJobs_sortBy,
    listTrainingJobs_statusEquals,
    listTrainingJobs_creationTimeAfter,
    listTrainingJobsResponse_nextToken,
    listTrainingJobsResponse_httpStatus,
    listTrainingJobsResponse_trainingJobSummaries,

    -- ** DescribeTransformJob
    describeTransformJob_transformJobName,
    describeTransformJobResponse_labelingJobArn,
    describeTransformJobResponse_transformStartTime,
    describeTransformJobResponse_transformOutput,
    describeTransformJobResponse_experimentConfig,
    describeTransformJobResponse_maxConcurrentTransforms,
    describeTransformJobResponse_environment,
    describeTransformJobResponse_maxPayloadInMB,
    describeTransformJobResponse_batchStrategy,
    describeTransformJobResponse_autoMLJobArn,
    describeTransformJobResponse_failureReason,
    describeTransformJobResponse_modelClientConfig,
    describeTransformJobResponse_transformEndTime,
    describeTransformJobResponse_dataProcessing,
    describeTransformJobResponse_httpStatus,
    describeTransformJobResponse_transformJobName,
    describeTransformJobResponse_transformJobArn,
    describeTransformJobResponse_transformJobStatus,
    describeTransformJobResponse_modelName,
    describeTransformJobResponse_transformInput,
    describeTransformJobResponse_transformResources,
    describeTransformJobResponse_creationTime,

    -- ** CreatePipeline
    createPipeline_pipelineDescription,
    createPipeline_pipelineDisplayName,
    createPipeline_tags,
    createPipeline_pipelineName,
    createPipeline_pipelineDefinition,
    createPipeline_clientRequestToken,
    createPipeline_roleArn,
    createPipelineResponse_pipelineArn,
    createPipelineResponse_httpStatus,

    -- ** CreateModelPackageGroup
    createModelPackageGroup_modelPackageGroupDescription,
    createModelPackageGroup_tags,
    createModelPackageGroup_modelPackageGroupName,
    createModelPackageGroupResponse_httpStatus,
    createModelPackageGroupResponse_modelPackageGroupArn,

    -- ** ListCandidatesForAutoMLJob
    listCandidatesForAutoMLJob_sortOrder,
    listCandidatesForAutoMLJob_nextToken,
    listCandidatesForAutoMLJob_maxResults,
    listCandidatesForAutoMLJob_candidateNameEquals,
    listCandidatesForAutoMLJob_sortBy,
    listCandidatesForAutoMLJob_statusEquals,
    listCandidatesForAutoMLJob_autoMLJobName,
    listCandidatesForAutoMLJobResponse_nextToken,
    listCandidatesForAutoMLJobResponse_httpStatus,
    listCandidatesForAutoMLJobResponse_candidates,

    -- ** DeleteAlgorithm
    deleteAlgorithm_algorithmName,

    -- ** GetDeviceFleetReport
    getDeviceFleetReport_deviceFleetName,
    getDeviceFleetReportResponse_modelStats,
    getDeviceFleetReportResponse_outputConfig,
    getDeviceFleetReportResponse_reportGenerated,
    getDeviceFleetReportResponse_deviceStats,
    getDeviceFleetReportResponse_description,
    getDeviceFleetReportResponse_agentVersions,
    getDeviceFleetReportResponse_httpStatus,
    getDeviceFleetReportResponse_deviceFleetArn,
    getDeviceFleetReportResponse_deviceFleetName,

    -- ** ListDataQualityJobDefinitions
    listDataQualityJobDefinitions_sortOrder,
    listDataQualityJobDefinitions_nextToken,
    listDataQualityJobDefinitions_endpointName,
    listDataQualityJobDefinitions_nameContains,
    listDataQualityJobDefinitions_maxResults,
    listDataQualityJobDefinitions_creationTimeBefore,
    listDataQualityJobDefinitions_sortBy,
    listDataQualityJobDefinitions_creationTimeAfter,
    listDataQualityJobDefinitionsResponse_nextToken,
    listDataQualityJobDefinitionsResponse_httpStatus,
    listDataQualityJobDefinitionsResponse_jobDefinitionSummaries,

    -- ** DescribeEdgePackagingJob
    describeEdgePackagingJob_edgePackagingJobName,
    describeEdgePackagingJobResponse_edgePackagingJobStatusMessage,
    describeEdgePackagingJobResponse_creationTime,
    describeEdgePackagingJobResponse_roleArn,
    describeEdgePackagingJobResponse_compilationJobName,
    describeEdgePackagingJobResponse_modelSignature,
    describeEdgePackagingJobResponse_resourceKey,
    describeEdgePackagingJobResponse_modelVersion,
    describeEdgePackagingJobResponse_outputConfig,
    describeEdgePackagingJobResponse_modelArtifact,
    describeEdgePackagingJobResponse_lastModifiedTime,
    describeEdgePackagingJobResponse_modelName,
    describeEdgePackagingJobResponse_httpStatus,
    describeEdgePackagingJobResponse_edgePackagingJobArn,
    describeEdgePackagingJobResponse_edgePackagingJobName,
    describeEdgePackagingJobResponse_edgePackagingJobStatus,

    -- ** ListContexts
    listContexts_contextType,
    listContexts_createdAfter,
    listContexts_sortOrder,
    listContexts_nextToken,
    listContexts_createdBefore,
    listContexts_maxResults,
    listContexts_sourceUri,
    listContexts_sortBy,
    listContextsResponse_nextToken,
    listContextsResponse_contextSummaries,
    listContextsResponse_httpStatus,

    -- ** DescribeEndpoint
    describeEndpoint_endpointName,
    describeEndpointResponse_productionVariants,
    describeEndpointResponse_lastDeploymentConfig,
    describeEndpointResponse_failureReason,
    describeEndpointResponse_dataCaptureConfig,
    describeEndpointResponse_httpStatus,
    describeEndpointResponse_endpointName,
    describeEndpointResponse_endpointArn,
    describeEndpointResponse_endpointConfigName,
    describeEndpointResponse_endpointStatus,
    describeEndpointResponse_creationTime,
    describeEndpointResponse_lastModifiedTime,

    -- ** DeleteCodeRepository
    deleteCodeRepository_codeRepositoryName,

    -- ** DeleteModelPackageGroupPolicy
    deleteModelPackageGroupPolicy_modelPackageGroupName,

    -- ** ListUserProfiles
    listUserProfiles_sortOrder,
    listUserProfiles_nextToken,
    listUserProfiles_userProfileNameContains,
    listUserProfiles_maxResults,
    listUserProfiles_domainIdEquals,
    listUserProfiles_sortBy,
    listUserProfilesResponse_nextToken,
    listUserProfilesResponse_userProfiles,
    listUserProfilesResponse_httpStatus,

    -- ** DescribeCompilationJob
    describeCompilationJob_compilationJobName,
    describeCompilationJobResponse_modelDigests,
    describeCompilationJobResponse_compilationStartTime,
    describeCompilationJobResponse_compilationEndTime,
    describeCompilationJobResponse_httpStatus,
    describeCompilationJobResponse_compilationJobName,
    describeCompilationJobResponse_compilationJobArn,
    describeCompilationJobResponse_compilationJobStatus,
    describeCompilationJobResponse_stoppingCondition,
    describeCompilationJobResponse_creationTime,
    describeCompilationJobResponse_lastModifiedTime,
    describeCompilationJobResponse_failureReason,
    describeCompilationJobResponse_modelArtifacts,
    describeCompilationJobResponse_roleArn,
    describeCompilationJobResponse_inputConfig,
    describeCompilationJobResponse_outputConfig,

    -- ** UpdatePipeline
    updatePipeline_pipelineDescription,
    updatePipeline_roleArn,
    updatePipeline_pipelineDefinition,
    updatePipeline_pipelineDisplayName,
    updatePipeline_pipelineName,
    updatePipelineResponse_pipelineArn,
    updatePipelineResponse_httpStatus,

    -- ** CreateCodeRepository
    createCodeRepository_tags,
    createCodeRepository_codeRepositoryName,
    createCodeRepository_gitConfig,
    createCodeRepositoryResponse_httpStatus,
    createCodeRepositoryResponse_codeRepositoryArn,

    -- ** DescribeArtifact
    describeArtifact_artifactArn,
    describeArtifactResponse_metadataProperties,
    describeArtifactResponse_creationTime,
    describeArtifactResponse_artifactName,
    describeArtifactResponse_artifactType,
    describeArtifactResponse_artifactArn,
    describeArtifactResponse_source,
    describeArtifactResponse_properties,
    describeArtifactResponse_lastModifiedTime,
    describeArtifactResponse_createdBy,
    describeArtifactResponse_lastModifiedBy,
    describeArtifactResponse_httpStatus,

    -- ** DescribeHumanTaskUi
    describeHumanTaskUi_humanTaskUiName,
    describeHumanTaskUiResponse_humanTaskUiStatus,
    describeHumanTaskUiResponse_httpStatus,
    describeHumanTaskUiResponse_humanTaskUiArn,
    describeHumanTaskUiResponse_humanTaskUiName,
    describeHumanTaskUiResponse_creationTime,
    describeHumanTaskUiResponse_uiTemplate,

    -- ** ListPipelineExecutionSteps
    listPipelineExecutionSteps_sortOrder,
    listPipelineExecutionSteps_nextToken,
    listPipelineExecutionSteps_maxResults,
    listPipelineExecutionSteps_pipelineExecutionArn,
    listPipelineExecutionStepsResponse_nextToken,
    listPipelineExecutionStepsResponse_pipelineExecutionSteps,
    listPipelineExecutionStepsResponse_httpStatus,

    -- ** ListCodeRepositories
    listCodeRepositories_lastModifiedTimeBefore,
    listCodeRepositories_sortOrder,
    listCodeRepositories_nextToken,
    listCodeRepositories_nameContains,
    listCodeRepositories_maxResults,
    listCodeRepositories_creationTimeBefore,
    listCodeRepositories_lastModifiedTimeAfter,
    listCodeRepositories_sortBy,
    listCodeRepositories_creationTimeAfter,
    listCodeRepositoriesResponse_nextToken,
    listCodeRepositoriesResponse_httpStatus,
    listCodeRepositoriesResponse_codeRepositorySummaryList,

    -- ** UpdateUserProfile
    updateUserProfile_userSettings,
    updateUserProfile_domainId,
    updateUserProfile_userProfileName,
    updateUserProfileResponse_userProfileArn,
    updateUserProfileResponse_httpStatus,

    -- ** DescribeAction
    describeAction_actionName,
    describeActionResponse_status,
    describeActionResponse_metadataProperties,
    describeActionResponse_creationTime,
    describeActionResponse_actionName,
    describeActionResponse_actionType,
    describeActionResponse_actionArn,
    describeActionResponse_source,
    describeActionResponse_properties,
    describeActionResponse_lastModifiedTime,
    describeActionResponse_description,
    describeActionResponse_createdBy,
    describeActionResponse_lastModifiedBy,
    describeActionResponse_httpStatus,

    -- ** StopTransformJob
    stopTransformJob_transformJobName,

    -- ** CreateTrainingJob
    createTrainingJob_vpcConfig,
    createTrainingJob_debugRuleConfigurations,
    createTrainingJob_inputDataConfig,
    createTrainingJob_hyperParameters,
    createTrainingJob_enableManagedSpotTraining,
    createTrainingJob_profilerConfig,
    createTrainingJob_experimentConfig,
    createTrainingJob_enableNetworkIsolation,
    createTrainingJob_enableInterContainerTrafficEncryption,
    createTrainingJob_checkpointConfig,
    createTrainingJob_profilerRuleConfigurations,
    createTrainingJob_tags,
    createTrainingJob_tensorBoardOutputConfig,
    createTrainingJob_debugHookConfig,
    createTrainingJob_trainingJobName,
    createTrainingJob_algorithmSpecification,
    createTrainingJob_roleArn,
    createTrainingJob_outputDataConfig,
    createTrainingJob_resourceConfig,
    createTrainingJob_stoppingCondition,
    createTrainingJobResponse_httpStatus,
    createTrainingJobResponse_trainingJobArn,

    -- ** DeleteUserProfile
    deleteUserProfile_domainId,
    deleteUserProfile_userProfileName,

    -- ** CreateContext
    createContext_tags,
    createContext_properties,
    createContext_description,
    createContext_contextName,
    createContext_source,
    createContext_contextType,
    createContextResponse_contextArn,
    createContextResponse_httpStatus,

    -- ** StopEdgePackagingJob
    stopEdgePackagingJob_edgePackagingJobName,

    -- ** CreateImage
    createImage_tags,
    createImage_description,
    createImage_displayName,
    createImage_imageName,
    createImage_roleArn,
    createImageResponse_imageArn,
    createImageResponse_httpStatus,

    -- ** DeregisterDevices
    deregisterDevices_deviceFleetName,
    deregisterDevices_deviceNames,

    -- ** CreateDataQualityJobDefinition
    createDataQualityJobDefinition_networkConfig,
    createDataQualityJobDefinition_dataQualityBaselineConfig,
    createDataQualityJobDefinition_tags,
    createDataQualityJobDefinition_stoppingCondition,
    createDataQualityJobDefinition_jobDefinitionName,
    createDataQualityJobDefinition_dataQualityAppSpecification,
    createDataQualityJobDefinition_dataQualityJobInput,
    createDataQualityJobDefinition_dataQualityJobOutputConfig,
    createDataQualityJobDefinition_jobResources,
    createDataQualityJobDefinition_roleArn,
    createDataQualityJobDefinitionResponse_httpStatus,
    createDataQualityJobDefinitionResponse_jobDefinitionArn,

    -- ** DeletePipeline
    deletePipeline_pipelineName,
    deletePipeline_clientRequestToken,
    deletePipelineResponse_pipelineArn,
    deletePipelineResponse_httpStatus,

    -- ** CreateAppImageConfig
    createAppImageConfig_kernelGatewayImageConfig,
    createAppImageConfig_tags,
    createAppImageConfig_appImageConfigName,
    createAppImageConfigResponse_appImageConfigArn,
    createAppImageConfigResponse_httpStatus,

    -- ** AddTags
    addTags_resourceArn,
    addTags_tags,
    addTagsResponse_tags,
    addTagsResponse_httpStatus,

    -- ** DisableSagemakerServicecatalogPortfolio
    disableSagemakerServicecatalogPortfolioResponse_httpStatus,

    -- ** DeleteAssociation
    deleteAssociation_sourceArn,
    deleteAssociation_destinationArn,
    deleteAssociationResponse_destinationArn,
    deleteAssociationResponse_sourceArn,
    deleteAssociationResponse_httpStatus,

    -- ** UpdateMonitoringSchedule
    updateMonitoringSchedule_monitoringScheduleName,
    updateMonitoringSchedule_monitoringScheduleConfig,
    updateMonitoringScheduleResponse_httpStatus,
    updateMonitoringScheduleResponse_monitoringScheduleArn,

    -- ** ListMonitoringSchedules
    listMonitoringSchedules_lastModifiedTimeBefore,
    listMonitoringSchedules_sortOrder,
    listMonitoringSchedules_nextToken,
    listMonitoringSchedules_endpointName,
    listMonitoringSchedules_nameContains,
    listMonitoringSchedules_monitoringJobDefinitionName,
    listMonitoringSchedules_maxResults,
    listMonitoringSchedules_creationTimeBefore,
    listMonitoringSchedules_lastModifiedTimeAfter,
    listMonitoringSchedules_sortBy,
    listMonitoringSchedules_statusEquals,
    listMonitoringSchedules_monitoringTypeEquals,
    listMonitoringSchedules_creationTimeAfter,
    listMonitoringSchedulesResponse_nextToken,
    listMonitoringSchedulesResponse_httpStatus,
    listMonitoringSchedulesResponse_monitoringScheduleSummaries,

    -- ** StopNotebookInstance
    stopNotebookInstance_notebookInstanceName,

    -- ** DeleteMonitoringSchedule
    deleteMonitoringSchedule_monitoringScheduleName,

    -- ** DeleteEndpointConfig
    deleteEndpointConfig_endpointConfigName,

    -- ** StartPipelineExecution
    startPipelineExecution_pipelineExecutionDescription,
    startPipelineExecution_pipelineParameters,
    startPipelineExecution_pipelineExecutionDisplayName,
    startPipelineExecution_pipelineName,
    startPipelineExecution_clientRequestToken,
    startPipelineExecutionResponse_pipelineExecutionArn,
    startPipelineExecutionResponse_httpStatus,

    -- ** DescribeModelPackage
    describeModelPackage_modelPackageName,
    describeModelPackageResponse_sourceAlgorithmSpecification,
    describeModelPackageResponse_modelPackageVersion,
    describeModelPackageResponse_metadataProperties,
    describeModelPackageResponse_validationSpecification,
    describeModelPackageResponse_modelMetrics,
    describeModelPackageResponse_certifyForMarketplace,
    describeModelPackageResponse_modelApprovalStatus,
    describeModelPackageResponse_approvalDescription,
    describeModelPackageResponse_lastModifiedTime,
    describeModelPackageResponse_inferenceSpecification,
    describeModelPackageResponse_modelPackageDescription,
    describeModelPackageResponse_createdBy,
    describeModelPackageResponse_lastModifiedBy,
    describeModelPackageResponse_modelPackageGroupName,
    describeModelPackageResponse_httpStatus,
    describeModelPackageResponse_modelPackageName,
    describeModelPackageResponse_modelPackageArn,
    describeModelPackageResponse_creationTime,
    describeModelPackageResponse_modelPackageStatus,
    describeModelPackageResponse_modelPackageStatusDetails,

    -- ** DeleteTags
    deleteTags_resourceArn,
    deleteTags_tagKeys,
    deleteTagsResponse_httpStatus,

    -- ** AddAssociation
    addAssociation_associationType,
    addAssociation_sourceArn,
    addAssociation_destinationArn,
    addAssociationResponse_destinationArn,
    addAssociationResponse_sourceArn,
    addAssociationResponse_httpStatus,

    -- ** CreateNotebookInstanceLifecycleConfig
    createNotebookInstanceLifecycleConfig_onStart,
    createNotebookInstanceLifecycleConfig_onCreate,
    createNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,
    createNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn,
    createNotebookInstanceLifecycleConfigResponse_httpStatus,

    -- ** ListApps
    listApps_sortOrder,
    listApps_nextToken,
    listApps_maxResults,
    listApps_domainIdEquals,
    listApps_userProfileNameEquals,
    listApps_sortBy,
    listAppsResponse_nextToken,
    listAppsResponse_apps,
    listAppsResponse_httpStatus,

    -- ** CreateWorkforce
    createWorkforce_tags,
    createWorkforce_sourceIpConfig,
    createWorkforce_oidcConfig,
    createWorkforce_cognitoConfig,
    createWorkforce_workforceName,
    createWorkforceResponse_httpStatus,
    createWorkforceResponse_workforceArn,

    -- ** ListAutoMLJobs
    listAutoMLJobs_lastModifiedTimeBefore,
    listAutoMLJobs_sortOrder,
    listAutoMLJobs_nextToken,
    listAutoMLJobs_nameContains,
    listAutoMLJobs_maxResults,
    listAutoMLJobs_creationTimeBefore,
    listAutoMLJobs_lastModifiedTimeAfter,
    listAutoMLJobs_sortBy,
    listAutoMLJobs_statusEquals,
    listAutoMLJobs_creationTimeAfter,
    listAutoMLJobsResponse_nextToken,
    listAutoMLJobsResponse_httpStatus,
    listAutoMLJobsResponse_autoMLJobSummaries,

    -- ** UpdateEndpointWeightsAndCapacities
    updateEndpointWeightsAndCapacities_endpointName,
    updateEndpointWeightsAndCapacities_desiredWeightsAndCapacities,
    updateEndpointWeightsAndCapacitiesResponse_httpStatus,
    updateEndpointWeightsAndCapacitiesResponse_endpointArn,

    -- ** StartNotebookInstance
    startNotebookInstance_notebookInstanceName,

    -- ** StopPipelineExecution
    stopPipelineExecution_pipelineExecutionArn,
    stopPipelineExecution_clientRequestToken,
    stopPipelineExecutionResponse_pipelineExecutionArn,
    stopPipelineExecutionResponse_httpStatus,

    -- ** ListEndpointConfigs
    listEndpointConfigs_sortOrder,
    listEndpointConfigs_nextToken,
    listEndpointConfigs_nameContains,
    listEndpointConfigs_maxResults,
    listEndpointConfigs_creationTimeBefore,
    listEndpointConfigs_sortBy,
    listEndpointConfigs_creationTimeAfter,
    listEndpointConfigsResponse_nextToken,
    listEndpointConfigsResponse_httpStatus,
    listEndpointConfigsResponse_endpointConfigs,

    -- ** DeleteWorkteam
    deleteWorkteam_workteamName,
    deleteWorkteamResponse_httpStatus,
    deleteWorkteamResponse_success,

    -- ** DeleteWorkforce
    deleteWorkforce_workforceName,
    deleteWorkforceResponse_httpStatus,

    -- ** DeleteModelBiasJobDefinition
    deleteModelBiasJobDefinition_jobDefinitionName,

    -- ** UpdateWorkforce
    updateWorkforce_sourceIpConfig,
    updateWorkforce_oidcConfig,
    updateWorkforce_workforceName,
    updateWorkforceResponse_httpStatus,
    updateWorkforceResponse_workforce,

    -- ** DescribeDevice
    describeDevice_nextToken,
    describeDevice_deviceName,
    describeDevice_deviceFleetName,
    describeDeviceResponse_nextToken,
    describeDeviceResponse_latestHeartbeat,
    describeDeviceResponse_maxModels,
    describeDeviceResponse_deviceArn,
    describeDeviceResponse_models,
    describeDeviceResponse_iotThingName,
    describeDeviceResponse_description,
    describeDeviceResponse_httpStatus,
    describeDeviceResponse_deviceName,
    describeDeviceResponse_deviceFleetName,
    describeDeviceResponse_registrationTime,

    -- ** DescribeDomain
    describeDomain_domainId,
    describeDomainResponse_status,
    describeDomainResponse_creationTime,
    describeDomainResponse_singleSignOnManagedApplicationInstanceId,
    describeDomainResponse_authMode,
    describeDomainResponse_subnetIds,
    describeDomainResponse_domainId,
    describeDomainResponse_domainArn,
    describeDomainResponse_kmsKeyId,
    describeDomainResponse_domainName,
    describeDomainResponse_defaultUserSettings,
    describeDomainResponse_failureReason,
    describeDomainResponse_homeEfsFileSystemId,
    describeDomainResponse_lastModifiedTime,
    describeDomainResponse_appNetworkAccessType,
    describeDomainResponse_homeEfsFileSystemKmsKeyId,
    describeDomainResponse_url,
    describeDomainResponse_vpcId,
    describeDomainResponse_httpStatus,

    -- ** DeleteNotebookInstanceLifecycleConfig
    deleteNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,

    -- ** DescribePipelineExecution
    describePipelineExecution_pipelineExecutionArn,
    describePipelineExecutionResponse_pipelineArn,
    describePipelineExecutionResponse_creationTime,
    describePipelineExecutionResponse_pipelineExecutionDescription,
    describePipelineExecutionResponse_pipelineExecutionDisplayName,
    describePipelineExecutionResponse_pipelineExecutionStatus,
    describePipelineExecutionResponse_lastModifiedTime,
    describePipelineExecutionResponse_createdBy,
    describePipelineExecutionResponse_lastModifiedBy,
    describePipelineExecutionResponse_pipelineExecutionArn,
    describePipelineExecutionResponse_httpStatus,

    -- ** UpdateWorkteam
    updateWorkteam_memberDefinitions,
    updateWorkteam_notificationConfiguration,
    updateWorkteam_description,
    updateWorkteam_workteamName,
    updateWorkteamResponse_httpStatus,
    updateWorkteamResponse_workteam,

    -- ** CreateLabelingJob
    createLabelingJob_stoppingConditions,
    createLabelingJob_labelCategoryConfigS3Uri,
    createLabelingJob_labelingJobAlgorithmsConfig,
    createLabelingJob_tags,
    createLabelingJob_labelingJobName,
    createLabelingJob_labelAttributeName,
    createLabelingJob_inputConfig,
    createLabelingJob_outputConfig,
    createLabelingJob_roleArn,
    createLabelingJob_humanTaskConfig,
    createLabelingJobResponse_httpStatus,
    createLabelingJobResponse_labelingJobArn,

    -- ** DescribeModelQualityJobDefinition
    describeModelQualityJobDefinition_jobDefinitionName,
    describeModelQualityJobDefinitionResponse_networkConfig,
    describeModelQualityJobDefinitionResponse_modelQualityBaselineConfig,
    describeModelQualityJobDefinitionResponse_stoppingCondition,
    describeModelQualityJobDefinitionResponse_httpStatus,
    describeModelQualityJobDefinitionResponse_jobDefinitionArn,
    describeModelQualityJobDefinitionResponse_jobDefinitionName,
    describeModelQualityJobDefinitionResponse_creationTime,
    describeModelQualityJobDefinitionResponse_modelQualityAppSpecification,
    describeModelQualityJobDefinitionResponse_modelQualityJobInput,
    describeModelQualityJobDefinitionResponse_modelQualityJobOutputConfig,
    describeModelQualityJobDefinitionResponse_jobResources,
    describeModelQualityJobDefinitionResponse_roleArn,

    -- ** CreateExperiment
    createExperiment_tags,
    createExperiment_description,
    createExperiment_displayName,
    createExperiment_experimentName,
    createExperimentResponse_experimentArn,
    createExperimentResponse_httpStatus,

    -- ** ListWorkforces
    listWorkforces_sortOrder,
    listWorkforces_nextToken,
    listWorkforces_nameContains,
    listWorkforces_maxResults,
    listWorkforces_sortBy,
    listWorkforcesResponse_nextToken,
    listWorkforcesResponse_httpStatus,
    listWorkforcesResponse_workforces,

    -- ** ListAppImageConfigs
    listAppImageConfigs_sortOrder,
    listAppImageConfigs_nextToken,
    listAppImageConfigs_nameContains,
    listAppImageConfigs_maxResults,
    listAppImageConfigs_modifiedTimeBefore,
    listAppImageConfigs_creationTimeBefore,
    listAppImageConfigs_sortBy,
    listAppImageConfigs_creationTimeAfter,
    listAppImageConfigs_modifiedTimeAfter,
    listAppImageConfigsResponse_nextToken,
    listAppImageConfigsResponse_appImageConfigs,
    listAppImageConfigsResponse_httpStatus,

    -- ** UpdateNotebookInstanceLifecycleConfig
    updateNotebookInstanceLifecycleConfig_onStart,
    updateNotebookInstanceLifecycleConfig_onCreate,
    updateNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,
    updateNotebookInstanceLifecycleConfigResponse_httpStatus,

    -- ** DescribeSubscribedWorkteam
    describeSubscribedWorkteam_workteamArn,
    describeSubscribedWorkteamResponse_httpStatus,
    describeSubscribedWorkteamResponse_subscribedWorkteam,

    -- ** ListNotebookInstanceLifecycleConfigs
    listNotebookInstanceLifecycleConfigs_lastModifiedTimeBefore,
    listNotebookInstanceLifecycleConfigs_sortOrder,
    listNotebookInstanceLifecycleConfigs_nextToken,
    listNotebookInstanceLifecycleConfigs_nameContains,
    listNotebookInstanceLifecycleConfigs_maxResults,
    listNotebookInstanceLifecycleConfigs_creationTimeBefore,
    listNotebookInstanceLifecycleConfigs_lastModifiedTimeAfter,
    listNotebookInstanceLifecycleConfigs_sortBy,
    listNotebookInstanceLifecycleConfigs_creationTimeAfter,
    listNotebookInstanceLifecycleConfigsResponse_nextToken,
    listNotebookInstanceLifecycleConfigsResponse_notebookInstanceLifecycleConfigs,
    listNotebookInstanceLifecycleConfigsResponse_httpStatus,

    -- ** ListEdgePackagingJobs
    listEdgePackagingJobs_lastModifiedTimeBefore,
    listEdgePackagingJobs_sortOrder,
    listEdgePackagingJobs_nextToken,
    listEdgePackagingJobs_nameContains,
    listEdgePackagingJobs_maxResults,
    listEdgePackagingJobs_modelNameContains,
    listEdgePackagingJobs_creationTimeBefore,
    listEdgePackagingJobs_lastModifiedTimeAfter,
    listEdgePackagingJobs_sortBy,
    listEdgePackagingJobs_statusEquals,
    listEdgePackagingJobs_creationTimeAfter,
    listEdgePackagingJobsResponse_nextToken,
    listEdgePackagingJobsResponse_httpStatus,
    listEdgePackagingJobsResponse_edgePackagingJobSummaries,

    -- ** DescribeCodeRepository
    describeCodeRepository_codeRepositoryName,
    describeCodeRepositoryResponse_gitConfig,
    describeCodeRepositoryResponse_httpStatus,
    describeCodeRepositoryResponse_codeRepositoryName,
    describeCodeRepositoryResponse_codeRepositoryArn,
    describeCodeRepositoryResponse_creationTime,
    describeCodeRepositoryResponse_lastModifiedTime,

    -- ** ListEndpoints
    listEndpoints_lastModifiedTimeBefore,
    listEndpoints_sortOrder,
    listEndpoints_nextToken,
    listEndpoints_nameContains,
    listEndpoints_maxResults,
    listEndpoints_creationTimeBefore,
    listEndpoints_lastModifiedTimeAfter,
    listEndpoints_sortBy,
    listEndpoints_statusEquals,
    listEndpoints_creationTimeAfter,
    listEndpointsResponse_nextToken,
    listEndpointsResponse_httpStatus,
    listEndpointsResponse_endpoints,

    -- ** DescribeDataQualityJobDefinition
    describeDataQualityJobDefinition_jobDefinitionName,
    describeDataQualityJobDefinitionResponse_networkConfig,
    describeDataQualityJobDefinitionResponse_dataQualityBaselineConfig,
    describeDataQualityJobDefinitionResponse_stoppingCondition,
    describeDataQualityJobDefinitionResponse_httpStatus,
    describeDataQualityJobDefinitionResponse_jobDefinitionArn,
    describeDataQualityJobDefinitionResponse_jobDefinitionName,
    describeDataQualityJobDefinitionResponse_creationTime,
    describeDataQualityJobDefinitionResponse_dataQualityAppSpecification,
    describeDataQualityJobDefinitionResponse_dataQualityJobInput,
    describeDataQualityJobDefinitionResponse_dataQualityJobOutputConfig,
    describeDataQualityJobDefinitionResponse_jobResources,
    describeDataQualityJobDefinitionResponse_roleArn,

    -- ** DescribeAlgorithm
    describeAlgorithm_algorithmName,
    describeAlgorithmResponse_algorithmDescription,
    describeAlgorithmResponse_validationSpecification,
    describeAlgorithmResponse_certifyForMarketplace,
    describeAlgorithmResponse_productId,
    describeAlgorithmResponse_inferenceSpecification,
    describeAlgorithmResponse_httpStatus,
    describeAlgorithmResponse_algorithmName,
    describeAlgorithmResponse_algorithmArn,
    describeAlgorithmResponse_creationTime,
    describeAlgorithmResponse_trainingSpecification,
    describeAlgorithmResponse_algorithmStatus,
    describeAlgorithmResponse_algorithmStatusDetails,

    -- ** CreateAction
    createAction_status,
    createAction_metadataProperties,
    createAction_tags,
    createAction_properties,
    createAction_description,
    createAction_actionName,
    createAction_source,
    createAction_actionType,
    createActionResponse_actionArn,
    createActionResponse_httpStatus,

    -- ** DeleteEndpoint
    deleteEndpoint_endpointName,

    -- ** CreatePresignedDomainUrl
    createPresignedDomainUrl_sessionExpirationDurationInSeconds,
    createPresignedDomainUrl_expiresInSeconds,
    createPresignedDomainUrl_domainId,
    createPresignedDomainUrl_userProfileName,
    createPresignedDomainUrlResponse_authorizedUrl,
    createPresignedDomainUrlResponse_httpStatus,

    -- ** ListTransformJobs
    listTransformJobs_lastModifiedTimeBefore,
    listTransformJobs_sortOrder,
    listTransformJobs_nextToken,
    listTransformJobs_nameContains,
    listTransformJobs_maxResults,
    listTransformJobs_creationTimeBefore,
    listTransformJobs_lastModifiedTimeAfter,
    listTransformJobs_sortBy,
    listTransformJobs_statusEquals,
    listTransformJobs_creationTimeAfter,
    listTransformJobsResponse_nextToken,
    listTransformJobsResponse_httpStatus,
    listTransformJobsResponse_transformJobSummaries,

    -- ** DescribeHyperParameterTuningJob
    describeHyperParameterTuningJob_hyperParameterTuningJobName,
    describeHyperParameterTuningJobResponse_bestTrainingJob,
    describeHyperParameterTuningJobResponse_warmStartConfig,
    describeHyperParameterTuningJobResponse_hyperParameterTuningEndTime,
    describeHyperParameterTuningJobResponse_failureReason,
    describeHyperParameterTuningJobResponse_trainingJobDefinitions,
    describeHyperParameterTuningJobResponse_lastModifiedTime,
    describeHyperParameterTuningJobResponse_overallBestTrainingJob,
    describeHyperParameterTuningJobResponse_trainingJobDefinition,
    describeHyperParameterTuningJobResponse_httpStatus,
    describeHyperParameterTuningJobResponse_hyperParameterTuningJobName,
    describeHyperParameterTuningJobResponse_hyperParameterTuningJobArn,
    describeHyperParameterTuningJobResponse_hyperParameterTuningJobConfig,
    describeHyperParameterTuningJobResponse_hyperParameterTuningJobStatus,
    describeHyperParameterTuningJobResponse_creationTime,
    describeHyperParameterTuningJobResponse_trainingJobStatusCounters,
    describeHyperParameterTuningJobResponse_objectiveStatusCounters,

    -- ** CreateCompilationJob
    createCompilationJob_tags,
    createCompilationJob_compilationJobName,
    createCompilationJob_roleArn,
    createCompilationJob_inputConfig,
    createCompilationJob_outputConfig,
    createCompilationJob_stoppingCondition,
    createCompilationJobResponse_httpStatus,
    createCompilationJobResponse_compilationJobArn,

    -- ** UpdateEndpoint
    updateEndpoint_excludeRetainedVariantProperties,
    updateEndpoint_retainAllVariantProperties,
    updateEndpoint_deploymentConfig,
    updateEndpoint_endpointName,
    updateEndpoint_endpointConfigName,
    updateEndpointResponse_httpStatus,
    updateEndpointResponse_endpointArn,

    -- ** DescribeModel
    describeModel_modelName,
    describeModelResponse_vpcConfig,
    describeModelResponse_primaryContainer,
    describeModelResponse_enableNetworkIsolation,
    describeModelResponse_containers,
    describeModelResponse_inferenceExecutionConfig,
    describeModelResponse_httpStatus,
    describeModelResponse_modelName,
    describeModelResponse_executionRoleArn,
    describeModelResponse_creationTime,
    describeModelResponse_modelArn,

    -- ** CreateDeviceFleet
    createDeviceFleet_roleArn,
    createDeviceFleet_tags,
    createDeviceFleet_description,
    createDeviceFleet_deviceFleetName,
    createDeviceFleet_outputConfig,

    -- ** CreateArtifact
    createArtifact_metadataProperties,
    createArtifact_artifactName,
    createArtifact_tags,
    createArtifact_properties,
    createArtifact_source,
    createArtifact_artifactType,
    createArtifactResponse_artifactArn,
    createArtifactResponse_httpStatus,

    -- ** UpdateDevices
    updateDevices_deviceFleetName,
    updateDevices_devices,

    -- ** ListArtifacts
    listArtifacts_createdAfter,
    listArtifacts_sortOrder,
    listArtifacts_nextToken,
    listArtifacts_createdBefore,
    listArtifacts_artifactType,
    listArtifacts_maxResults,
    listArtifacts_sourceUri,
    listArtifacts_sortBy,
    listArtifactsResponse_nextToken,
    listArtifactsResponse_artifactSummaries,
    listArtifactsResponse_httpStatus,

    -- ** DeleteDeviceFleet
    deleteDeviceFleet_deviceFleetName,

    -- ** ListMonitoringExecutions
    listMonitoringExecutions_lastModifiedTimeBefore,
    listMonitoringExecutions_sortOrder,
    listMonitoringExecutions_nextToken,
    listMonitoringExecutions_endpointName,
    listMonitoringExecutions_monitoringJobDefinitionName,
    listMonitoringExecutions_monitoringScheduleName,
    listMonitoringExecutions_maxResults,
    listMonitoringExecutions_scheduledTimeAfter,
    listMonitoringExecutions_creationTimeBefore,
    listMonitoringExecutions_lastModifiedTimeAfter,
    listMonitoringExecutions_sortBy,
    listMonitoringExecutions_statusEquals,
    listMonitoringExecutions_monitoringTypeEquals,
    listMonitoringExecutions_creationTimeAfter,
    listMonitoringExecutions_scheduledTimeBefore,
    listMonitoringExecutionsResponse_nextToken,
    listMonitoringExecutionsResponse_httpStatus,
    listMonitoringExecutionsResponse_monitoringExecutionSummaries,

    -- ** ListCompilationJobs
    listCompilationJobs_lastModifiedTimeBefore,
    listCompilationJobs_sortOrder,
    listCompilationJobs_nextToken,
    listCompilationJobs_nameContains,
    listCompilationJobs_maxResults,
    listCompilationJobs_creationTimeBefore,
    listCompilationJobs_lastModifiedTimeAfter,
    listCompilationJobs_sortBy,
    listCompilationJobs_statusEquals,
    listCompilationJobs_creationTimeAfter,
    listCompilationJobsResponse_nextToken,
    listCompilationJobsResponse_httpStatus,
    listCompilationJobsResponse_compilationJobSummaries,

    -- ** ListActions
    listActions_createdAfter,
    listActions_sortOrder,
    listActions_nextToken,
    listActions_createdBefore,
    listActions_actionType,
    listActions_maxResults,
    listActions_sourceUri,
    listActions_sortBy,
    listActionsResponse_nextToken,
    listActionsResponse_actionSummaries,
    listActionsResponse_httpStatus,

    -- ** ListDeviceFleets
    listDeviceFleets_lastModifiedTimeBefore,
    listDeviceFleets_sortOrder,
    listDeviceFleets_nextToken,
    listDeviceFleets_nameContains,
    listDeviceFleets_maxResults,
    listDeviceFleets_creationTimeBefore,
    listDeviceFleets_lastModifiedTimeAfter,
    listDeviceFleets_sortBy,
    listDeviceFleets_creationTimeAfter,
    listDeviceFleetsResponse_nextToken,
    listDeviceFleetsResponse_httpStatus,
    listDeviceFleetsResponse_deviceFleetSummaries,

    -- ** DescribeModelPackageGroup
    describeModelPackageGroup_modelPackageGroupName,
    describeModelPackageGroupResponse_modelPackageGroupDescription,
    describeModelPackageGroupResponse_httpStatus,
    describeModelPackageGroupResponse_modelPackageGroupName,
    describeModelPackageGroupResponse_modelPackageGroupArn,
    describeModelPackageGroupResponse_creationTime,
    describeModelPackageGroupResponse_createdBy,
    describeModelPackageGroupResponse_modelPackageGroupStatus,

    -- ** StopHyperParameterTuningJob
    stopHyperParameterTuningJob_hyperParameterTuningJobName,

    -- ** DescribeTrial
    describeTrial_trialName,
    describeTrialResponse_trialArn,
    describeTrialResponse_metadataProperties,
    describeTrialResponse_creationTime,
    describeTrialResponse_source,
    describeTrialResponse_lastModifiedTime,
    describeTrialResponse_experimentName,
    describeTrialResponse_createdBy,
    describeTrialResponse_lastModifiedBy,
    describeTrialResponse_displayName,
    describeTrialResponse_trialName,
    describeTrialResponse_httpStatus,

    -- ** UpdateDeviceFleet
    updateDeviceFleet_roleArn,
    updateDeviceFleet_description,
    updateDeviceFleet_deviceFleetName,
    updateDeviceFleet_outputConfig,

    -- ** ListLabelingJobsForWorkteam
    listLabelingJobsForWorkteam_sortOrder,
    listLabelingJobsForWorkteam_nextToken,
    listLabelingJobsForWorkteam_maxResults,
    listLabelingJobsForWorkteam_creationTimeBefore,
    listLabelingJobsForWorkteam_jobReferenceCodeContains,
    listLabelingJobsForWorkteam_sortBy,
    listLabelingJobsForWorkteam_creationTimeAfter,
    listLabelingJobsForWorkteam_workteamArn,
    listLabelingJobsForWorkteamResponse_nextToken,
    listLabelingJobsForWorkteamResponse_httpStatus,
    listLabelingJobsForWorkteamResponse_labelingJobSummaryList,

    -- ** CreateFeatureGroup
    createFeatureGroup_offlineStoreConfig,
    createFeatureGroup_roleArn,
    createFeatureGroup_tags,
    createFeatureGroup_description,
    createFeatureGroup_onlineStoreConfig,
    createFeatureGroup_featureGroupName,
    createFeatureGroup_recordIdentifierFeatureName,
    createFeatureGroup_eventTimeFeatureName,
    createFeatureGroup_featureDefinitions,
    createFeatureGroupResponse_httpStatus,
    createFeatureGroupResponse_featureGroupArn,

    -- ** CreateDomain
    createDomain_kmsKeyId,
    createDomain_tags,
    createDomain_appNetworkAccessType,
    createDomain_homeEfsFileSystemKmsKeyId,
    createDomain_domainName,
    createDomain_authMode,
    createDomain_defaultUserSettings,
    createDomain_subnetIds,
    createDomain_vpcId,
    createDomainResponse_domainArn,
    createDomainResponse_url,
    createDomainResponse_httpStatus,

    -- ** ListImageVersions
    listImageVersions_lastModifiedTimeBefore,
    listImageVersions_sortOrder,
    listImageVersions_nextToken,
    listImageVersions_maxResults,
    listImageVersions_creationTimeBefore,
    listImageVersions_lastModifiedTimeAfter,
    listImageVersions_sortBy,
    listImageVersions_creationTimeAfter,
    listImageVersions_imageName,
    listImageVersionsResponse_nextToken,
    listImageVersionsResponse_imageVersions,
    listImageVersionsResponse_httpStatus,

    -- ** StopProcessingJob
    stopProcessingJob_processingJobName,

    -- ** DeleteImageVersion
    deleteImageVersion_imageName,
    deleteImageVersion_version,
    deleteImageVersionResponse_httpStatus,

    -- ** DeleteProject
    deleteProject_projectName,

    -- ** DescribeExperiment
    describeExperiment_experimentName,
    describeExperimentResponse_experimentArn,
    describeExperimentResponse_creationTime,
    describeExperimentResponse_source,
    describeExperimentResponse_lastModifiedTime,
    describeExperimentResponse_experimentName,
    describeExperimentResponse_description,
    describeExperimentResponse_createdBy,
    describeExperimentResponse_lastModifiedBy,
    describeExperimentResponse_displayName,
    describeExperimentResponse_httpStatus,

    -- ** DescribeAutoMLJob
    describeAutoMLJob_autoMLJobName,
    describeAutoMLJobResponse_generateCandidateDefinitionsOnly,
    describeAutoMLJobResponse_endTime,
    describeAutoMLJobResponse_resolvedAttributes,
    describeAutoMLJobResponse_autoMLJobArtifacts,
    describeAutoMLJobResponse_failureReason,
    describeAutoMLJobResponse_autoMLJobObjective,
    describeAutoMLJobResponse_autoMLJobConfig,
    describeAutoMLJobResponse_problemType,
    describeAutoMLJobResponse_bestCandidate,
    describeAutoMLJobResponse_httpStatus,
    describeAutoMLJobResponse_autoMLJobName,
    describeAutoMLJobResponse_autoMLJobArn,
    describeAutoMLJobResponse_inputDataConfig,
    describeAutoMLJobResponse_outputDataConfig,
    describeAutoMLJobResponse_roleArn,
    describeAutoMLJobResponse_creationTime,
    describeAutoMLJobResponse_lastModifiedTime,
    describeAutoMLJobResponse_autoMLJobStatus,
    describeAutoMLJobResponse_autoMLJobSecondaryStatus,

    -- ** DescribeApp
    describeApp_domainId,
    describeApp_userProfileName,
    describeApp_appType,
    describeApp_appName,
    describeAppResponse_resourceSpec,
    describeAppResponse_status,
    describeAppResponse_creationTime,
    describeAppResponse_appType,
    describeAppResponse_appName,
    describeAppResponse_userProfileName,
    describeAppResponse_domainId,
    describeAppResponse_appArn,
    describeAppResponse_failureReason,
    describeAppResponse_lastHealthCheckTimestamp,
    describeAppResponse_lastUserActivityTimestamp,
    describeAppResponse_httpStatus,

    -- ** ListTrialComponents
    listTrialComponents_createdAfter,
    listTrialComponents_sortOrder,
    listTrialComponents_nextToken,
    listTrialComponents_createdBefore,
    listTrialComponents_maxResults,
    listTrialComponents_sortBy,
    listTrialComponents_experimentName,
    listTrialComponents_sourceArn,
    listTrialComponents_trialName,
    listTrialComponentsResponse_nextToken,
    listTrialComponentsResponse_trialComponentSummaries,
    listTrialComponentsResponse_httpStatus,

    -- ** UpdateTrialComponent
    updateTrialComponent_outputArtifactsToRemove,
    updateTrialComponent_parametersToRemove,
    updateTrialComponent_status,
    updateTrialComponent_inputArtifactsToRemove,
    updateTrialComponent_startTime,
    updateTrialComponent_endTime,
    updateTrialComponent_inputArtifacts,
    updateTrialComponent_displayName,
    updateTrialComponent_parameters,
    updateTrialComponent_outputArtifacts,
    updateTrialComponent_trialComponentName,
    updateTrialComponentResponse_trialComponentArn,
    updateTrialComponentResponse_httpStatus,

    -- ** DeleteTrialComponent
    deleteTrialComponent_trialComponentName,
    deleteTrialComponentResponse_trialComponentArn,
    deleteTrialComponentResponse_httpStatus,

    -- ** CreateTrialComponent
    createTrialComponent_status,
    createTrialComponent_metadataProperties,
    createTrialComponent_startTime,
    createTrialComponent_endTime,
    createTrialComponent_tags,
    createTrialComponent_inputArtifacts,
    createTrialComponent_displayName,
    createTrialComponent_parameters,
    createTrialComponent_outputArtifacts,
    createTrialComponent_trialComponentName,
    createTrialComponentResponse_trialComponentArn,
    createTrialComponentResponse_httpStatus,

    -- ** DescribeWorkforce
    describeWorkforce_workforceName,
    describeWorkforceResponse_httpStatus,
    describeWorkforceResponse_workforce,

    -- ** ListNotebookInstances
    listNotebookInstances_lastModifiedTimeBefore,
    listNotebookInstances_sortOrder,
    listNotebookInstances_nextToken,
    listNotebookInstances_nameContains,
    listNotebookInstances_additionalCodeRepositoryEquals,
    listNotebookInstances_maxResults,
    listNotebookInstances_creationTimeBefore,
    listNotebookInstances_lastModifiedTimeAfter,
    listNotebookInstances_defaultCodeRepositoryContains,
    listNotebookInstances_sortBy,
    listNotebookInstances_statusEquals,
    listNotebookInstances_notebookInstanceLifecycleConfigNameContains,
    listNotebookInstances_creationTimeAfter,
    listNotebookInstancesResponse_nextToken,
    listNotebookInstancesResponse_notebookInstances,
    listNotebookInstancesResponse_httpStatus,

    -- ** ListModelExplainabilityJobDefinitions
    listModelExplainabilityJobDefinitions_sortOrder,
    listModelExplainabilityJobDefinitions_nextToken,
    listModelExplainabilityJobDefinitions_endpointName,
    listModelExplainabilityJobDefinitions_nameContains,
    listModelExplainabilityJobDefinitions_maxResults,
    listModelExplainabilityJobDefinitions_creationTimeBefore,
    listModelExplainabilityJobDefinitions_sortBy,
    listModelExplainabilityJobDefinitions_creationTimeAfter,
    listModelExplainabilityJobDefinitionsResponse_nextToken,
    listModelExplainabilityJobDefinitionsResponse_httpStatus,
    listModelExplainabilityJobDefinitionsResponse_jobDefinitionSummaries,

    -- ** DeleteModelQualityJobDefinition
    deleteModelQualityJobDefinition_jobDefinitionName,

    -- ** StopLabelingJob
    stopLabelingJob_labelingJobName,

    -- ** ListModelQualityJobDefinitions
    listModelQualityJobDefinitions_sortOrder,
    listModelQualityJobDefinitions_nextToken,
    listModelQualityJobDefinitions_endpointName,
    listModelQualityJobDefinitions_nameContains,
    listModelQualityJobDefinitions_maxResults,
    listModelQualityJobDefinitions_creationTimeBefore,
    listModelQualityJobDefinitions_sortBy,
    listModelQualityJobDefinitions_creationTimeAfter,
    listModelQualityJobDefinitionsResponse_nextToken,
    listModelQualityJobDefinitionsResponse_httpStatus,
    listModelQualityJobDefinitionsResponse_jobDefinitionSummaries,

    -- ** DescribeModelBiasJobDefinition
    describeModelBiasJobDefinition_jobDefinitionName,
    describeModelBiasJobDefinitionResponse_networkConfig,
    describeModelBiasJobDefinitionResponse_modelBiasBaselineConfig,
    describeModelBiasJobDefinitionResponse_stoppingCondition,
    describeModelBiasJobDefinitionResponse_httpStatus,
    describeModelBiasJobDefinitionResponse_jobDefinitionArn,
    describeModelBiasJobDefinitionResponse_jobDefinitionName,
    describeModelBiasJobDefinitionResponse_creationTime,
    describeModelBiasJobDefinitionResponse_modelBiasAppSpecification,
    describeModelBiasJobDefinitionResponse_modelBiasJobInput,
    describeModelBiasJobDefinitionResponse_modelBiasJobOutputConfig,
    describeModelBiasJobDefinitionResponse_jobResources,
    describeModelBiasJobDefinitionResponse_roleArn,

    -- ** DescribeWorkteam
    describeWorkteam_workteamName,
    describeWorkteamResponse_httpStatus,
    describeWorkteamResponse_workteam,

    -- ** DescribeNotebookInstanceLifecycleConfig
    describeNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,
    describeNotebookInstanceLifecycleConfigResponse_creationTime,
    describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn,
    describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigName,
    describeNotebookInstanceLifecycleConfigResponse_onStart,
    describeNotebookInstanceLifecycleConfigResponse_lastModifiedTime,
    describeNotebookInstanceLifecycleConfigResponse_onCreate,
    describeNotebookInstanceLifecycleConfigResponse_httpStatus,

    -- ** ListPipelineExecutions
    listPipelineExecutions_createdAfter,
    listPipelineExecutions_sortOrder,
    listPipelineExecutions_nextToken,
    listPipelineExecutions_createdBefore,
    listPipelineExecutions_maxResults,
    listPipelineExecutions_sortBy,
    listPipelineExecutions_pipelineName,
    listPipelineExecutionsResponse_nextToken,
    listPipelineExecutionsResponse_pipelineExecutionSummaries,
    listPipelineExecutionsResponse_httpStatus,

    -- ** UpdateDomain
    updateDomain_defaultUserSettings,
    updateDomain_domainId,
    updateDomainResponse_domainArn,
    updateDomainResponse_httpStatus,

    -- ** AssociateTrialComponent
    associateTrialComponent_trialComponentName,
    associateTrialComponent_trialName,
    associateTrialComponentResponse_trialArn,
    associateTrialComponentResponse_trialComponentArn,
    associateTrialComponentResponse_httpStatus,

    -- ** UpdatePipelineExecution
    updatePipelineExecution_pipelineExecutionDescription,
    updatePipelineExecution_pipelineExecutionDisplayName,
    updatePipelineExecution_pipelineExecutionArn,
    updatePipelineExecutionResponse_pipelineExecutionArn,
    updatePipelineExecutionResponse_httpStatus,

    -- ** CreateImageVersion
    createImageVersion_baseImage,
    createImageVersion_clientToken,
    createImageVersion_imageName,
    createImageVersionResponse_imageVersionArn,
    createImageVersionResponse_httpStatus,

    -- ** DeleteDomain
    deleteDomain_retentionPolicy,
    deleteDomain_domainId,

    -- ** UpdateTrainingJob
    updateTrainingJob_profilerConfig,
    updateTrainingJob_profilerRuleConfigurations,
    updateTrainingJob_trainingJobName,
    updateTrainingJobResponse_httpStatus,
    updateTrainingJobResponse_trainingJobArn,

    -- ** UpdateImage
    updateImage_roleArn,
    updateImage_deleteProperties,
    updateImage_description,
    updateImage_displayName,
    updateImage_imageName,
    updateImageResponse_imageArn,
    updateImageResponse_httpStatus,

    -- ** UpdateContext
    updateContext_propertiesToRemove,
    updateContext_properties,
    updateContext_description,
    updateContext_contextName,
    updateContextResponse_contextArn,
    updateContextResponse_httpStatus,

    -- ** DeleteImage
    deleteImage_imageName,
    deleteImageResponse_httpStatus,

    -- ** ListFlowDefinitions
    listFlowDefinitions_sortOrder,
    listFlowDefinitions_nextToken,
    listFlowDefinitions_maxResults,
    listFlowDefinitions_creationTimeBefore,
    listFlowDefinitions_creationTimeAfter,
    listFlowDefinitionsResponse_nextToken,
    listFlowDefinitionsResponse_httpStatus,
    listFlowDefinitionsResponse_flowDefinitionSummaries,

    -- ** ListModels
    listModels_sortOrder,
    listModels_nextToken,
    listModels_nameContains,
    listModels_maxResults,
    listModels_creationTimeBefore,
    listModels_sortBy,
    listModels_creationTimeAfter,
    listModelsResponse_nextToken,
    listModelsResponse_httpStatus,
    listModelsResponse_models,

    -- ** CreateUserProfile
    createUserProfile_userSettings,
    createUserProfile_tags,
    createUserProfile_singleSignOnUserIdentifier,
    createUserProfile_singleSignOnUserValue,
    createUserProfile_domainId,
    createUserProfile_userProfileName,
    createUserProfileResponse_userProfileArn,
    createUserProfileResponse_httpStatus,

    -- ** RenderUiTemplate
    renderUiTemplate_uiTemplate,
    renderUiTemplate_humanTaskUiArn,
    renderUiTemplate_task,
    renderUiTemplate_roleArn,
    renderUiTemplateResponse_httpStatus,
    renderUiTemplateResponse_renderedContent,
    renderUiTemplateResponse_errors,

    -- ** DescribeFeatureGroup
    describeFeatureGroup_nextToken,
    describeFeatureGroup_featureGroupName,
    describeFeatureGroupResponse_featureGroupStatus,
    describeFeatureGroupResponse_offlineStoreConfig,
    describeFeatureGroupResponse_roleArn,
    describeFeatureGroupResponse_offlineStoreStatus,
    describeFeatureGroupResponse_failureReason,
    describeFeatureGroupResponse_description,
    describeFeatureGroupResponse_onlineStoreConfig,
    describeFeatureGroupResponse_httpStatus,
    describeFeatureGroupResponse_featureGroupArn,
    describeFeatureGroupResponse_featureGroupName,
    describeFeatureGroupResponse_recordIdentifierFeatureName,
    describeFeatureGroupResponse_eventTimeFeatureName,
    describeFeatureGroupResponse_featureDefinitions,
    describeFeatureGroupResponse_creationTime,
    describeFeatureGroupResponse_nextToken,

    -- ** DeleteContext
    deleteContext_contextName,
    deleteContextResponse_contextArn,
    deleteContextResponse_httpStatus,

    -- ** ListHyperParameterTuningJobs
    listHyperParameterTuningJobs_lastModifiedTimeBefore,
    listHyperParameterTuningJobs_sortOrder,
    listHyperParameterTuningJobs_nextToken,
    listHyperParameterTuningJobs_nameContains,
    listHyperParameterTuningJobs_maxResults,
    listHyperParameterTuningJobs_creationTimeBefore,
    listHyperParameterTuningJobs_lastModifiedTimeAfter,
    listHyperParameterTuningJobs_sortBy,
    listHyperParameterTuningJobs_statusEquals,
    listHyperParameterTuningJobs_creationTimeAfter,
    listHyperParameterTuningJobsResponse_nextToken,
    listHyperParameterTuningJobsResponse_httpStatus,
    listHyperParameterTuningJobsResponse_hyperParameterTuningJobSummaries,

    -- ** DeleteFlowDefinition
    deleteFlowDefinition_flowDefinitionName,
    deleteFlowDefinitionResponse_httpStatus,

    -- ** ListAlgorithms
    listAlgorithms_sortOrder,
    listAlgorithms_nextToken,
    listAlgorithms_nameContains,
    listAlgorithms_maxResults,
    listAlgorithms_creationTimeBefore,
    listAlgorithms_sortBy,
    listAlgorithms_creationTimeAfter,
    listAlgorithmsResponse_nextToken,
    listAlgorithmsResponse_httpStatus,
    listAlgorithmsResponse_algorithmSummaryList,

    -- ** CreateAlgorithm
    createAlgorithm_algorithmDescription,
    createAlgorithm_validationSpecification,
    createAlgorithm_certifyForMarketplace,
    createAlgorithm_tags,
    createAlgorithm_inferenceSpecification,
    createAlgorithm_algorithmName,
    createAlgorithm_trainingSpecification,
    createAlgorithmResponse_httpStatus,
    createAlgorithmResponse_algorithmArn,

    -- ** CreateFlowDefinition
    createFlowDefinition_humanLoopRequestSource,
    createFlowDefinition_tags,
    createFlowDefinition_humanLoopActivationConfig,
    createFlowDefinition_flowDefinitionName,
    createFlowDefinition_humanLoopConfig,
    createFlowDefinition_outputConfig,
    createFlowDefinition_roleArn,
    createFlowDefinitionResponse_httpStatus,
    createFlowDefinitionResponse_flowDefinitionArn,

    -- ** ListPipelineParametersForExecution
    listPipelineParametersForExecution_nextToken,
    listPipelineParametersForExecution_maxResults,
    listPipelineParametersForExecution_pipelineExecutionArn,
    listPipelineParametersForExecutionResponse_nextToken,
    listPipelineParametersForExecutionResponse_pipelineParameters,
    listPipelineParametersForExecutionResponse_httpStatus,

    -- ** ListTrials
    listTrials_createdAfter,
    listTrials_sortOrder,
    listTrials_nextToken,
    listTrials_createdBefore,
    listTrials_maxResults,
    listTrials_sortBy,
    listTrials_experimentName,
    listTrials_trialComponentName,
    listTrialsResponse_nextToken,
    listTrialsResponse_trialSummaries,
    listTrialsResponse_httpStatus,

    -- ** CreateHyperParameterTuningJob
    createHyperParameterTuningJob_warmStartConfig,
    createHyperParameterTuningJob_tags,
    createHyperParameterTuningJob_trainingJobDefinitions,
    createHyperParameterTuningJob_trainingJobDefinition,
    createHyperParameterTuningJob_hyperParameterTuningJobName,
    createHyperParameterTuningJob_hyperParameterTuningJobConfig,
    createHyperParameterTuningJobResponse_httpStatus,
    createHyperParameterTuningJobResponse_hyperParameterTuningJobArn,

    -- ** CreateModel
    createModel_vpcConfig,
    createModel_primaryContainer,
    createModel_enableNetworkIsolation,
    createModel_containers,
    createModel_tags,
    createModel_inferenceExecutionConfig,
    createModel_modelName,
    createModel_executionRoleArn,
    createModelResponse_httpStatus,
    createModelResponse_modelArn,

    -- ** UpdateTrial
    updateTrial_displayName,
    updateTrial_trialName,
    updateTrialResponse_trialArn,
    updateTrialResponse_httpStatus,

    -- ** DeleteModelPackageGroup
    deleteModelPackageGroup_modelPackageGroupName,

    -- ** DescribeDeviceFleet
    describeDeviceFleet_deviceFleetName,
    describeDeviceFleetResponse_roleArn,
    describeDeviceFleetResponse_iotRoleAlias,
    describeDeviceFleetResponse_description,
    describeDeviceFleetResponse_httpStatus,
    describeDeviceFleetResponse_deviceFleetName,
    describeDeviceFleetResponse_deviceFleetArn,
    describeDeviceFleetResponse_outputConfig,
    describeDeviceFleetResponse_creationTime,
    describeDeviceFleetResponse_lastModifiedTime,

    -- ** ListModelPackageGroups
    listModelPackageGroups_sortOrder,
    listModelPackageGroups_nextToken,
    listModelPackageGroups_nameContains,
    listModelPackageGroups_maxResults,
    listModelPackageGroups_creationTimeBefore,
    listModelPackageGroups_sortBy,
    listModelPackageGroups_creationTimeAfter,
    listModelPackageGroupsResponse_nextToken,
    listModelPackageGroupsResponse_httpStatus,
    listModelPackageGroupsResponse_modelPackageGroupSummaryList,

    -- ** ListPipelines
    listPipelines_createdAfter,
    listPipelines_sortOrder,
    listPipelines_nextToken,
    listPipelines_createdBefore,
    listPipelines_maxResults,
    listPipelines_sortBy,
    listPipelines_pipelineNamePrefix,
    listPipelinesResponse_nextToken,
    listPipelinesResponse_pipelineSummaries,
    listPipelinesResponse_httpStatus,

    -- ** ListTags
    listTags_nextToken,
    listTags_maxResults,
    listTags_resourceArn,
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** DescribePipelineDefinitionForExecution
    describePipelineDefinitionForExecution_pipelineExecutionArn,
    describePipelineDefinitionForExecutionResponse_creationTime,
    describePipelineDefinitionForExecutionResponse_pipelineDefinition,
    describePipelineDefinitionForExecutionResponse_httpStatus,

    -- ** DeleteTrial
    deleteTrial_trialName,
    deleteTrialResponse_trialArn,
    deleteTrialResponse_httpStatus,

    -- ** PutModelPackageGroupPolicy
    putModelPackageGroupPolicy_modelPackageGroupName,
    putModelPackageGroupPolicy_resourcePolicy,
    putModelPackageGroupPolicyResponse_httpStatus,
    putModelPackageGroupPolicyResponse_modelPackageGroupArn,

    -- ** ListExperiments
    listExperiments_createdAfter,
    listExperiments_sortOrder,
    listExperiments_nextToken,
    listExperiments_createdBefore,
    listExperiments_maxResults,
    listExperiments_sortBy,
    listExperimentsResponse_nextToken,
    listExperimentsResponse_experimentSummaries,
    listExperimentsResponse_httpStatus,

    -- ** UpdateExperiment
    updateExperiment_description,
    updateExperiment_displayName,
    updateExperiment_experimentName,
    updateExperimentResponse_experimentArn,
    updateExperimentResponse_httpStatus,

    -- ** DeleteExperiment
    deleteExperiment_experimentName,
    deleteExperimentResponse_experimentArn,
    deleteExperimentResponse_httpStatus,

    -- ** ListLabelingJobs
    listLabelingJobs_lastModifiedTimeBefore,
    listLabelingJobs_sortOrder,
    listLabelingJobs_nextToken,
    listLabelingJobs_nameContains,
    listLabelingJobs_maxResults,
    listLabelingJobs_creationTimeBefore,
    listLabelingJobs_lastModifiedTimeAfter,
    listLabelingJobs_sortBy,
    listLabelingJobs_statusEquals,
    listLabelingJobs_creationTimeAfter,
    listLabelingJobsResponse_labelingJobSummaryList,
    listLabelingJobsResponse_nextToken,
    listLabelingJobsResponse_httpStatus,

    -- ** DescribeImageVersion
    describeImageVersion_version,
    describeImageVersion_imageName,
    describeImageVersionResponse_creationTime,
    describeImageVersionResponse_imageVersionStatus,
    describeImageVersionResponse_containerImage,
    describeImageVersionResponse_imageVersionArn,
    describeImageVersionResponse_baseImage,
    describeImageVersionResponse_version,
    describeImageVersionResponse_failureReason,
    describeImageVersionResponse_lastModifiedTime,
    describeImageVersionResponse_imageArn,
    describeImageVersionResponse_httpStatus,

    -- ** DeleteApp
    deleteApp_domainId,
    deleteApp_userProfileName,
    deleteApp_appType,
    deleteApp_appName,

    -- ** CreateModelBiasJobDefinition
    createModelBiasJobDefinition_networkConfig,
    createModelBiasJobDefinition_modelBiasBaselineConfig,
    createModelBiasJobDefinition_tags,
    createModelBiasJobDefinition_stoppingCondition,
    createModelBiasJobDefinition_jobDefinitionName,
    createModelBiasJobDefinition_modelBiasAppSpecification,
    createModelBiasJobDefinition_modelBiasJobInput,
    createModelBiasJobDefinition_modelBiasJobOutputConfig,
    createModelBiasJobDefinition_jobResources,
    createModelBiasJobDefinition_roleArn,
    createModelBiasJobDefinitionResponse_httpStatus,
    createModelBiasJobDefinitionResponse_jobDefinitionArn,

    -- ** DescribeTrialComponent
    describeTrialComponent_trialComponentName,
    describeTrialComponentResponse_status,
    describeTrialComponentResponse_metadataProperties,
    describeTrialComponentResponse_creationTime,
    describeTrialComponentResponse_trialComponentArn,
    describeTrialComponentResponse_startTime,
    describeTrialComponentResponse_source,
    describeTrialComponentResponse_endTime,
    describeTrialComponentResponse_metrics,
    describeTrialComponentResponse_lastModifiedTime,
    describeTrialComponentResponse_inputArtifacts,
    describeTrialComponentResponse_createdBy,
    describeTrialComponentResponse_lastModifiedBy,
    describeTrialComponentResponse_displayName,
    describeTrialComponentResponse_parameters,
    describeTrialComponentResponse_outputArtifacts,
    describeTrialComponentResponse_trialComponentName,
    describeTrialComponentResponse_httpStatus,

    -- ** CreateWorkteam
    createWorkteam_workforceName,
    createWorkteam_notificationConfiguration,
    createWorkteam_tags,
    createWorkteam_workteamName,
    createWorkteam_memberDefinitions,
    createWorkteam_description,
    createWorkteamResponse_workteamArn,
    createWorkteamResponse_httpStatus,

    -- ** DescribeProject
    describeProject_projectName,
    describeProjectResponse_serviceCatalogProvisionedProductDetails,
    describeProjectResponse_projectDescription,
    describeProjectResponse_createdBy,
    describeProjectResponse_httpStatus,
    describeProjectResponse_projectArn,
    describeProjectResponse_projectName,
    describeProjectResponse_projectId,
    describeProjectResponse_serviceCatalogProvisioningDetails,
    describeProjectResponse_projectStatus,
    describeProjectResponse_creationTime,

    -- ** CreateProcessingJob
    createProcessingJob_networkConfig,
    createProcessingJob_processingOutputConfig,
    createProcessingJob_experimentConfig,
    createProcessingJob_environment,
    createProcessingJob_tags,
    createProcessingJob_processingInputs,
    createProcessingJob_stoppingCondition,
    createProcessingJob_processingJobName,
    createProcessingJob_processingResources,
    createProcessingJob_appSpecification,
    createProcessingJob_roleArn,
    createProcessingJobResponse_httpStatus,
    createProcessingJobResponse_processingJobArn,

    -- ** ListAssociations
    listAssociations_createdAfter,
    listAssociations_sortOrder,
    listAssociations_nextToken,
    listAssociations_destinationType,
    listAssociations_createdBefore,
    listAssociations_destinationArn,
    listAssociations_maxResults,
    listAssociations_sortBy,
    listAssociations_associationType,
    listAssociations_sourceArn,
    listAssociations_sourceType,
    listAssociationsResponse_nextToken,
    listAssociationsResponse_associationSummaries,
    listAssociationsResponse_httpStatus,

    -- ** EnableSagemakerServicecatalogPortfolio
    enableSagemakerServicecatalogPortfolioResponse_httpStatus,

    -- ** UpdateAppImageConfig
    updateAppImageConfig_kernelGatewayImageConfig,
    updateAppImageConfig_appImageConfigName,
    updateAppImageConfigResponse_appImageConfigArn,
    updateAppImageConfigResponse_httpStatus,

    -- ** ListModelBiasJobDefinitions
    listModelBiasJobDefinitions_sortOrder,
    listModelBiasJobDefinitions_nextToken,
    listModelBiasJobDefinitions_endpointName,
    listModelBiasJobDefinitions_nameContains,
    listModelBiasJobDefinitions_maxResults,
    listModelBiasJobDefinitions_creationTimeBefore,
    listModelBiasJobDefinitions_sortBy,
    listModelBiasJobDefinitions_creationTimeAfter,
    listModelBiasJobDefinitionsResponse_nextToken,
    listModelBiasJobDefinitionsResponse_httpStatus,
    listModelBiasJobDefinitionsResponse_jobDefinitionSummaries,

    -- ** CreateAutoMLJob
    createAutoMLJob_generateCandidateDefinitionsOnly,
    createAutoMLJob_tags,
    createAutoMLJob_autoMLJobObjective,
    createAutoMLJob_autoMLJobConfig,
    createAutoMLJob_problemType,
    createAutoMLJob_autoMLJobName,
    createAutoMLJob_inputDataConfig,
    createAutoMLJob_outputDataConfig,
    createAutoMLJob_roleArn,
    createAutoMLJobResponse_httpStatus,
    createAutoMLJobResponse_autoMLJobArn,

    -- ** CreateApp
    createApp_resourceSpec,
    createApp_tags,
    createApp_domainId,
    createApp_userProfileName,
    createApp_appType,
    createApp_appName,
    createAppResponse_appArn,
    createAppResponse_httpStatus,

    -- ** DescribeNotebookInstance
    describeNotebookInstance_notebookInstanceName,
    describeNotebookInstanceResponse_notebookInstanceName,
    describeNotebookInstanceResponse_creationTime,
    describeNotebookInstanceResponse_acceleratorTypes,
    describeNotebookInstanceResponse_defaultCodeRepository,
    describeNotebookInstanceResponse_roleArn,
    describeNotebookInstanceResponse_instanceType,
    describeNotebookInstanceResponse_notebookInstanceLifecycleConfigName,
    describeNotebookInstanceResponse_additionalCodeRepositories,
    describeNotebookInstanceResponse_securityGroups,
    describeNotebookInstanceResponse_kmsKeyId,
    describeNotebookInstanceResponse_volumeSizeInGB,
    describeNotebookInstanceResponse_notebookInstanceStatus,
    describeNotebookInstanceResponse_failureReason,
    describeNotebookInstanceResponse_networkInterfaceId,
    describeNotebookInstanceResponse_lastModifiedTime,
    describeNotebookInstanceResponse_subnetId,
    describeNotebookInstanceResponse_url,
    describeNotebookInstanceResponse_notebookInstanceArn,
    describeNotebookInstanceResponse_directInternetAccess,
    describeNotebookInstanceResponse_rootAccess,
    describeNotebookInstanceResponse_httpStatus,

    -- ** DeleteAppImageConfig
    deleteAppImageConfig_appImageConfigName,

    -- ** CreateEndpointConfig
    createEndpointConfig_kmsKeyId,
    createEndpointConfig_tags,
    createEndpointConfig_dataCaptureConfig,
    createEndpointConfig_endpointConfigName,
    createEndpointConfig_productionVariants,
    createEndpointConfigResponse_httpStatus,
    createEndpointConfigResponse_endpointConfigArn,

    -- ** ListProcessingJobs
    listProcessingJobs_lastModifiedTimeBefore,
    listProcessingJobs_sortOrder,
    listProcessingJobs_nextToken,
    listProcessingJobs_nameContains,
    listProcessingJobs_maxResults,
    listProcessingJobs_creationTimeBefore,
    listProcessingJobs_lastModifiedTimeAfter,
    listProcessingJobs_sortBy,
    listProcessingJobs_statusEquals,
    listProcessingJobs_creationTimeAfter,
    listProcessingJobsResponse_nextToken,
    listProcessingJobsResponse_httpStatus,
    listProcessingJobsResponse_processingJobSummaries,

    -- ** CreateMonitoringSchedule
    createMonitoringSchedule_tags,
    createMonitoringSchedule_monitoringScheduleName,
    createMonitoringSchedule_monitoringScheduleConfig,
    createMonitoringScheduleResponse_httpStatus,
    createMonitoringScheduleResponse_monitoringScheduleArn,

    -- ** DescribeModelExplainabilityJobDefinition
    describeModelExplainabilityJobDefinition_jobDefinitionName,
    describeModelExplainabilityJobDefinitionResponse_networkConfig,
    describeModelExplainabilityJobDefinitionResponse_modelExplainabilityBaselineConfig,
    describeModelExplainabilityJobDefinitionResponse_stoppingCondition,
    describeModelExplainabilityJobDefinitionResponse_httpStatus,
    describeModelExplainabilityJobDefinitionResponse_jobDefinitionArn,
    describeModelExplainabilityJobDefinitionResponse_jobDefinitionName,
    describeModelExplainabilityJobDefinitionResponse_creationTime,
    describeModelExplainabilityJobDefinitionResponse_modelExplainabilityAppSpecification,
    describeModelExplainabilityJobDefinitionResponse_modelExplainabilityJobInput,
    describeModelExplainabilityJobDefinitionResponse_modelExplainabilityJobOutputConfig,
    describeModelExplainabilityJobDefinitionResponse_jobResources,
    describeModelExplainabilityJobDefinitionResponse_roleArn,

    -- ** ListWorkteams
    listWorkteams_sortOrder,
    listWorkteams_nextToken,
    listWorkteams_nameContains,
    listWorkteams_maxResults,
    listWorkteams_sortBy,
    listWorkteamsResponse_nextToken,
    listWorkteamsResponse_httpStatus,
    listWorkteamsResponse_workteams,

    -- ** DescribeFlowDefinition
    describeFlowDefinition_flowDefinitionName,
    describeFlowDefinitionResponse_humanLoopRequestSource,
    describeFlowDefinitionResponse_failureReason,
    describeFlowDefinitionResponse_humanLoopActivationConfig,
    describeFlowDefinitionResponse_httpStatus,
    describeFlowDefinitionResponse_flowDefinitionArn,
    describeFlowDefinitionResponse_flowDefinitionName,
    describeFlowDefinitionResponse_flowDefinitionStatus,
    describeFlowDefinitionResponse_creationTime,
    describeFlowDefinitionResponse_humanLoopConfig,
    describeFlowDefinitionResponse_outputConfig,
    describeFlowDefinitionResponse_roleArn,

    -- ** DescribeContext
    describeContext_contextName,
    describeContextResponse_contextType,
    describeContextResponse_creationTime,
    describeContextResponse_contextName,
    describeContextResponse_source,
    describeContextResponse_properties,
    describeContextResponse_lastModifiedTime,
    describeContextResponse_description,
    describeContextResponse_createdBy,
    describeContextResponse_lastModifiedBy,
    describeContextResponse_contextArn,
    describeContextResponse_httpStatus,

    -- ** RegisterDevices
    registerDevices_tags,
    registerDevices_deviceFleetName,
    registerDevices_devices,

    -- ** ListFeatureGroups
    listFeatureGroups_sortOrder,
    listFeatureGroups_nextToken,
    listFeatureGroups_nameContains,
    listFeatureGroups_maxResults,
    listFeatureGroups_offlineStoreStatusEquals,
    listFeatureGroups_creationTimeBefore,
    listFeatureGroups_sortBy,
    listFeatureGroups_creationTimeAfter,
    listFeatureGroups_featureGroupStatusEquals,
    listFeatureGroupsResponse_nextToken,
    listFeatureGroupsResponse_httpStatus,
    listFeatureGroupsResponse_featureGroupSummaries,

    -- ** CreatePresignedNotebookInstanceUrl
    createPresignedNotebookInstanceUrl_sessionExpirationDurationInSeconds,
    createPresignedNotebookInstanceUrl_notebookInstanceName,
    createPresignedNotebookInstanceUrlResponse_authorizedUrl,
    createPresignedNotebookInstanceUrlResponse_httpStatus,

    -- ** DescribeTrainingJob
    describeTrainingJob_trainingJobName,
    describeTrainingJobResponse_vpcConfig,
    describeTrainingJobResponse_debugRuleConfigurations,
    describeTrainingJobResponse_inputDataConfig,
    describeTrainingJobResponse_hyperParameters,
    describeTrainingJobResponse_enableManagedSpotTraining,
    describeTrainingJobResponse_labelingJobArn,
    describeTrainingJobResponse_roleArn,
    describeTrainingJobResponse_trainingTimeInSeconds,
    describeTrainingJobResponse_profilerConfig,
    describeTrainingJobResponse_experimentConfig,
    describeTrainingJobResponse_profilerRuleEvaluationStatuses,
    describeTrainingJobResponse_enableNetworkIsolation,
    describeTrainingJobResponse_enableInterContainerTrafficEncryption,
    describeTrainingJobResponse_checkpointConfig,
    describeTrainingJobResponse_outputDataConfig,
    describeTrainingJobResponse_tuningJobArn,
    describeTrainingJobResponse_secondaryStatusTransitions,
    describeTrainingJobResponse_finalMetricDataList,
    describeTrainingJobResponse_profilingStatus,
    describeTrainingJobResponse_profilerRuleConfigurations,
    describeTrainingJobResponse_autoMLJobArn,
    describeTrainingJobResponse_failureReason,
    describeTrainingJobResponse_lastModifiedTime,
    describeTrainingJobResponse_tensorBoardOutputConfig,
    describeTrainingJobResponse_debugRuleEvaluationStatuses,
    describeTrainingJobResponse_debugHookConfig,
    describeTrainingJobResponse_billableTimeInSeconds,
    describeTrainingJobResponse_trainingStartTime,
    describeTrainingJobResponse_trainingEndTime,
    describeTrainingJobResponse_httpStatus,
    describeTrainingJobResponse_trainingJobName,
    describeTrainingJobResponse_trainingJobArn,
    describeTrainingJobResponse_modelArtifacts,
    describeTrainingJobResponse_trainingJobStatus,
    describeTrainingJobResponse_secondaryStatus,
    describeTrainingJobResponse_algorithmSpecification,
    describeTrainingJobResponse_resourceConfig,
    describeTrainingJobResponse_stoppingCondition,
    describeTrainingJobResponse_creationTime,

    -- ** CreateHumanTaskUi
    createHumanTaskUi_tags,
    createHumanTaskUi_humanTaskUiName,
    createHumanTaskUi_uiTemplate,
    createHumanTaskUiResponse_httpStatus,
    createHumanTaskUiResponse_humanTaskUiArn,

    -- ** ListTrainingJobsForHyperParameterTuningJob
    listTrainingJobsForHyperParameterTuningJob_sortOrder,
    listTrainingJobsForHyperParameterTuningJob_nextToken,
    listTrainingJobsForHyperParameterTuningJob_maxResults,
    listTrainingJobsForHyperParameterTuningJob_sortBy,
    listTrainingJobsForHyperParameterTuningJob_statusEquals,
    listTrainingJobsForHyperParameterTuningJob_hyperParameterTuningJobName,
    listTrainingJobsForHyperParameterTuningJobResponse_nextToken,
    listTrainingJobsForHyperParameterTuningJobResponse_httpStatus,
    listTrainingJobsForHyperParameterTuningJobResponse_trainingJobSummaries,

    -- ** DescribeImage
    describeImage_imageName,
    describeImageResponse_creationTime,
    describeImageResponse_roleArn,
    describeImageResponse_imageName,
    describeImageResponse_failureReason,
    describeImageResponse_lastModifiedTime,
    describeImageResponse_description,
    describeImageResponse_imageStatus,
    describeImageResponse_displayName,
    describeImageResponse_imageArn,
    describeImageResponse_httpStatus,

    -- ** DeleteFeatureGroup
    deleteFeatureGroup_featureGroupName,

    -- * Types

    -- ** ActionSource
    actionSource_sourceId,
    actionSource_sourceType,
    actionSource_sourceUri,

    -- ** ActionSummary
    actionSummary_status,
    actionSummary_creationTime,
    actionSummary_actionName,
    actionSummary_actionType,
    actionSummary_actionArn,
    actionSummary_source,
    actionSummary_lastModifiedTime,

    -- ** AgentVersion
    agentVersion_version,
    agentVersion_agentCount,

    -- ** Alarm
    alarm_alarmName,

    -- ** AlgorithmSpecification
    algorithmSpecification_trainingImage,
    algorithmSpecification_enableSageMakerMetricsTimeSeries,
    algorithmSpecification_metricDefinitions,
    algorithmSpecification_algorithmName,
    algorithmSpecification_trainingInputMode,

    -- ** AlgorithmStatusDetails
    algorithmStatusDetails_validationStatuses,
    algorithmStatusDetails_imageScanStatuses,

    -- ** AlgorithmStatusItem
    algorithmStatusItem_failureReason,
    algorithmStatusItem_name,
    algorithmStatusItem_status,

    -- ** AlgorithmSummary
    algorithmSummary_algorithmDescription,
    algorithmSummary_algorithmName,
    algorithmSummary_algorithmArn,
    algorithmSummary_creationTime,
    algorithmSummary_algorithmStatus,

    -- ** AlgorithmValidationProfile
    algorithmValidationProfile_transformJobDefinition,
    algorithmValidationProfile_profileName,
    algorithmValidationProfile_trainingJobDefinition,

    -- ** AlgorithmValidationSpecification
    algorithmValidationSpecification_validationRole,
    algorithmValidationSpecification_validationProfiles,

    -- ** AnnotationConsolidationConfig
    annotationConsolidationConfig_annotationConsolidationLambdaArn,

    -- ** AppDetails
    appDetails_status,
    appDetails_creationTime,
    appDetails_appType,
    appDetails_appName,
    appDetails_userProfileName,
    appDetails_domainId,

    -- ** AppImageConfigDetails
    appImageConfigDetails_creationTime,
    appImageConfigDetails_appImageConfigArn,
    appImageConfigDetails_kernelGatewayImageConfig,
    appImageConfigDetails_appImageConfigName,
    appImageConfigDetails_lastModifiedTime,

    -- ** AppSpecification
    appSpecification_containerArguments,
    appSpecification_containerEntrypoint,
    appSpecification_imageUri,

    -- ** ArtifactSource
    artifactSource_sourceTypes,
    artifactSource_sourceUri,

    -- ** ArtifactSourceType
    artifactSourceType_sourceIdType,
    artifactSourceType_value,

    -- ** ArtifactSummary
    artifactSummary_creationTime,
    artifactSummary_artifactName,
    artifactSummary_artifactType,
    artifactSummary_artifactArn,
    artifactSummary_source,
    artifactSummary_lastModifiedTime,

    -- ** AssociationSummary
    associationSummary_destinationType,
    associationSummary_creationTime,
    associationSummary_destinationArn,
    associationSummary_destinationName,
    associationSummary_sourceName,
    associationSummary_associationType,
    associationSummary_createdBy,
    associationSummary_sourceArn,
    associationSummary_sourceType,

    -- ** AthenaDatasetDefinition
    athenaDatasetDefinition_outputCompression,
    athenaDatasetDefinition_kmsKeyId,
    athenaDatasetDefinition_workGroup,
    athenaDatasetDefinition_catalog,
    athenaDatasetDefinition_database,
    athenaDatasetDefinition_queryString,
    athenaDatasetDefinition_outputS3Uri,
    athenaDatasetDefinition_outputFormat,

    -- ** AutoMLCandidate
    autoMLCandidate_endTime,
    autoMLCandidate_inferenceContainers,
    autoMLCandidate_failureReason,
    autoMLCandidate_finalAutoMLJobObjectiveMetric,
    autoMLCandidate_candidateName,
    autoMLCandidate_objectiveStatus,
    autoMLCandidate_candidateSteps,
    autoMLCandidate_candidateStatus,
    autoMLCandidate_creationTime,
    autoMLCandidate_lastModifiedTime,

    -- ** AutoMLCandidateStep
    autoMLCandidateStep_candidateStepType,
    autoMLCandidateStep_candidateStepArn,
    autoMLCandidateStep_candidateStepName,

    -- ** AutoMLChannel
    autoMLChannel_compressionType,
    autoMLChannel_dataSource,
    autoMLChannel_targetAttributeName,

    -- ** AutoMLContainerDefinition
    autoMLContainerDefinition_environment,
    autoMLContainerDefinition_image,
    autoMLContainerDefinition_modelDataUrl,

    -- ** AutoMLDataSource
    autoMLDataSource_s3DataSource,

    -- ** AutoMLJobArtifacts
    autoMLJobArtifacts_candidateDefinitionNotebookLocation,
    autoMLJobArtifacts_dataExplorationNotebookLocation,

    -- ** AutoMLJobCompletionCriteria
    autoMLJobCompletionCriteria_maxRuntimePerTrainingJobInSeconds,
    autoMLJobCompletionCriteria_maxAutoMLJobRuntimeInSeconds,
    autoMLJobCompletionCriteria_maxCandidates,

    -- ** AutoMLJobConfig
    autoMLJobConfig_securityConfig,
    autoMLJobConfig_completionCriteria,

    -- ** AutoMLJobObjective
    autoMLJobObjective_metricName,

    -- ** AutoMLJobSummary
    autoMLJobSummary_endTime,
    autoMLJobSummary_failureReason,
    autoMLJobSummary_autoMLJobName,
    autoMLJobSummary_autoMLJobArn,
    autoMLJobSummary_autoMLJobStatus,
    autoMLJobSummary_autoMLJobSecondaryStatus,
    autoMLJobSummary_creationTime,
    autoMLJobSummary_lastModifiedTime,

    -- ** AutoMLOutputDataConfig
    autoMLOutputDataConfig_kmsKeyId,
    autoMLOutputDataConfig_s3OutputPath,

    -- ** AutoMLS3DataSource
    autoMLS3DataSource_s3DataType,
    autoMLS3DataSource_s3Uri,

    -- ** AutoMLSecurityConfig
    autoMLSecurityConfig_vpcConfig,
    autoMLSecurityConfig_enableInterContainerTrafficEncryption,
    autoMLSecurityConfig_volumeKmsKeyId,

    -- ** AutoRollbackConfig
    autoRollbackConfig_alarms,

    -- ** Bias
    bias_report,

    -- ** BlueGreenUpdatePolicy
    blueGreenUpdatePolicy_terminationWaitInSeconds,
    blueGreenUpdatePolicy_maximumExecutionTimeoutInSeconds,
    blueGreenUpdatePolicy_trafficRoutingConfiguration,

    -- ** CacheHitResult
    cacheHitResult_sourcePipelineExecutionArn,

    -- ** CapacitySize
    capacitySize_type,
    capacitySize_value,

    -- ** CaptureContentTypeHeader
    captureContentTypeHeader_csvContentTypes,
    captureContentTypeHeader_jsonContentTypes,

    -- ** CaptureOption
    captureOption_captureMode,

    -- ** CategoricalParameterRange
    categoricalParameterRange_name,
    categoricalParameterRange_values,

    -- ** CategoricalParameterRangeSpecification
    categoricalParameterRangeSpecification_values,

    -- ** Channel
    channel_contentType,
    channel_recordWrapperType,
    channel_shuffleConfig,
    channel_compressionType,
    channel_inputMode,
    channel_channelName,
    channel_dataSource,

    -- ** ChannelSpecification
    channelSpecification_description,
    channelSpecification_isRequired,
    channelSpecification_supportedCompressionTypes,
    channelSpecification_name,
    channelSpecification_supportedContentTypes,
    channelSpecification_supportedInputModes,

    -- ** CheckpointConfig
    checkpointConfig_localPath,
    checkpointConfig_s3Uri,

    -- ** CodeRepositorySummary
    codeRepositorySummary_gitConfig,
    codeRepositorySummary_codeRepositoryName,
    codeRepositorySummary_codeRepositoryArn,
    codeRepositorySummary_creationTime,
    codeRepositorySummary_lastModifiedTime,

    -- ** CognitoConfig
    cognitoConfig_userPool,
    cognitoConfig_clientId,

    -- ** CognitoMemberDefinition
    cognitoMemberDefinition_userPool,
    cognitoMemberDefinition_userGroup,
    cognitoMemberDefinition_clientId,

    -- ** CollectionConfiguration
    collectionConfiguration_collectionParameters,
    collectionConfiguration_collectionName,

    -- ** CompilationJobSummary
    compilationJobSummary_compilationTargetPlatformArch,
    compilationJobSummary_compilationStartTime,
    compilationJobSummary_compilationTargetPlatformOs,
    compilationJobSummary_compilationTargetPlatformAccelerator,
    compilationJobSummary_lastModifiedTime,
    compilationJobSummary_compilationEndTime,
    compilationJobSummary_compilationTargetDevice,
    compilationJobSummary_compilationJobName,
    compilationJobSummary_compilationJobArn,
    compilationJobSummary_creationTime,
    compilationJobSummary_compilationJobStatus,

    -- ** ConditionStepMetadata
    conditionStepMetadata_outcome,

    -- ** ContainerDefinition
    containerDefinition_multiModelConfig,
    containerDefinition_modelDataUrl,
    containerDefinition_mode,
    containerDefinition_containerHostname,
    containerDefinition_imageConfig,
    containerDefinition_environment,
    containerDefinition_modelPackageName,
    containerDefinition_image,

    -- ** ContextSource
    contextSource_sourceId,
    contextSource_sourceType,
    contextSource_sourceUri,

    -- ** ContextSummary
    contextSummary_contextType,
    contextSummary_creationTime,
    contextSummary_contextName,
    contextSummary_source,
    contextSummary_lastModifiedTime,
    contextSummary_contextArn,

    -- ** ContinuousParameterRange
    continuousParameterRange_scalingType,
    continuousParameterRange_name,
    continuousParameterRange_minValue,
    continuousParameterRange_maxValue,

    -- ** ContinuousParameterRangeSpecification
    continuousParameterRangeSpecification_minValue,
    continuousParameterRangeSpecification_maxValue,

    -- ** CustomImage
    customImage_imageVersionNumber,
    customImage_imageName,
    customImage_appImageConfigName,

    -- ** DataCaptureConfig
    dataCaptureConfig_captureContentTypeHeader,
    dataCaptureConfig_kmsKeyId,
    dataCaptureConfig_enableCapture,
    dataCaptureConfig_initialSamplingPercentage,
    dataCaptureConfig_destinationS3Uri,
    dataCaptureConfig_captureOptions,

    -- ** DataCaptureConfigSummary
    dataCaptureConfigSummary_enableCapture,
    dataCaptureConfigSummary_captureStatus,
    dataCaptureConfigSummary_currentSamplingPercentage,
    dataCaptureConfigSummary_destinationS3Uri,
    dataCaptureConfigSummary_kmsKeyId,

    -- ** DataCatalogConfig
    dataCatalogConfig_tableName,
    dataCatalogConfig_catalog,
    dataCatalogConfig_database,

    -- ** DataProcessing
    dataProcessing_outputFilter,
    dataProcessing_joinSource,
    dataProcessing_inputFilter,

    -- ** DataQualityAppSpecification
    dataQualityAppSpecification_containerArguments,
    dataQualityAppSpecification_containerEntrypoint,
    dataQualityAppSpecification_postAnalyticsProcessorSourceUri,
    dataQualityAppSpecification_environment,
    dataQualityAppSpecification_recordPreprocessorSourceUri,
    dataQualityAppSpecification_imageUri,

    -- ** DataQualityBaselineConfig
    dataQualityBaselineConfig_statisticsResource,
    dataQualityBaselineConfig_constraintsResource,
    dataQualityBaselineConfig_baseliningJobName,

    -- ** DataQualityJobInput
    dataQualityJobInput_endpointInput,

    -- ** DataSource
    dataSource_fileSystemDataSource,
    dataSource_s3DataSource,

    -- ** DatasetDefinition
    datasetDefinition_redshiftDatasetDefinition,
    datasetDefinition_athenaDatasetDefinition,
    datasetDefinition_localPath,
    datasetDefinition_inputMode,
    datasetDefinition_dataDistributionType,

    -- ** DebugHookConfig
    debugHookConfig_collectionConfigurations,
    debugHookConfig_hookParameters,
    debugHookConfig_localPath,
    debugHookConfig_s3OutputPath,

    -- ** DebugRuleConfiguration
    debugRuleConfiguration_ruleParameters,
    debugRuleConfiguration_instanceType,
    debugRuleConfiguration_s3OutputPath,
    debugRuleConfiguration_volumeSizeInGB,
    debugRuleConfiguration_localPath,
    debugRuleConfiguration_ruleConfigurationName,
    debugRuleConfiguration_ruleEvaluatorImage,

    -- ** DebugRuleEvaluationStatus
    debugRuleEvaluationStatus_ruleConfigurationName,
    debugRuleEvaluationStatus_statusDetails,
    debugRuleEvaluationStatus_ruleEvaluationStatus,
    debugRuleEvaluationStatus_lastModifiedTime,
    debugRuleEvaluationStatus_ruleEvaluationJobArn,

    -- ** DeployedImage
    deployedImage_specifiedImage,
    deployedImage_resolvedImage,
    deployedImage_resolutionTime,

    -- ** DeploymentConfig
    deploymentConfig_autoRollbackConfiguration,
    deploymentConfig_blueGreenUpdatePolicy,

    -- ** DesiredWeightAndCapacity
    desiredWeightAndCapacity_desiredInstanceCount,
    desiredWeightAndCapacity_desiredWeight,
    desiredWeightAndCapacity_variantName,

    -- ** Device
    device_iotThingName,
    device_description,
    device_deviceName,

    -- ** DeviceFleetSummary
    deviceFleetSummary_creationTime,
    deviceFleetSummary_lastModifiedTime,
    deviceFleetSummary_deviceFleetArn,
    deviceFleetSummary_deviceFleetName,

    -- ** DeviceStats
    deviceStats_connectedDeviceCount,
    deviceStats_registeredDeviceCount,

    -- ** DeviceSummary
    deviceSummary_deviceFleetName,
    deviceSummary_latestHeartbeat,
    deviceSummary_registrationTime,
    deviceSummary_models,
    deviceSummary_iotThingName,
    deviceSummary_description,
    deviceSummary_deviceName,
    deviceSummary_deviceArn,

    -- ** DomainDetails
    domainDetails_status,
    domainDetails_creationTime,
    domainDetails_domainId,
    domainDetails_domainArn,
    domainDetails_domainName,
    domainDetails_lastModifiedTime,
    domainDetails_url,

    -- ** EdgeModel
    edgeModel_latestInference,
    edgeModel_latestSampleTime,
    edgeModel_modelName,
    edgeModel_modelVersion,

    -- ** EdgeModelStat
    edgeModelStat_modelName,
    edgeModelStat_modelVersion,
    edgeModelStat_offlineDeviceCount,
    edgeModelStat_connectedDeviceCount,
    edgeModelStat_activeDeviceCount,
    edgeModelStat_samplingDeviceCount,

    -- ** EdgeModelSummary
    edgeModelSummary_modelName,
    edgeModelSummary_modelVersion,

    -- ** EdgeOutputConfig
    edgeOutputConfig_kmsKeyId,
    edgeOutputConfig_s3OutputLocation,

    -- ** EdgePackagingJobSummary
    edgePackagingJobSummary_creationTime,
    edgePackagingJobSummary_compilationJobName,
    edgePackagingJobSummary_modelVersion,
    edgePackagingJobSummary_lastModifiedTime,
    edgePackagingJobSummary_modelName,
    edgePackagingJobSummary_edgePackagingJobArn,
    edgePackagingJobSummary_edgePackagingJobName,
    edgePackagingJobSummary_edgePackagingJobStatus,

    -- ** Endpoint
    endpoint_productionVariants,
    endpoint_monitoringSchedules,
    endpoint_failureReason,
    endpoint_tags,
    endpoint_dataCaptureConfig,
    endpoint_endpointName,
    endpoint_endpointArn,
    endpoint_endpointConfigName,
    endpoint_endpointStatus,
    endpoint_creationTime,
    endpoint_lastModifiedTime,

    -- ** EndpointConfigSummary
    endpointConfigSummary_endpointConfigName,
    endpointConfigSummary_endpointConfigArn,
    endpointConfigSummary_creationTime,

    -- ** EndpointInput
    endpointInput_endTimeOffset,
    endpointInput_inferenceAttribute,
    endpointInput_s3InputMode,
    endpointInput_s3DataDistributionType,
    endpointInput_probabilityAttribute,
    endpointInput_probabilityThresholdAttribute,
    endpointInput_featuresAttribute,
    endpointInput_startTimeOffset,
    endpointInput_endpointName,
    endpointInput_localPath,

    -- ** EndpointSummary
    endpointSummary_endpointName,
    endpointSummary_endpointArn,
    endpointSummary_creationTime,
    endpointSummary_lastModifiedTime,
    endpointSummary_endpointStatus,

    -- ** Experiment
    experiment_experimentArn,
    experiment_creationTime,
    experiment_source,
    experiment_tags,
    experiment_lastModifiedTime,
    experiment_experimentName,
    experiment_description,
    experiment_createdBy,
    experiment_lastModifiedBy,
    experiment_displayName,

    -- ** ExperimentConfig
    experimentConfig_experimentName,
    experimentConfig_trialComponentDisplayName,
    experimentConfig_trialName,

    -- ** ExperimentSource
    experimentSource_sourceType,
    experimentSource_sourceArn,

    -- ** ExperimentSummary
    experimentSummary_experimentArn,
    experimentSummary_creationTime,
    experimentSummary_experimentSource,
    experimentSummary_lastModifiedTime,
    experimentSummary_experimentName,
    experimentSummary_displayName,

    -- ** Explainability
    explainability_report,

    -- ** FeatureDefinition
    featureDefinition_featureType,
    featureDefinition_featureName,

    -- ** FeatureGroup
    featureGroup_featureGroupStatus,
    featureGroup_offlineStoreConfig,
    featureGroup_creationTime,
    featureGroup_roleArn,
    featureGroup_featureGroupArn,
    featureGroup_recordIdentifierFeatureName,
    featureGroup_featureDefinitions,
    featureGroup_offlineStoreStatus,
    featureGroup_eventTimeFeatureName,
    featureGroup_featureGroupName,
    featureGroup_failureReason,
    featureGroup_tags,
    featureGroup_description,
    featureGroup_onlineStoreConfig,

    -- ** FeatureGroupSummary
    featureGroupSummary_featureGroupStatus,
    featureGroupSummary_offlineStoreStatus,
    featureGroupSummary_featureGroupName,
    featureGroupSummary_featureGroupArn,
    featureGroupSummary_creationTime,

    -- ** FileSystemConfig
    fileSystemConfig_defaultGid,
    fileSystemConfig_mountPath,
    fileSystemConfig_defaultUid,

    -- ** FileSystemDataSource
    fileSystemDataSource_fileSystemId,
    fileSystemDataSource_fileSystemAccessMode,
    fileSystemDataSource_fileSystemType,
    fileSystemDataSource_directoryPath,

    -- ** Filter
    filter_operator,
    filter_value,
    filter_name,

    -- ** FinalAutoMLJobObjectiveMetric
    finalAutoMLJobObjectiveMetric_type,
    finalAutoMLJobObjectiveMetric_metricName,
    finalAutoMLJobObjectiveMetric_value,

    -- ** FinalHyperParameterTuningJobObjectiveMetric
    finalHyperParameterTuningJobObjectiveMetric_type,
    finalHyperParameterTuningJobObjectiveMetric_metricName,
    finalHyperParameterTuningJobObjectiveMetric_value,

    -- ** FlowDefinitionOutputConfig
    flowDefinitionOutputConfig_kmsKeyId,
    flowDefinitionOutputConfig_s3OutputPath,

    -- ** FlowDefinitionSummary
    flowDefinitionSummary_failureReason,
    flowDefinitionSummary_flowDefinitionName,
    flowDefinitionSummary_flowDefinitionArn,
    flowDefinitionSummary_flowDefinitionStatus,
    flowDefinitionSummary_creationTime,

    -- ** GitConfig
    gitConfig_secretArn,
    gitConfig_branch,
    gitConfig_repositoryUrl,

    -- ** GitConfigForUpdate
    gitConfigForUpdate_secretArn,

    -- ** HumanLoopActivationConditionsConfig
    humanLoopActivationConditionsConfig_humanLoopActivationConditions,

    -- ** HumanLoopActivationConfig
    humanLoopActivationConfig_humanLoopActivationConditionsConfig,

    -- ** HumanLoopConfig
    humanLoopConfig_taskKeywords,
    humanLoopConfig_taskTimeLimitInSeconds,
    humanLoopConfig_taskAvailabilityLifetimeInSeconds,
    humanLoopConfig_publicWorkforceTaskPrice,
    humanLoopConfig_workteamArn,
    humanLoopConfig_humanTaskUiArn,
    humanLoopConfig_taskTitle,
    humanLoopConfig_taskDescription,
    humanLoopConfig_taskCount,

    -- ** HumanLoopRequestSource
    humanLoopRequestSource_awsManagedHumanLoopRequestSource,

    -- ** HumanTaskConfig
    humanTaskConfig_taskKeywords,
    humanTaskConfig_taskAvailabilityLifetimeInSeconds,
    humanTaskConfig_maxConcurrentTaskCount,
    humanTaskConfig_publicWorkforceTaskPrice,
    humanTaskConfig_workteamArn,
    humanTaskConfig_uiConfig,
    humanTaskConfig_preHumanTaskLambdaArn,
    humanTaskConfig_taskTitle,
    humanTaskConfig_taskDescription,
    humanTaskConfig_numberOfHumanWorkersPerDataObject,
    humanTaskConfig_taskTimeLimitInSeconds,
    humanTaskConfig_annotationConsolidationConfig,

    -- ** HumanTaskUiSummary
    humanTaskUiSummary_humanTaskUiName,
    humanTaskUiSummary_humanTaskUiArn,
    humanTaskUiSummary_creationTime,

    -- ** HyperParameterAlgorithmSpecification
    hyperParameterAlgorithmSpecification_trainingImage,
    hyperParameterAlgorithmSpecification_metricDefinitions,
    hyperParameterAlgorithmSpecification_algorithmName,
    hyperParameterAlgorithmSpecification_trainingInputMode,

    -- ** HyperParameterSpecification
    hyperParameterSpecification_range,
    hyperParameterSpecification_isTunable,
    hyperParameterSpecification_description,
    hyperParameterSpecification_isRequired,
    hyperParameterSpecification_defaultValue,
    hyperParameterSpecification_name,
    hyperParameterSpecification_type,

    -- ** HyperParameterTrainingJobDefinition
    hyperParameterTrainingJobDefinition_vpcConfig,
    hyperParameterTrainingJobDefinition_inputDataConfig,
    hyperParameterTrainingJobDefinition_enableManagedSpotTraining,
    hyperParameterTrainingJobDefinition_staticHyperParameters,
    hyperParameterTrainingJobDefinition_hyperParameterRanges,
    hyperParameterTrainingJobDefinition_enableNetworkIsolation,
    hyperParameterTrainingJobDefinition_enableInterContainerTrafficEncryption,
    hyperParameterTrainingJobDefinition_checkpointConfig,
    hyperParameterTrainingJobDefinition_tuningObjective,
    hyperParameterTrainingJobDefinition_definitionName,
    hyperParameterTrainingJobDefinition_algorithmSpecification,
    hyperParameterTrainingJobDefinition_roleArn,
    hyperParameterTrainingJobDefinition_outputDataConfig,
    hyperParameterTrainingJobDefinition_resourceConfig,
    hyperParameterTrainingJobDefinition_stoppingCondition,

    -- ** HyperParameterTrainingJobSummary
    hyperParameterTrainingJobSummary_finalHyperParameterTuningJobObjectiveMetric,
    hyperParameterTrainingJobSummary_tuningJobName,
    hyperParameterTrainingJobSummary_failureReason,
    hyperParameterTrainingJobSummary_objectiveStatus,
    hyperParameterTrainingJobSummary_trainingJobDefinitionName,
    hyperParameterTrainingJobSummary_trainingStartTime,
    hyperParameterTrainingJobSummary_trainingEndTime,
    hyperParameterTrainingJobSummary_trainingJobName,
    hyperParameterTrainingJobSummary_trainingJobArn,
    hyperParameterTrainingJobSummary_creationTime,
    hyperParameterTrainingJobSummary_trainingJobStatus,
    hyperParameterTrainingJobSummary_tunedHyperParameters,

    -- ** HyperParameterTuningJobConfig
    hyperParameterTuningJobConfig_hyperParameterTuningJobObjective,
    hyperParameterTuningJobConfig_parameterRanges,
    hyperParameterTuningJobConfig_tuningJobCompletionCriteria,
    hyperParameterTuningJobConfig_trainingJobEarlyStoppingType,
    hyperParameterTuningJobConfig_strategy,
    hyperParameterTuningJobConfig_resourceLimits,

    -- ** HyperParameterTuningJobObjective
    hyperParameterTuningJobObjective_type,
    hyperParameterTuningJobObjective_metricName,

    -- ** HyperParameterTuningJobSummary
    hyperParameterTuningJobSummary_resourceLimits,
    hyperParameterTuningJobSummary_hyperParameterTuningEndTime,
    hyperParameterTuningJobSummary_lastModifiedTime,
    hyperParameterTuningJobSummary_hyperParameterTuningJobName,
    hyperParameterTuningJobSummary_hyperParameterTuningJobArn,
    hyperParameterTuningJobSummary_hyperParameterTuningJobStatus,
    hyperParameterTuningJobSummary_strategy,
    hyperParameterTuningJobSummary_creationTime,
    hyperParameterTuningJobSummary_trainingJobStatusCounters,
    hyperParameterTuningJobSummary_objectiveStatusCounters,

    -- ** HyperParameterTuningJobWarmStartConfig
    hyperParameterTuningJobWarmStartConfig_parentHyperParameterTuningJobs,
    hyperParameterTuningJobWarmStartConfig_warmStartType,

    -- ** Image
    image_failureReason,
    image_description,
    image_displayName,
    image_creationTime,
    image_imageArn,
    image_imageName,
    image_imageStatus,
    image_lastModifiedTime,

    -- ** ImageConfig
    imageConfig_repositoryAccessMode,

    -- ** ImageVersion
    imageVersion_failureReason,
    imageVersion_creationTime,
    imageVersion_imageArn,
    imageVersion_imageVersionArn,
    imageVersion_imageVersionStatus,
    imageVersion_lastModifiedTime,
    imageVersion_version,

    -- ** InferenceExecutionConfig
    inferenceExecutionConfig_mode,

    -- ** InferenceSpecification
    inferenceSpecification_supportedTransformInstanceTypes,
    inferenceSpecification_supportedRealtimeInferenceInstanceTypes,
    inferenceSpecification_containers,
    inferenceSpecification_supportedContentTypes,
    inferenceSpecification_supportedResponseMIMETypes,

    -- ** InputConfig
    inputConfig_frameworkVersion,
    inputConfig_s3Uri,
    inputConfig_dataInputConfig,
    inputConfig_framework,

    -- ** IntegerParameterRange
    integerParameterRange_scalingType,
    integerParameterRange_name,
    integerParameterRange_minValue,
    integerParameterRange_maxValue,

    -- ** IntegerParameterRangeSpecification
    integerParameterRangeSpecification_minValue,
    integerParameterRangeSpecification_maxValue,

    -- ** JupyterServerAppSettings
    jupyterServerAppSettings_defaultResourceSpec,

    -- ** KernelGatewayAppSettings
    kernelGatewayAppSettings_customImages,
    kernelGatewayAppSettings_defaultResourceSpec,

    -- ** KernelGatewayImageConfig
    kernelGatewayImageConfig_fileSystemConfig,
    kernelGatewayImageConfig_kernelSpecs,

    -- ** KernelSpec
    kernelSpec_displayName,
    kernelSpec_name,

    -- ** LabelCounters
    labelCounters_unlabeled,
    labelCounters_failedNonRetryableError,
    labelCounters_machineLabeled,
    labelCounters_humanLabeled,
    labelCounters_totalLabeled,

    -- ** LabelCountersForWorkteam
    labelCountersForWorkteam_pendingHuman,
    labelCountersForWorkteam_total,
    labelCountersForWorkteam_humanLabeled,

    -- ** LabelingJobAlgorithmsConfig
    labelingJobAlgorithmsConfig_initialActiveLearningModelArn,
    labelingJobAlgorithmsConfig_labelingJobResourceConfig,
    labelingJobAlgorithmsConfig_labelingJobAlgorithmSpecificationArn,

    -- ** LabelingJobDataAttributes
    labelingJobDataAttributes_contentClassifiers,

    -- ** LabelingJobDataSource
    labelingJobDataSource_snsDataSource,
    labelingJobDataSource_s3DataSource,

    -- ** LabelingJobForWorkteamSummary
    labelingJobForWorkteamSummary_labelCounters,
    labelingJobForWorkteamSummary_labelingJobName,
    labelingJobForWorkteamSummary_numberOfHumanWorkersPerDataObject,
    labelingJobForWorkteamSummary_jobReferenceCode,
    labelingJobForWorkteamSummary_workRequesterAccountId,
    labelingJobForWorkteamSummary_creationTime,

    -- ** LabelingJobInputConfig
    labelingJobInputConfig_dataAttributes,
    labelingJobInputConfig_dataSource,

    -- ** LabelingJobOutput
    labelingJobOutput_finalActiveLearningModelArn,
    labelingJobOutput_outputDatasetS3Uri,

    -- ** LabelingJobOutputConfig
    labelingJobOutputConfig_kmsKeyId,
    labelingJobOutputConfig_snsTopicArn,
    labelingJobOutputConfig_s3OutputPath,

    -- ** LabelingJobResourceConfig
    labelingJobResourceConfig_volumeKmsKeyId,

    -- ** LabelingJobS3DataSource
    labelingJobS3DataSource_manifestS3Uri,

    -- ** LabelingJobSnsDataSource
    labelingJobSnsDataSource_snsTopicArn,

    -- ** LabelingJobStoppingConditions
    labelingJobStoppingConditions_maxPercentageOfInputDatasetLabeled,
    labelingJobStoppingConditions_maxHumanLabeledObjectCount,

    -- ** LabelingJobSummary
    labelingJobSummary_annotationConsolidationLambdaArn,
    labelingJobSummary_inputConfig,
    labelingJobSummary_failureReason,
    labelingJobSummary_labelingJobOutput,
    labelingJobSummary_labelingJobName,
    labelingJobSummary_labelingJobArn,
    labelingJobSummary_creationTime,
    labelingJobSummary_lastModifiedTime,
    labelingJobSummary_labelingJobStatus,
    labelingJobSummary_labelCounters,
    labelingJobSummary_workteamArn,
    labelingJobSummary_preHumanTaskLambdaArn,

    -- ** MemberDefinition
    memberDefinition_oidcMemberDefinition,
    memberDefinition_cognitoMemberDefinition,

    -- ** MetadataProperties
    metadataProperties_generatedBy,
    metadataProperties_commitId,
    metadataProperties_projectId,
    metadataProperties_repository,

    -- ** MetricData
    metricData_metricName,
    metricData_timestamp,
    metricData_value,

    -- ** MetricDefinition
    metricDefinition_name,
    metricDefinition_regex,

    -- ** MetricsSource
    metricsSource_contentDigest,
    metricsSource_contentType,
    metricsSource_s3Uri,

    -- ** ModelArtifacts
    modelArtifacts_s3ModelArtifacts,

    -- ** ModelBiasAppSpecification
    modelBiasAppSpecification_environment,
    modelBiasAppSpecification_imageUri,
    modelBiasAppSpecification_configUri,

    -- ** ModelBiasBaselineConfig
    modelBiasBaselineConfig_constraintsResource,
    modelBiasBaselineConfig_baseliningJobName,

    -- ** ModelBiasJobInput
    modelBiasJobInput_endpointInput,
    modelBiasJobInput_groundTruthS3Input,

    -- ** ModelClientConfig
    modelClientConfig_invocationsTimeoutInSeconds,
    modelClientConfig_invocationsMaxRetries,

    -- ** ModelDataQuality
    modelDataQuality_constraints,
    modelDataQuality_statistics,

    -- ** ModelDigests
    modelDigests_artifactDigest,

    -- ** ModelExplainabilityAppSpecification
    modelExplainabilityAppSpecification_environment,
    modelExplainabilityAppSpecification_imageUri,
    modelExplainabilityAppSpecification_configUri,

    -- ** ModelExplainabilityBaselineConfig
    modelExplainabilityBaselineConfig_constraintsResource,
    modelExplainabilityBaselineConfig_baseliningJobName,

    -- ** ModelExplainabilityJobInput
    modelExplainabilityJobInput_endpointInput,

    -- ** ModelMetrics
    modelMetrics_bias,
    modelMetrics_explainability,
    modelMetrics_modelDataQuality,
    modelMetrics_modelQuality,

    -- ** ModelPackage
    modelPackage_sourceAlgorithmSpecification,
    modelPackage_modelPackageVersion,
    modelPackage_metadataProperties,
    modelPackage_creationTime,
    modelPackage_validationSpecification,
    modelPackage_modelPackageStatusDetails,
    modelPackage_modelMetrics,
    modelPackage_certifyForMarketplace,
    modelPackage_modelPackageName,
    modelPackage_modelApprovalStatus,
    modelPackage_approvalDescription,
    modelPackage_tags,
    modelPackage_modelPackageStatus,
    modelPackage_lastModifiedTime,
    modelPackage_inferenceSpecification,
    modelPackage_modelPackageDescription,
    modelPackage_createdBy,
    modelPackage_modelPackageArn,
    modelPackage_lastModifiedBy,
    modelPackage_modelPackageGroupName,

    -- ** ModelPackageContainerDefinition
    modelPackageContainerDefinition_imageDigest,
    modelPackageContainerDefinition_modelDataUrl,
    modelPackageContainerDefinition_containerHostname,
    modelPackageContainerDefinition_productId,
    modelPackageContainerDefinition_image,

    -- ** ModelPackageGroup
    modelPackageGroup_modelPackageGroupArn,
    modelPackageGroup_creationTime,
    modelPackageGroup_modelPackageGroupDescription,
    modelPackageGroup_tags,
    modelPackageGroup_modelPackageGroupStatus,
    modelPackageGroup_createdBy,
    modelPackageGroup_modelPackageGroupName,

    -- ** ModelPackageGroupSummary
    modelPackageGroupSummary_modelPackageGroupDescription,
    modelPackageGroupSummary_modelPackageGroupName,
    modelPackageGroupSummary_modelPackageGroupArn,
    modelPackageGroupSummary_creationTime,
    modelPackageGroupSummary_modelPackageGroupStatus,

    -- ** ModelPackageStatusDetails
    modelPackageStatusDetails_imageScanStatuses,
    modelPackageStatusDetails_validationStatuses,

    -- ** ModelPackageStatusItem
    modelPackageStatusItem_failureReason,
    modelPackageStatusItem_name,
    modelPackageStatusItem_status,

    -- ** ModelPackageSummary
    modelPackageSummary_modelPackageVersion,
    modelPackageSummary_modelApprovalStatus,
    modelPackageSummary_modelPackageDescription,
    modelPackageSummary_modelPackageGroupName,
    modelPackageSummary_modelPackageName,
    modelPackageSummary_modelPackageArn,
    modelPackageSummary_creationTime,
    modelPackageSummary_modelPackageStatus,

    -- ** ModelPackageValidationProfile
    modelPackageValidationProfile_profileName,
    modelPackageValidationProfile_transformJobDefinition,

    -- ** ModelPackageValidationSpecification
    modelPackageValidationSpecification_validationRole,
    modelPackageValidationSpecification_validationProfiles,

    -- ** ModelQuality
    modelQuality_constraints,
    modelQuality_statistics,

    -- ** ModelQualityAppSpecification
    modelQualityAppSpecification_containerArguments,
    modelQualityAppSpecification_containerEntrypoint,
    modelQualityAppSpecification_postAnalyticsProcessorSourceUri,
    modelQualityAppSpecification_environment,
    modelQualityAppSpecification_recordPreprocessorSourceUri,
    modelQualityAppSpecification_problemType,
    modelQualityAppSpecification_imageUri,

    -- ** ModelQualityBaselineConfig
    modelQualityBaselineConfig_constraintsResource,
    modelQualityBaselineConfig_baseliningJobName,

    -- ** ModelQualityJobInput
    modelQualityJobInput_endpointInput,
    modelQualityJobInput_groundTruthS3Input,

    -- ** ModelStepMetadata
    modelStepMetadata_arn,

    -- ** ModelSummary
    modelSummary_modelName,
    modelSummary_modelArn,
    modelSummary_creationTime,

    -- ** MonitoringAppSpecification
    monitoringAppSpecification_containerArguments,
    monitoringAppSpecification_containerEntrypoint,
    monitoringAppSpecification_postAnalyticsProcessorSourceUri,
    monitoringAppSpecification_recordPreprocessorSourceUri,
    monitoringAppSpecification_imageUri,

    -- ** MonitoringBaselineConfig
    monitoringBaselineConfig_statisticsResource,
    monitoringBaselineConfig_constraintsResource,
    monitoringBaselineConfig_baseliningJobName,

    -- ** MonitoringClusterConfig
    monitoringClusterConfig_volumeKmsKeyId,
    monitoringClusterConfig_instanceCount,
    monitoringClusterConfig_instanceType,
    monitoringClusterConfig_volumeSizeInGB,

    -- ** MonitoringConstraintsResource
    monitoringConstraintsResource_s3Uri,

    -- ** MonitoringExecutionSummary
    monitoringExecutionSummary_endpointName,
    monitoringExecutionSummary_monitoringType,
    monitoringExecutionSummary_monitoringJobDefinitionName,
    monitoringExecutionSummary_failureReason,
    monitoringExecutionSummary_processingJobArn,
    monitoringExecutionSummary_monitoringScheduleName,
    monitoringExecutionSummary_scheduledTime,
    monitoringExecutionSummary_creationTime,
    monitoringExecutionSummary_lastModifiedTime,
    monitoringExecutionSummary_monitoringExecutionStatus,

    -- ** MonitoringGroundTruthS3Input
    monitoringGroundTruthS3Input_s3Uri,

    -- ** MonitoringInput
    monitoringInput_endpointInput,

    -- ** MonitoringJobDefinition
    monitoringJobDefinition_networkConfig,
    monitoringJobDefinition_environment,
    monitoringJobDefinition_baselineConfig,
    monitoringJobDefinition_stoppingCondition,
    monitoringJobDefinition_monitoringInputs,
    monitoringJobDefinition_monitoringOutputConfig,
    monitoringJobDefinition_monitoringResources,
    monitoringJobDefinition_monitoringAppSpecification,
    monitoringJobDefinition_roleArn,

    -- ** MonitoringJobDefinitionSummary
    monitoringJobDefinitionSummary_monitoringJobDefinitionName,
    monitoringJobDefinitionSummary_monitoringJobDefinitionArn,
    monitoringJobDefinitionSummary_creationTime,
    monitoringJobDefinitionSummary_endpointName,

    -- ** MonitoringNetworkConfig
    monitoringNetworkConfig_vpcConfig,
    monitoringNetworkConfig_enableNetworkIsolation,
    monitoringNetworkConfig_enableInterContainerTrafficEncryption,

    -- ** MonitoringOutput
    monitoringOutput_s3Output,

    -- ** MonitoringOutputConfig
    monitoringOutputConfig_kmsKeyId,
    monitoringOutputConfig_monitoringOutputs,

    -- ** MonitoringResources
    monitoringResources_clusterConfig,

    -- ** MonitoringS3Output
    monitoringS3Output_s3UploadMode,
    monitoringS3Output_s3Uri,
    monitoringS3Output_localPath,

    -- ** MonitoringSchedule
    monitoringSchedule_endpointName,
    monitoringSchedule_creationTime,
    monitoringSchedule_monitoringType,
    monitoringSchedule_monitoringScheduleName,
    monitoringSchedule_monitoringScheduleStatus,
    monitoringSchedule_failureReason,
    monitoringSchedule_monitoringScheduleArn,
    monitoringSchedule_tags,
    monitoringSchedule_lastModifiedTime,
    monitoringSchedule_monitoringScheduleConfig,
    monitoringSchedule_lastMonitoringExecutionSummary,

    -- ** MonitoringScheduleConfig
    monitoringScheduleConfig_scheduleConfig,
    monitoringScheduleConfig_monitoringType,
    monitoringScheduleConfig_monitoringJobDefinitionName,
    monitoringScheduleConfig_monitoringJobDefinition,

    -- ** MonitoringScheduleSummary
    monitoringScheduleSummary_endpointName,
    monitoringScheduleSummary_monitoringType,
    monitoringScheduleSummary_monitoringJobDefinitionName,
    monitoringScheduleSummary_monitoringScheduleName,
    monitoringScheduleSummary_monitoringScheduleArn,
    monitoringScheduleSummary_creationTime,
    monitoringScheduleSummary_lastModifiedTime,
    monitoringScheduleSummary_monitoringScheduleStatus,

    -- ** MonitoringStatisticsResource
    monitoringStatisticsResource_s3Uri,

    -- ** MonitoringStoppingCondition
    monitoringStoppingCondition_maxRuntimeInSeconds,

    -- ** MultiModelConfig
    multiModelConfig_modelCacheSetting,

    -- ** NestedFilters
    nestedFilters_nestedPropertyName,
    nestedFilters_filters,

    -- ** NetworkConfig
    networkConfig_vpcConfig,
    networkConfig_enableNetworkIsolation,
    networkConfig_enableInterContainerTrafficEncryption,

    -- ** NotebookInstanceLifecycleConfigSummary
    notebookInstanceLifecycleConfigSummary_creationTime,
    notebookInstanceLifecycleConfigSummary_lastModifiedTime,
    notebookInstanceLifecycleConfigSummary_notebookInstanceLifecycleConfigName,
    notebookInstanceLifecycleConfigSummary_notebookInstanceLifecycleConfigArn,

    -- ** NotebookInstanceLifecycleHook
    notebookInstanceLifecycleHook_content,

    -- ** NotebookInstanceSummary
    notebookInstanceSummary_creationTime,
    notebookInstanceSummary_defaultCodeRepository,
    notebookInstanceSummary_instanceType,
    notebookInstanceSummary_notebookInstanceLifecycleConfigName,
    notebookInstanceSummary_additionalCodeRepositories,
    notebookInstanceSummary_notebookInstanceStatus,
    notebookInstanceSummary_lastModifiedTime,
    notebookInstanceSummary_url,
    notebookInstanceSummary_notebookInstanceName,
    notebookInstanceSummary_notebookInstanceArn,

    -- ** NotificationConfiguration
    notificationConfiguration_notificationTopicArn,

    -- ** ObjectiveStatusCounters
    objectiveStatusCounters_succeeded,
    objectiveStatusCounters_pending,
    objectiveStatusCounters_failed,

    -- ** OfflineStoreConfig
    offlineStoreConfig_disableGlueTableCreation,
    offlineStoreConfig_dataCatalogConfig,
    offlineStoreConfig_s3StorageConfig,

    -- ** OfflineStoreStatus
    offlineStoreStatus_blockedReason,
    offlineStoreStatus_status,

    -- ** OidcConfig
    oidcConfig_clientId,
    oidcConfig_clientSecret,
    oidcConfig_issuer,
    oidcConfig_authorizationEndpoint,
    oidcConfig_tokenEndpoint,
    oidcConfig_userInfoEndpoint,
    oidcConfig_logoutEndpoint,
    oidcConfig_jwksUri,

    -- ** OidcConfigForResponse
    oidcConfigForResponse_clientId,
    oidcConfigForResponse_tokenEndpoint,
    oidcConfigForResponse_authorizationEndpoint,
    oidcConfigForResponse_userInfoEndpoint,
    oidcConfigForResponse_logoutEndpoint,
    oidcConfigForResponse_issuer,
    oidcConfigForResponse_jwksUri,

    -- ** OidcMemberDefinition
    oidcMemberDefinition_groups,

    -- ** OnlineStoreConfig
    onlineStoreConfig_securityConfig,
    onlineStoreConfig_enableOnlineStore,

    -- ** OnlineStoreSecurityConfig
    onlineStoreSecurityConfig_kmsKeyId,

    -- ** OutputConfig
    outputConfig_compilerOptions,
    outputConfig_kmsKeyId,
    outputConfig_targetDevice,
    outputConfig_targetPlatform,
    outputConfig_s3OutputLocation,

    -- ** OutputDataConfig
    outputDataConfig_kmsKeyId,
    outputDataConfig_s3OutputPath,

    -- ** Parameter
    parameter_name,
    parameter_value,

    -- ** ParameterRange
    parameterRange_continuousParameterRangeSpecification,
    parameterRange_integerParameterRangeSpecification,
    parameterRange_categoricalParameterRangeSpecification,

    -- ** ParameterRanges
    parameterRanges_integerParameterRanges,
    parameterRanges_categoricalParameterRanges,
    parameterRanges_continuousParameterRanges,

    -- ** Parent
    parent_experimentName,
    parent_trialName,

    -- ** ParentHyperParameterTuningJob
    parentHyperParameterTuningJob_hyperParameterTuningJobName,

    -- ** Pipeline
    pipeline_pipelineArn,
    pipeline_pipelineDescription,
    pipeline_creationTime,
    pipeline_roleArn,
    pipeline_lastRunTime,
    pipeline_pipelineDisplayName,
    pipeline_tags,
    pipeline_lastModifiedTime,
    pipeline_pipelineStatus,
    pipeline_createdBy,
    pipeline_lastModifiedBy,
    pipeline_pipelineName,

    -- ** PipelineExecution
    pipelineExecution_pipelineArn,
    pipelineExecution_creationTime,
    pipelineExecution_pipelineExecutionDescription,
    pipelineExecution_pipelineParameters,
    pipelineExecution_pipelineExecutionDisplayName,
    pipelineExecution_pipelineExecutionStatus,
    pipelineExecution_lastModifiedTime,
    pipelineExecution_createdBy,
    pipelineExecution_lastModifiedBy,
    pipelineExecution_pipelineExecutionArn,

    -- ** PipelineExecutionStep
    pipelineExecutionStep_startTime,
    pipelineExecutionStep_metadata,
    pipelineExecutionStep_endTime,
    pipelineExecutionStep_failureReason,
    pipelineExecutionStep_stepStatus,
    pipelineExecutionStep_cacheHitResult,
    pipelineExecutionStep_stepName,

    -- ** PipelineExecutionStepMetadata
    pipelineExecutionStepMetadata_model,
    pipelineExecutionStepMetadata_processingJob,
    pipelineExecutionStepMetadata_condition,
    pipelineExecutionStepMetadata_trainingJob,
    pipelineExecutionStepMetadata_registerModel,
    pipelineExecutionStepMetadata_transformJob,

    -- ** PipelineExecutionSummary
    pipelineExecutionSummary_pipelineExecutionDescription,
    pipelineExecutionSummary_startTime,
    pipelineExecutionSummary_pipelineExecutionDisplayName,
    pipelineExecutionSummary_pipelineExecutionStatus,
    pipelineExecutionSummary_pipelineExecutionArn,

    -- ** PipelineSummary
    pipelineSummary_pipelineArn,
    pipelineSummary_pipelineDescription,
    pipelineSummary_creationTime,
    pipelineSummary_roleArn,
    pipelineSummary_lastExecutionTime,
    pipelineSummary_pipelineDisplayName,
    pipelineSummary_lastModifiedTime,
    pipelineSummary_pipelineName,

    -- ** ProcessingClusterConfig
    processingClusterConfig_volumeKmsKeyId,
    processingClusterConfig_instanceCount,
    processingClusterConfig_instanceType,
    processingClusterConfig_volumeSizeInGB,

    -- ** ProcessingFeatureStoreOutput
    processingFeatureStoreOutput_featureGroupName,

    -- ** ProcessingInput
    processingInput_datasetDefinition,
    processingInput_appManaged,
    processingInput_s3Input,
    processingInput_inputName,

    -- ** ProcessingJob
    processingJob_networkConfig,
    processingJob_creationTime,
    processingJob_appSpecification,
    processingJob_processingEndTime,
    processingJob_roleArn,
    processingJob_processingOutputConfig,
    processingJob_exitMessage,
    processingJob_experimentConfig,
    processingJob_processingJobStatus,
    processingJob_environment,
    processingJob_autoMLJobArn,
    processingJob_failureReason,
    processingJob_monitoringScheduleArn,
    processingJob_processingJobArn,
    processingJob_tags,
    processingJob_lastModifiedTime,
    processingJob_processingInputs,
    processingJob_processingStartTime,
    processingJob_stoppingCondition,
    processingJob_processingJobName,
    processingJob_processingResources,
    processingJob_trainingJobArn,

    -- ** ProcessingJobStepMetadata
    processingJobStepMetadata_arn,

    -- ** ProcessingJobSummary
    processingJobSummary_processingEndTime,
    processingJobSummary_exitMessage,
    processingJobSummary_failureReason,
    processingJobSummary_lastModifiedTime,
    processingJobSummary_processingJobName,
    processingJobSummary_processingJobArn,
    processingJobSummary_creationTime,
    processingJobSummary_processingJobStatus,

    -- ** ProcessingOutput
    processingOutput_s3Output,
    processingOutput_featureStoreOutput,
    processingOutput_appManaged,
    processingOutput_outputName,

    -- ** ProcessingOutputConfig
    processingOutputConfig_kmsKeyId,
    processingOutputConfig_outputs,

    -- ** ProcessingResources
    processingResources_clusterConfig,

    -- ** ProcessingS3Input
    processingS3Input_s3CompressionType,
    processingS3Input_s3InputMode,
    processingS3Input_s3DataDistributionType,
    processingS3Input_localPath,
    processingS3Input_s3Uri,
    processingS3Input_s3DataType,

    -- ** ProcessingS3Output
    processingS3Output_s3Uri,
    processingS3Output_localPath,
    processingS3Output_s3UploadMode,

    -- ** ProcessingStoppingCondition
    processingStoppingCondition_maxRuntimeInSeconds,

    -- ** ProductionVariant
    productionVariant_initialVariantWeight,
    productionVariant_acceleratorType,
    productionVariant_coreDumpConfig,
    productionVariant_variantName,
    productionVariant_modelName,
    productionVariant_initialInstanceCount,
    productionVariant_instanceType,

    -- ** ProductionVariantCoreDumpConfig
    productionVariantCoreDumpConfig_kmsKeyId,
    productionVariantCoreDumpConfig_destinationS3Uri,

    -- ** ProductionVariantSummary
    productionVariantSummary_deployedImages,
    productionVariantSummary_desiredInstanceCount,
    productionVariantSummary_currentWeight,
    productionVariantSummary_currentInstanceCount,
    productionVariantSummary_desiredWeight,
    productionVariantSummary_variantName,

    -- ** ProfilerConfig
    profilerConfig_profilingParameters,
    profilerConfig_profilingIntervalInMilliseconds,
    profilerConfig_s3OutputPath,

    -- ** ProfilerConfigForUpdate
    profilerConfigForUpdate_s3OutputPath,
    profilerConfigForUpdate_profilingParameters,
    profilerConfigForUpdate_profilingIntervalInMilliseconds,
    profilerConfigForUpdate_disableProfiler,

    -- ** ProfilerRuleConfiguration
    profilerRuleConfiguration_ruleParameters,
    profilerRuleConfiguration_instanceType,
    profilerRuleConfiguration_s3OutputPath,
    profilerRuleConfiguration_volumeSizeInGB,
    profilerRuleConfiguration_localPath,
    profilerRuleConfiguration_ruleConfigurationName,
    profilerRuleConfiguration_ruleEvaluatorImage,

    -- ** ProfilerRuleEvaluationStatus
    profilerRuleEvaluationStatus_ruleConfigurationName,
    profilerRuleEvaluationStatus_statusDetails,
    profilerRuleEvaluationStatus_ruleEvaluationStatus,
    profilerRuleEvaluationStatus_lastModifiedTime,
    profilerRuleEvaluationStatus_ruleEvaluationJobArn,

    -- ** ProjectSummary
    projectSummary_projectDescription,
    projectSummary_projectName,
    projectSummary_projectArn,
    projectSummary_projectId,
    projectSummary_creationTime,
    projectSummary_projectStatus,

    -- ** PropertyNameQuery
    propertyNameQuery_propertyNameHint,

    -- ** PropertyNameSuggestion
    propertyNameSuggestion_propertyName,

    -- ** ProvisioningParameter
    provisioningParameter_key,
    provisioningParameter_value,

    -- ** PublicWorkforceTaskPrice
    publicWorkforceTaskPrice_amountInUsd,

    -- ** RedshiftDatasetDefinition
    redshiftDatasetDefinition_outputCompression,
    redshiftDatasetDefinition_kmsKeyId,
    redshiftDatasetDefinition_clusterId,
    redshiftDatasetDefinition_database,
    redshiftDatasetDefinition_dbUser,
    redshiftDatasetDefinition_queryString,
    redshiftDatasetDefinition_clusterRoleArn,
    redshiftDatasetDefinition_outputS3Uri,
    redshiftDatasetDefinition_outputFormat,

    -- ** RegisterModelStepMetadata
    registerModelStepMetadata_arn,

    -- ** RenderableTask
    renderableTask_input,

    -- ** RenderingError
    renderingError_code,
    renderingError_message,

    -- ** ResolvedAttributes
    resolvedAttributes_completionCriteria,
    resolvedAttributes_autoMLJobObjective,
    resolvedAttributes_problemType,

    -- ** ResourceConfig
    resourceConfig_volumeKmsKeyId,
    resourceConfig_instanceType,
    resourceConfig_instanceCount,
    resourceConfig_volumeSizeInGB,

    -- ** ResourceLimits
    resourceLimits_maxNumberOfTrainingJobs,
    resourceLimits_maxParallelTrainingJobs,

    -- ** ResourceSpec
    resourceSpec_instanceType,
    resourceSpec_sageMakerImageArn,
    resourceSpec_sageMakerImageVersionArn,

    -- ** RetentionPolicy
    retentionPolicy_homeEfsFileSystem,

    -- ** S3DataSource
    s3DataSource_s3DataDistributionType,
    s3DataSource_attributeNames,
    s3DataSource_s3DataType,
    s3DataSource_s3Uri,

    -- ** S3StorageConfig
    s3StorageConfig_kmsKeyId,
    s3StorageConfig_resolvedOutputS3Uri,
    s3StorageConfig_s3Uri,

    -- ** ScheduleConfig
    scheduleConfig_scheduleExpression,

    -- ** SearchExpression
    searchExpression_nestedFilters,
    searchExpression_operator,
    searchExpression_filters,
    searchExpression_subExpressions,

    -- ** SearchRecord
    searchRecord_experiment,
    searchRecord_featureGroup,
    searchRecord_modelPackage,
    searchRecord_trainingJob,
    searchRecord_endpoint,
    searchRecord_pipelineExecution,
    searchRecord_trialComponent,
    searchRecord_modelPackageGroup,
    searchRecord_pipeline,
    searchRecord_trial,

    -- ** SecondaryStatusTransition
    secondaryStatusTransition_statusMessage,
    secondaryStatusTransition_endTime,
    secondaryStatusTransition_status,
    secondaryStatusTransition_startTime,

    -- ** ServiceCatalogProvisionedProductDetails
    serviceCatalogProvisionedProductDetails_provisionedProductStatusMessage,
    serviceCatalogProvisionedProductDetails_provisionedProductId,

    -- ** ServiceCatalogProvisioningDetails
    serviceCatalogProvisioningDetails_provisioningParameters,
    serviceCatalogProvisioningDetails_pathId,
    serviceCatalogProvisioningDetails_productId,
    serviceCatalogProvisioningDetails_provisioningArtifactId,

    -- ** SharingSettings
    sharingSettings_s3KmsKeyId,
    sharingSettings_s3OutputPath,
    sharingSettings_notebookOutputOption,

    -- ** ShuffleConfig
    shuffleConfig_seed,

    -- ** SourceAlgorithm
    sourceAlgorithm_modelDataUrl,
    sourceAlgorithm_algorithmName,

    -- ** SourceAlgorithmSpecification
    sourceAlgorithmSpecification_sourceAlgorithms,

    -- ** SourceIpConfig
    sourceIpConfig_cidrs,

    -- ** StoppingCondition
    stoppingCondition_maxRuntimeInSeconds,
    stoppingCondition_maxWaitTimeInSeconds,

    -- ** SubscribedWorkteam
    subscribedWorkteam_marketplaceTitle,
    subscribedWorkteam_listingId,
    subscribedWorkteam_marketplaceDescription,
    subscribedWorkteam_sellerName,
    subscribedWorkteam_workteamArn,

    -- ** SuggestionQuery
    suggestionQuery_propertyNameQuery,

    -- ** Tag
    tag_key,
    tag_value,

    -- ** TargetPlatform
    targetPlatform_accelerator,
    targetPlatform_os,
    targetPlatform_arch,

    -- ** TensorBoardAppSettings
    tensorBoardAppSettings_defaultResourceSpec,

    -- ** TensorBoardOutputConfig
    tensorBoardOutputConfig_localPath,
    tensorBoardOutputConfig_s3OutputPath,

    -- ** TrafficRoutingConfig
    trafficRoutingConfig_canarySize,
    trafficRoutingConfig_type,
    trafficRoutingConfig_waitIntervalInSeconds,

    -- ** TrainingJob
    trainingJob_vpcConfig,
    trainingJob_debugRuleConfigurations,
    trainingJob_inputDataConfig,
    trainingJob_hyperParameters,
    trainingJob_enableManagedSpotTraining,
    trainingJob_creationTime,
    trainingJob_labelingJobArn,
    trainingJob_roleArn,
    trainingJob_trainingTimeInSeconds,
    trainingJob_experimentConfig,
    trainingJob_enableNetworkIsolation,
    trainingJob_enableInterContainerTrafficEncryption,
    trainingJob_trainingJobName,
    trainingJob_checkpointConfig,
    trainingJob_outputDataConfig,
    trainingJob_tuningJobArn,
    trainingJob_modelArtifacts,
    trainingJob_secondaryStatusTransitions,
    trainingJob_finalMetricDataList,
    trainingJob_autoMLJobArn,
    trainingJob_failureReason,
    trainingJob_tags,
    trainingJob_secondaryStatus,
    trainingJob_lastModifiedTime,
    trainingJob_tensorBoardOutputConfig,
    trainingJob_stoppingCondition,
    trainingJob_debugRuleEvaluationStatuses,
    trainingJob_trainingJobStatus,
    trainingJob_debugHookConfig,
    trainingJob_billableTimeInSeconds,
    trainingJob_resourceConfig,
    trainingJob_trainingStartTime,
    trainingJob_trainingEndTime,
    trainingJob_algorithmSpecification,
    trainingJob_trainingJobArn,

    -- ** TrainingJobDefinition
    trainingJobDefinition_hyperParameters,
    trainingJobDefinition_trainingInputMode,
    trainingJobDefinition_inputDataConfig,
    trainingJobDefinition_outputDataConfig,
    trainingJobDefinition_resourceConfig,
    trainingJobDefinition_stoppingCondition,

    -- ** TrainingJobStatusCounters
    trainingJobStatusCounters_stopped,
    trainingJobStatusCounters_completed,
    trainingJobStatusCounters_nonRetryableError,
    trainingJobStatusCounters_inProgress,
    trainingJobStatusCounters_retryableError,

    -- ** TrainingJobStepMetadata
    trainingJobStepMetadata_arn,

    -- ** TrainingJobSummary
    trainingJobSummary_lastModifiedTime,
    trainingJobSummary_trainingEndTime,
    trainingJobSummary_trainingJobName,
    trainingJobSummary_trainingJobArn,
    trainingJobSummary_creationTime,
    trainingJobSummary_trainingJobStatus,

    -- ** TrainingSpecification
    trainingSpecification_supportedHyperParameters,
    trainingSpecification_metricDefinitions,
    trainingSpecification_trainingImageDigest,
    trainingSpecification_supportsDistributedTraining,
    trainingSpecification_supportedTuningJobObjectiveMetrics,
    trainingSpecification_trainingImage,
    trainingSpecification_supportedTrainingInstanceTypes,
    trainingSpecification_trainingChannels,

    -- ** TransformDataSource
    transformDataSource_s3DataSource,

    -- ** TransformInput
    transformInput_contentType,
    transformInput_splitType,
    transformInput_compressionType,
    transformInput_dataSource,

    -- ** TransformJob
    transformJob_creationTime,
    transformJob_labelingJobArn,
    transformJob_transformJobName,
    transformJob_transformStartTime,
    transformJob_transformOutput,
    transformJob_experimentConfig,
    transformJob_maxConcurrentTransforms,
    transformJob_environment,
    transformJob_maxPayloadInMB,
    transformJob_batchStrategy,
    transformJob_transformJobStatus,
    transformJob_autoMLJobArn,
    transformJob_failureReason,
    transformJob_modelClientConfig,
    transformJob_tags,
    transformJob_transformEndTime,
    transformJob_transformJobArn,
    transformJob_dataProcessing,
    transformJob_modelName,
    transformJob_transformResources,
    transformJob_transformInput,

    -- ** TransformJobDefinition
    transformJobDefinition_maxConcurrentTransforms,
    transformJobDefinition_environment,
    transformJobDefinition_maxPayloadInMB,
    transformJobDefinition_batchStrategy,
    transformJobDefinition_transformInput,
    transformJobDefinition_transformOutput,
    transformJobDefinition_transformResources,

    -- ** TransformJobStepMetadata
    transformJobStepMetadata_arn,

    -- ** TransformJobSummary
    transformJobSummary_failureReason,
    transformJobSummary_lastModifiedTime,
    transformJobSummary_transformEndTime,
    transformJobSummary_transformJobName,
    transformJobSummary_transformJobArn,
    transformJobSummary_creationTime,
    transformJobSummary_transformJobStatus,

    -- ** TransformOutput
    transformOutput_accept,
    transformOutput_assembleWith,
    transformOutput_kmsKeyId,
    transformOutput_s3OutputPath,

    -- ** TransformResources
    transformResources_volumeKmsKeyId,
    transformResources_instanceType,
    transformResources_instanceCount,

    -- ** TransformS3DataSource
    transformS3DataSource_s3DataType,
    transformS3DataSource_s3Uri,

    -- ** Trial
    trial_trialArn,
    trial_metadataProperties,
    trial_creationTime,
    trial_source,
    trial_trialComponentSummaries,
    trial_tags,
    trial_lastModifiedTime,
    trial_experimentName,
    trial_createdBy,
    trial_lastModifiedBy,
    trial_displayName,
    trial_trialName,

    -- ** TrialComponent
    trialComponent_parents,
    trialComponent_status,
    trialComponent_metadataProperties,
    trialComponent_creationTime,
    trialComponent_sourceDetail,
    trialComponent_trialComponentArn,
    trialComponent_startTime,
    trialComponent_source,
    trialComponent_endTime,
    trialComponent_metrics,
    trialComponent_tags,
    trialComponent_lastModifiedTime,
    trialComponent_inputArtifacts,
    trialComponent_createdBy,
    trialComponent_lastModifiedBy,
    trialComponent_displayName,
    trialComponent_parameters,
    trialComponent_outputArtifacts,
    trialComponent_trialComponentName,

    -- ** TrialComponentArtifact
    trialComponentArtifact_mediaType,
    trialComponentArtifact_value,

    -- ** TrialComponentMetricSummary
    trialComponentMetricSummary_metricName,
    trialComponentMetricSummary_min,
    trialComponentMetricSummary_stdDev,
    trialComponentMetricSummary_max,
    trialComponentMetricSummary_timeStamp,
    trialComponentMetricSummary_count,
    trialComponentMetricSummary_sourceArn,
    trialComponentMetricSummary_avg,
    trialComponentMetricSummary_last,

    -- ** TrialComponentParameterValue
    trialComponentParameterValue_stringValue,
    trialComponentParameterValue_numberValue,

    -- ** TrialComponentSimpleSummary
    trialComponentSimpleSummary_creationTime,
    trialComponentSimpleSummary_trialComponentArn,
    trialComponentSimpleSummary_createdBy,
    trialComponentSimpleSummary_trialComponentSource,
    trialComponentSimpleSummary_trialComponentName,

    -- ** TrialComponentSource
    trialComponentSource_sourceType,
    trialComponentSource_sourceArn,

    -- ** TrialComponentSourceDetail
    trialComponentSourceDetail_processingJob,
    trialComponentSourceDetail_trainingJob,
    trialComponentSourceDetail_transformJob,
    trialComponentSourceDetail_sourceArn,

    -- ** TrialComponentStatus
    trialComponentStatus_message,
    trialComponentStatus_primaryStatus,

    -- ** TrialComponentSummary
    trialComponentSummary_status,
    trialComponentSummary_creationTime,
    trialComponentSummary_trialComponentArn,
    trialComponentSummary_startTime,
    trialComponentSummary_endTime,
    trialComponentSummary_lastModifiedTime,
    trialComponentSummary_createdBy,
    trialComponentSummary_lastModifiedBy,
    trialComponentSummary_trialComponentSource,
    trialComponentSummary_displayName,
    trialComponentSummary_trialComponentName,

    -- ** TrialSource
    trialSource_sourceType,
    trialSource_sourceArn,

    -- ** TrialSummary
    trialSummary_trialSource,
    trialSummary_trialArn,
    trialSummary_creationTime,
    trialSummary_lastModifiedTime,
    trialSummary_displayName,
    trialSummary_trialName,

    -- ** TuningJobCompletionCriteria
    tuningJobCompletionCriteria_targetObjectiveMetricValue,

    -- ** USD
    usd_dollars,
    usd_cents,
    usd_tenthFractionsOfACent,

    -- ** UiConfig
    uiConfig_humanTaskUiArn,
    uiConfig_uiTemplateS3Uri,

    -- ** UiTemplate
    uiTemplate_content,

    -- ** UiTemplateInfo
    uiTemplateInfo_contentSha256,
    uiTemplateInfo_url,

    -- ** UserContext
    userContext_userProfileName,
    userContext_domainId,
    userContext_userProfileArn,

    -- ** UserProfileDetails
    userProfileDetails_status,
    userProfileDetails_creationTime,
    userProfileDetails_userProfileName,
    userProfileDetails_domainId,
    userProfileDetails_lastModifiedTime,

    -- ** UserSettings
    userSettings_kernelGatewayAppSettings,
    userSettings_tensorBoardAppSettings,
    userSettings_securityGroups,
    userSettings_jupyterServerAppSettings,
    userSettings_executionRole,
    userSettings_sharingSettings,

    -- ** VariantProperty
    variantProperty_variantPropertyType,

    -- ** VpcConfig
    vpcConfig_securityGroupIds,
    vpcConfig_subnets,

    -- ** Workforce
    workforce_lastUpdatedDate,
    workforce_createDate,
    workforce_subDomain,
    workforce_sourceIpConfig,
    workforce_oidcConfig,
    workforce_cognitoConfig,
    workforce_workforceName,
    workforce_workforceArn,

    -- ** Workteam
    workteam_workforceArn,
    workteam_lastUpdatedDate,
    workteam_createDate,
    workteam_notificationConfiguration,
    workteam_productListingIds,
    workteam_subDomain,
    workteam_workteamName,
    workteam_memberDefinitions,
    workteam_workteamArn,
    workteam_description,
  )
where

import Network.AWS.SageMaker.AddAssociation
import Network.AWS.SageMaker.AddTags
import Network.AWS.SageMaker.AssociateTrialComponent
import Network.AWS.SageMaker.CreateAction
import Network.AWS.SageMaker.CreateAlgorithm
import Network.AWS.SageMaker.CreateApp
import Network.AWS.SageMaker.CreateAppImageConfig
import Network.AWS.SageMaker.CreateArtifact
import Network.AWS.SageMaker.CreateAutoMLJob
import Network.AWS.SageMaker.CreateCodeRepository
import Network.AWS.SageMaker.CreateCompilationJob
import Network.AWS.SageMaker.CreateContext
import Network.AWS.SageMaker.CreateDataQualityJobDefinition
import Network.AWS.SageMaker.CreateDeviceFleet
import Network.AWS.SageMaker.CreateDomain
import Network.AWS.SageMaker.CreateEdgePackagingJob
import Network.AWS.SageMaker.CreateEndpoint
import Network.AWS.SageMaker.CreateEndpointConfig
import Network.AWS.SageMaker.CreateExperiment
import Network.AWS.SageMaker.CreateFeatureGroup
import Network.AWS.SageMaker.CreateFlowDefinition
import Network.AWS.SageMaker.CreateHumanTaskUi
import Network.AWS.SageMaker.CreateHyperParameterTuningJob
import Network.AWS.SageMaker.CreateImage
import Network.AWS.SageMaker.CreateImageVersion
import Network.AWS.SageMaker.CreateLabelingJob
import Network.AWS.SageMaker.CreateModel
import Network.AWS.SageMaker.CreateModelBiasJobDefinition
import Network.AWS.SageMaker.CreateModelExplainabilityJobDefinition
import Network.AWS.SageMaker.CreateModelPackage
import Network.AWS.SageMaker.CreateModelPackageGroup
import Network.AWS.SageMaker.CreateModelQualityJobDefinition
import Network.AWS.SageMaker.CreateMonitoringSchedule
import Network.AWS.SageMaker.CreateNotebookInstance
import Network.AWS.SageMaker.CreateNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.CreatePipeline
import Network.AWS.SageMaker.CreatePresignedDomainUrl
import Network.AWS.SageMaker.CreatePresignedNotebookInstanceUrl
import Network.AWS.SageMaker.CreateProcessingJob
import Network.AWS.SageMaker.CreateProject
import Network.AWS.SageMaker.CreateTrainingJob
import Network.AWS.SageMaker.CreateTransformJob
import Network.AWS.SageMaker.CreateTrial
import Network.AWS.SageMaker.CreateTrialComponent
import Network.AWS.SageMaker.CreateUserProfile
import Network.AWS.SageMaker.CreateWorkforce
import Network.AWS.SageMaker.CreateWorkteam
import Network.AWS.SageMaker.DeleteAction
import Network.AWS.SageMaker.DeleteAlgorithm
import Network.AWS.SageMaker.DeleteApp
import Network.AWS.SageMaker.DeleteAppImageConfig
import Network.AWS.SageMaker.DeleteArtifact
import Network.AWS.SageMaker.DeleteAssociation
import Network.AWS.SageMaker.DeleteCodeRepository
import Network.AWS.SageMaker.DeleteContext
import Network.AWS.SageMaker.DeleteDataQualityJobDefinition
import Network.AWS.SageMaker.DeleteDeviceFleet
import Network.AWS.SageMaker.DeleteDomain
import Network.AWS.SageMaker.DeleteEndpoint
import Network.AWS.SageMaker.DeleteEndpointConfig
import Network.AWS.SageMaker.DeleteExperiment
import Network.AWS.SageMaker.DeleteFeatureGroup
import Network.AWS.SageMaker.DeleteFlowDefinition
import Network.AWS.SageMaker.DeleteHumanTaskUi
import Network.AWS.SageMaker.DeleteImage
import Network.AWS.SageMaker.DeleteImageVersion
import Network.AWS.SageMaker.DeleteModel
import Network.AWS.SageMaker.DeleteModelBiasJobDefinition
import Network.AWS.SageMaker.DeleteModelExplainabilityJobDefinition
import Network.AWS.SageMaker.DeleteModelPackage
import Network.AWS.SageMaker.DeleteModelPackageGroup
import Network.AWS.SageMaker.DeleteModelPackageGroupPolicy
import Network.AWS.SageMaker.DeleteModelQualityJobDefinition
import Network.AWS.SageMaker.DeleteMonitoringSchedule
import Network.AWS.SageMaker.DeleteNotebookInstance
import Network.AWS.SageMaker.DeleteNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.DeletePipeline
import Network.AWS.SageMaker.DeleteProject
import Network.AWS.SageMaker.DeleteTags
import Network.AWS.SageMaker.DeleteTrial
import Network.AWS.SageMaker.DeleteTrialComponent
import Network.AWS.SageMaker.DeleteUserProfile
import Network.AWS.SageMaker.DeleteWorkforce
import Network.AWS.SageMaker.DeleteWorkteam
import Network.AWS.SageMaker.DeregisterDevices
import Network.AWS.SageMaker.DescribeAction
import Network.AWS.SageMaker.DescribeAlgorithm
import Network.AWS.SageMaker.DescribeApp
import Network.AWS.SageMaker.DescribeAppImageConfig
import Network.AWS.SageMaker.DescribeArtifact
import Network.AWS.SageMaker.DescribeAutoMLJob
import Network.AWS.SageMaker.DescribeCodeRepository
import Network.AWS.SageMaker.DescribeCompilationJob
import Network.AWS.SageMaker.DescribeContext
import Network.AWS.SageMaker.DescribeDataQualityJobDefinition
import Network.AWS.SageMaker.DescribeDevice
import Network.AWS.SageMaker.DescribeDeviceFleet
import Network.AWS.SageMaker.DescribeDomain
import Network.AWS.SageMaker.DescribeEdgePackagingJob
import Network.AWS.SageMaker.DescribeEndpoint
import Network.AWS.SageMaker.DescribeEndpointConfig
import Network.AWS.SageMaker.DescribeExperiment
import Network.AWS.SageMaker.DescribeFeatureGroup
import Network.AWS.SageMaker.DescribeFlowDefinition
import Network.AWS.SageMaker.DescribeHumanTaskUi
import Network.AWS.SageMaker.DescribeHyperParameterTuningJob
import Network.AWS.SageMaker.DescribeImage
import Network.AWS.SageMaker.DescribeImageVersion
import Network.AWS.SageMaker.DescribeLabelingJob
import Network.AWS.SageMaker.DescribeModel
import Network.AWS.SageMaker.DescribeModelBiasJobDefinition
import Network.AWS.SageMaker.DescribeModelExplainabilityJobDefinition
import Network.AWS.SageMaker.DescribeModelPackage
import Network.AWS.SageMaker.DescribeModelPackageGroup
import Network.AWS.SageMaker.DescribeModelQualityJobDefinition
import Network.AWS.SageMaker.DescribeMonitoringSchedule
import Network.AWS.SageMaker.DescribeNotebookInstance
import Network.AWS.SageMaker.DescribeNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.DescribePipeline
import Network.AWS.SageMaker.DescribePipelineDefinitionForExecution
import Network.AWS.SageMaker.DescribePipelineExecution
import Network.AWS.SageMaker.DescribeProcessingJob
import Network.AWS.SageMaker.DescribeProject
import Network.AWS.SageMaker.DescribeSubscribedWorkteam
import Network.AWS.SageMaker.DescribeTrainingJob
import Network.AWS.SageMaker.DescribeTransformJob
import Network.AWS.SageMaker.DescribeTrial
import Network.AWS.SageMaker.DescribeTrialComponent
import Network.AWS.SageMaker.DescribeUserProfile
import Network.AWS.SageMaker.DescribeWorkforce
import Network.AWS.SageMaker.DescribeWorkteam
import Network.AWS.SageMaker.DisableSagemakerServicecatalogPortfolio
import Network.AWS.SageMaker.DisassociateTrialComponent
import Network.AWS.SageMaker.EnableSagemakerServicecatalogPortfolio
import Network.AWS.SageMaker.GetDeviceFleetReport
import Network.AWS.SageMaker.GetModelPackageGroupPolicy
import Network.AWS.SageMaker.GetSagemakerServicecatalogPortfolioStatus
import Network.AWS.SageMaker.GetSearchSuggestions
import Network.AWS.SageMaker.ListActions
import Network.AWS.SageMaker.ListAlgorithms
import Network.AWS.SageMaker.ListAppImageConfigs
import Network.AWS.SageMaker.ListApps
import Network.AWS.SageMaker.ListArtifacts
import Network.AWS.SageMaker.ListAssociations
import Network.AWS.SageMaker.ListAutoMLJobs
import Network.AWS.SageMaker.ListCandidatesForAutoMLJob
import Network.AWS.SageMaker.ListCodeRepositories
import Network.AWS.SageMaker.ListCompilationJobs
import Network.AWS.SageMaker.ListContexts
import Network.AWS.SageMaker.ListDataQualityJobDefinitions
import Network.AWS.SageMaker.ListDeviceFleets
import Network.AWS.SageMaker.ListDevices
import Network.AWS.SageMaker.ListDomains
import Network.AWS.SageMaker.ListEdgePackagingJobs
import Network.AWS.SageMaker.ListEndpointConfigs
import Network.AWS.SageMaker.ListEndpoints
import Network.AWS.SageMaker.ListExperiments
import Network.AWS.SageMaker.ListFeatureGroups
import Network.AWS.SageMaker.ListFlowDefinitions
import Network.AWS.SageMaker.ListHumanTaskUis
import Network.AWS.SageMaker.ListHyperParameterTuningJobs
import Network.AWS.SageMaker.ListImageVersions
import Network.AWS.SageMaker.ListImages
import Network.AWS.SageMaker.ListLabelingJobs
import Network.AWS.SageMaker.ListLabelingJobsForWorkteam
import Network.AWS.SageMaker.ListModelBiasJobDefinitions
import Network.AWS.SageMaker.ListModelExplainabilityJobDefinitions
import Network.AWS.SageMaker.ListModelPackageGroups
import Network.AWS.SageMaker.ListModelPackages
import Network.AWS.SageMaker.ListModelQualityJobDefinitions
import Network.AWS.SageMaker.ListModels
import Network.AWS.SageMaker.ListMonitoringExecutions
import Network.AWS.SageMaker.ListMonitoringSchedules
import Network.AWS.SageMaker.ListNotebookInstanceLifecycleConfigs
import Network.AWS.SageMaker.ListNotebookInstances
import Network.AWS.SageMaker.ListPipelineExecutionSteps
import Network.AWS.SageMaker.ListPipelineExecutions
import Network.AWS.SageMaker.ListPipelineParametersForExecution
import Network.AWS.SageMaker.ListPipelines
import Network.AWS.SageMaker.ListProcessingJobs
import Network.AWS.SageMaker.ListProjects
import Network.AWS.SageMaker.ListSubscribedWorkteams
import Network.AWS.SageMaker.ListTags
import Network.AWS.SageMaker.ListTrainingJobs
import Network.AWS.SageMaker.ListTrainingJobsForHyperParameterTuningJob
import Network.AWS.SageMaker.ListTransformJobs
import Network.AWS.SageMaker.ListTrialComponents
import Network.AWS.SageMaker.ListTrials
import Network.AWS.SageMaker.ListUserProfiles
import Network.AWS.SageMaker.ListWorkforces
import Network.AWS.SageMaker.ListWorkteams
import Network.AWS.SageMaker.PutModelPackageGroupPolicy
import Network.AWS.SageMaker.RegisterDevices
import Network.AWS.SageMaker.RenderUiTemplate
import Network.AWS.SageMaker.Search
import Network.AWS.SageMaker.StartMonitoringSchedule
import Network.AWS.SageMaker.StartNotebookInstance
import Network.AWS.SageMaker.StartPipelineExecution
import Network.AWS.SageMaker.StopAutoMLJob
import Network.AWS.SageMaker.StopCompilationJob
import Network.AWS.SageMaker.StopEdgePackagingJob
import Network.AWS.SageMaker.StopHyperParameterTuningJob
import Network.AWS.SageMaker.StopLabelingJob
import Network.AWS.SageMaker.StopMonitoringSchedule
import Network.AWS.SageMaker.StopNotebookInstance
import Network.AWS.SageMaker.StopPipelineExecution
import Network.AWS.SageMaker.StopProcessingJob
import Network.AWS.SageMaker.StopTrainingJob
import Network.AWS.SageMaker.StopTransformJob
import Network.AWS.SageMaker.Types.ActionSource
import Network.AWS.SageMaker.Types.ActionSummary
import Network.AWS.SageMaker.Types.AgentVersion
import Network.AWS.SageMaker.Types.Alarm
import Network.AWS.SageMaker.Types.AlgorithmSpecification
import Network.AWS.SageMaker.Types.AlgorithmStatusDetails
import Network.AWS.SageMaker.Types.AlgorithmStatusItem
import Network.AWS.SageMaker.Types.AlgorithmSummary
import Network.AWS.SageMaker.Types.AlgorithmValidationProfile
import Network.AWS.SageMaker.Types.AlgorithmValidationSpecification
import Network.AWS.SageMaker.Types.AnnotationConsolidationConfig
import Network.AWS.SageMaker.Types.AppDetails
import Network.AWS.SageMaker.Types.AppImageConfigDetails
import Network.AWS.SageMaker.Types.AppSpecification
import Network.AWS.SageMaker.Types.ArtifactSource
import Network.AWS.SageMaker.Types.ArtifactSourceType
import Network.AWS.SageMaker.Types.ArtifactSummary
import Network.AWS.SageMaker.Types.AssociationSummary
import Network.AWS.SageMaker.Types.AthenaDatasetDefinition
import Network.AWS.SageMaker.Types.AutoMLCandidate
import Network.AWS.SageMaker.Types.AutoMLCandidateStep
import Network.AWS.SageMaker.Types.AutoMLChannel
import Network.AWS.SageMaker.Types.AutoMLContainerDefinition
import Network.AWS.SageMaker.Types.AutoMLDataSource
import Network.AWS.SageMaker.Types.AutoMLJobArtifacts
import Network.AWS.SageMaker.Types.AutoMLJobCompletionCriteria
import Network.AWS.SageMaker.Types.AutoMLJobConfig
import Network.AWS.SageMaker.Types.AutoMLJobObjective
import Network.AWS.SageMaker.Types.AutoMLJobSummary
import Network.AWS.SageMaker.Types.AutoMLOutputDataConfig
import Network.AWS.SageMaker.Types.AutoMLS3DataSource
import Network.AWS.SageMaker.Types.AutoMLSecurityConfig
import Network.AWS.SageMaker.Types.AutoRollbackConfig
import Network.AWS.SageMaker.Types.Bias
import Network.AWS.SageMaker.Types.BlueGreenUpdatePolicy
import Network.AWS.SageMaker.Types.CacheHitResult
import Network.AWS.SageMaker.Types.CapacitySize
import Network.AWS.SageMaker.Types.CaptureContentTypeHeader
import Network.AWS.SageMaker.Types.CaptureOption
import Network.AWS.SageMaker.Types.CategoricalParameterRange
import Network.AWS.SageMaker.Types.CategoricalParameterRangeSpecification
import Network.AWS.SageMaker.Types.Channel
import Network.AWS.SageMaker.Types.ChannelSpecification
import Network.AWS.SageMaker.Types.CheckpointConfig
import Network.AWS.SageMaker.Types.CodeRepositorySummary
import Network.AWS.SageMaker.Types.CognitoConfig
import Network.AWS.SageMaker.Types.CognitoMemberDefinition
import Network.AWS.SageMaker.Types.CollectionConfiguration
import Network.AWS.SageMaker.Types.CompilationJobSummary
import Network.AWS.SageMaker.Types.ConditionStepMetadata
import Network.AWS.SageMaker.Types.ContainerDefinition
import Network.AWS.SageMaker.Types.ContextSource
import Network.AWS.SageMaker.Types.ContextSummary
import Network.AWS.SageMaker.Types.ContinuousParameterRange
import Network.AWS.SageMaker.Types.ContinuousParameterRangeSpecification
import Network.AWS.SageMaker.Types.CustomImage
import Network.AWS.SageMaker.Types.DataCaptureConfig
import Network.AWS.SageMaker.Types.DataCaptureConfigSummary
import Network.AWS.SageMaker.Types.DataCatalogConfig
import Network.AWS.SageMaker.Types.DataProcessing
import Network.AWS.SageMaker.Types.DataQualityAppSpecification
import Network.AWS.SageMaker.Types.DataQualityBaselineConfig
import Network.AWS.SageMaker.Types.DataQualityJobInput
import Network.AWS.SageMaker.Types.DataSource
import Network.AWS.SageMaker.Types.DatasetDefinition
import Network.AWS.SageMaker.Types.DebugHookConfig
import Network.AWS.SageMaker.Types.DebugRuleConfiguration
import Network.AWS.SageMaker.Types.DebugRuleEvaluationStatus
import Network.AWS.SageMaker.Types.DeployedImage
import Network.AWS.SageMaker.Types.DeploymentConfig
import Network.AWS.SageMaker.Types.DesiredWeightAndCapacity
import Network.AWS.SageMaker.Types.Device
import Network.AWS.SageMaker.Types.DeviceFleetSummary
import Network.AWS.SageMaker.Types.DeviceStats
import Network.AWS.SageMaker.Types.DeviceSummary
import Network.AWS.SageMaker.Types.DomainDetails
import Network.AWS.SageMaker.Types.EdgeModel
import Network.AWS.SageMaker.Types.EdgeModelStat
import Network.AWS.SageMaker.Types.EdgeModelSummary
import Network.AWS.SageMaker.Types.EdgeOutputConfig
import Network.AWS.SageMaker.Types.EdgePackagingJobSummary
import Network.AWS.SageMaker.Types.Endpoint
import Network.AWS.SageMaker.Types.EndpointConfigSummary
import Network.AWS.SageMaker.Types.EndpointInput
import Network.AWS.SageMaker.Types.EndpointSummary
import Network.AWS.SageMaker.Types.Experiment
import Network.AWS.SageMaker.Types.ExperimentConfig
import Network.AWS.SageMaker.Types.ExperimentSource
import Network.AWS.SageMaker.Types.ExperimentSummary
import Network.AWS.SageMaker.Types.Explainability
import Network.AWS.SageMaker.Types.FeatureDefinition
import Network.AWS.SageMaker.Types.FeatureGroup
import Network.AWS.SageMaker.Types.FeatureGroupSummary
import Network.AWS.SageMaker.Types.FileSystemConfig
import Network.AWS.SageMaker.Types.FileSystemDataSource
import Network.AWS.SageMaker.Types.Filter
import Network.AWS.SageMaker.Types.FinalAutoMLJobObjectiveMetric
import Network.AWS.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric
import Network.AWS.SageMaker.Types.FlowDefinitionOutputConfig
import Network.AWS.SageMaker.Types.FlowDefinitionSummary
import Network.AWS.SageMaker.Types.GitConfig
import Network.AWS.SageMaker.Types.GitConfigForUpdate
import Network.AWS.SageMaker.Types.HumanLoopActivationConditionsConfig
import Network.AWS.SageMaker.Types.HumanLoopActivationConfig
import Network.AWS.SageMaker.Types.HumanLoopConfig
import Network.AWS.SageMaker.Types.HumanLoopRequestSource
import Network.AWS.SageMaker.Types.HumanTaskConfig
import Network.AWS.SageMaker.Types.HumanTaskUiSummary
import Network.AWS.SageMaker.Types.HyperParameterAlgorithmSpecification
import Network.AWS.SageMaker.Types.HyperParameterSpecification
import Network.AWS.SageMaker.Types.HyperParameterTrainingJobDefinition
import Network.AWS.SageMaker.Types.HyperParameterTrainingJobSummary
import Network.AWS.SageMaker.Types.HyperParameterTuningJobConfig
import Network.AWS.SageMaker.Types.HyperParameterTuningJobObjective
import Network.AWS.SageMaker.Types.HyperParameterTuningJobSummary
import Network.AWS.SageMaker.Types.HyperParameterTuningJobWarmStartConfig
import Network.AWS.SageMaker.Types.Image
import Network.AWS.SageMaker.Types.ImageConfig
import Network.AWS.SageMaker.Types.ImageVersion
import Network.AWS.SageMaker.Types.InferenceExecutionConfig
import Network.AWS.SageMaker.Types.InferenceSpecification
import Network.AWS.SageMaker.Types.InputConfig
import Network.AWS.SageMaker.Types.IntegerParameterRange
import Network.AWS.SageMaker.Types.IntegerParameterRangeSpecification
import Network.AWS.SageMaker.Types.JupyterServerAppSettings
import Network.AWS.SageMaker.Types.KernelGatewayAppSettings
import Network.AWS.SageMaker.Types.KernelGatewayImageConfig
import Network.AWS.SageMaker.Types.KernelSpec
import Network.AWS.SageMaker.Types.LabelCounters
import Network.AWS.SageMaker.Types.LabelCountersForWorkteam
import Network.AWS.SageMaker.Types.LabelingJobAlgorithmsConfig
import Network.AWS.SageMaker.Types.LabelingJobDataAttributes
import Network.AWS.SageMaker.Types.LabelingJobDataSource
import Network.AWS.SageMaker.Types.LabelingJobForWorkteamSummary
import Network.AWS.SageMaker.Types.LabelingJobInputConfig
import Network.AWS.SageMaker.Types.LabelingJobOutput
import Network.AWS.SageMaker.Types.LabelingJobOutputConfig
import Network.AWS.SageMaker.Types.LabelingJobResourceConfig
import Network.AWS.SageMaker.Types.LabelingJobS3DataSource
import Network.AWS.SageMaker.Types.LabelingJobSnsDataSource
import Network.AWS.SageMaker.Types.LabelingJobStoppingConditions
import Network.AWS.SageMaker.Types.LabelingJobSummary
import Network.AWS.SageMaker.Types.MemberDefinition
import Network.AWS.SageMaker.Types.MetadataProperties
import Network.AWS.SageMaker.Types.MetricData
import Network.AWS.SageMaker.Types.MetricDefinition
import Network.AWS.SageMaker.Types.MetricsSource
import Network.AWS.SageMaker.Types.ModelArtifacts
import Network.AWS.SageMaker.Types.ModelBiasAppSpecification
import Network.AWS.SageMaker.Types.ModelBiasBaselineConfig
import Network.AWS.SageMaker.Types.ModelBiasJobInput
import Network.AWS.SageMaker.Types.ModelClientConfig
import Network.AWS.SageMaker.Types.ModelDataQuality
import Network.AWS.SageMaker.Types.ModelDigests
import Network.AWS.SageMaker.Types.ModelExplainabilityAppSpecification
import Network.AWS.SageMaker.Types.ModelExplainabilityBaselineConfig
import Network.AWS.SageMaker.Types.ModelExplainabilityJobInput
import Network.AWS.SageMaker.Types.ModelMetrics
import Network.AWS.SageMaker.Types.ModelPackage
import Network.AWS.SageMaker.Types.ModelPackageContainerDefinition
import Network.AWS.SageMaker.Types.ModelPackageGroup
import Network.AWS.SageMaker.Types.ModelPackageGroupSummary
import Network.AWS.SageMaker.Types.ModelPackageStatusDetails
import Network.AWS.SageMaker.Types.ModelPackageStatusItem
import Network.AWS.SageMaker.Types.ModelPackageSummary
import Network.AWS.SageMaker.Types.ModelPackageValidationProfile
import Network.AWS.SageMaker.Types.ModelPackageValidationSpecification
import Network.AWS.SageMaker.Types.ModelQuality
import Network.AWS.SageMaker.Types.ModelQualityAppSpecification
import Network.AWS.SageMaker.Types.ModelQualityBaselineConfig
import Network.AWS.SageMaker.Types.ModelQualityJobInput
import Network.AWS.SageMaker.Types.ModelStepMetadata
import Network.AWS.SageMaker.Types.ModelSummary
import Network.AWS.SageMaker.Types.MonitoringAppSpecification
import Network.AWS.SageMaker.Types.MonitoringBaselineConfig
import Network.AWS.SageMaker.Types.MonitoringClusterConfig
import Network.AWS.SageMaker.Types.MonitoringConstraintsResource
import Network.AWS.SageMaker.Types.MonitoringExecutionSummary
import Network.AWS.SageMaker.Types.MonitoringGroundTruthS3Input
import Network.AWS.SageMaker.Types.MonitoringInput
import Network.AWS.SageMaker.Types.MonitoringJobDefinition
import Network.AWS.SageMaker.Types.MonitoringJobDefinitionSummary
import Network.AWS.SageMaker.Types.MonitoringNetworkConfig
import Network.AWS.SageMaker.Types.MonitoringOutput
import Network.AWS.SageMaker.Types.MonitoringOutputConfig
import Network.AWS.SageMaker.Types.MonitoringResources
import Network.AWS.SageMaker.Types.MonitoringS3Output
import Network.AWS.SageMaker.Types.MonitoringSchedule
import Network.AWS.SageMaker.Types.MonitoringScheduleConfig
import Network.AWS.SageMaker.Types.MonitoringScheduleSummary
import Network.AWS.SageMaker.Types.MonitoringStatisticsResource
import Network.AWS.SageMaker.Types.MonitoringStoppingCondition
import Network.AWS.SageMaker.Types.MultiModelConfig
import Network.AWS.SageMaker.Types.NestedFilters
import Network.AWS.SageMaker.Types.NetworkConfig
import Network.AWS.SageMaker.Types.NotebookInstanceLifecycleConfigSummary
import Network.AWS.SageMaker.Types.NotebookInstanceLifecycleHook
import Network.AWS.SageMaker.Types.NotebookInstanceSummary
import Network.AWS.SageMaker.Types.NotificationConfiguration
import Network.AWS.SageMaker.Types.ObjectiveStatusCounters
import Network.AWS.SageMaker.Types.OfflineStoreConfig
import Network.AWS.SageMaker.Types.OfflineStoreStatus
import Network.AWS.SageMaker.Types.OidcConfig
import Network.AWS.SageMaker.Types.OidcConfigForResponse
import Network.AWS.SageMaker.Types.OidcMemberDefinition
import Network.AWS.SageMaker.Types.OnlineStoreConfig
import Network.AWS.SageMaker.Types.OnlineStoreSecurityConfig
import Network.AWS.SageMaker.Types.OutputConfig
import Network.AWS.SageMaker.Types.OutputDataConfig
import Network.AWS.SageMaker.Types.Parameter
import Network.AWS.SageMaker.Types.ParameterRange
import Network.AWS.SageMaker.Types.ParameterRanges
import Network.AWS.SageMaker.Types.Parent
import Network.AWS.SageMaker.Types.ParentHyperParameterTuningJob
import Network.AWS.SageMaker.Types.Pipeline
import Network.AWS.SageMaker.Types.PipelineExecution
import Network.AWS.SageMaker.Types.PipelineExecutionStep
import Network.AWS.SageMaker.Types.PipelineExecutionStepMetadata
import Network.AWS.SageMaker.Types.PipelineExecutionSummary
import Network.AWS.SageMaker.Types.PipelineSummary
import Network.AWS.SageMaker.Types.ProcessingClusterConfig
import Network.AWS.SageMaker.Types.ProcessingFeatureStoreOutput
import Network.AWS.SageMaker.Types.ProcessingInput
import Network.AWS.SageMaker.Types.ProcessingJob
import Network.AWS.SageMaker.Types.ProcessingJobStepMetadata
import Network.AWS.SageMaker.Types.ProcessingJobSummary
import Network.AWS.SageMaker.Types.ProcessingOutput
import Network.AWS.SageMaker.Types.ProcessingOutputConfig
import Network.AWS.SageMaker.Types.ProcessingResources
import Network.AWS.SageMaker.Types.ProcessingS3Input
import Network.AWS.SageMaker.Types.ProcessingS3Output
import Network.AWS.SageMaker.Types.ProcessingStoppingCondition
import Network.AWS.SageMaker.Types.ProductionVariant
import Network.AWS.SageMaker.Types.ProductionVariantCoreDumpConfig
import Network.AWS.SageMaker.Types.ProductionVariantSummary
import Network.AWS.SageMaker.Types.ProfilerConfig
import Network.AWS.SageMaker.Types.ProfilerConfigForUpdate
import Network.AWS.SageMaker.Types.ProfilerRuleConfiguration
import Network.AWS.SageMaker.Types.ProfilerRuleEvaluationStatus
import Network.AWS.SageMaker.Types.ProjectSummary
import Network.AWS.SageMaker.Types.PropertyNameQuery
import Network.AWS.SageMaker.Types.PropertyNameSuggestion
import Network.AWS.SageMaker.Types.ProvisioningParameter
import Network.AWS.SageMaker.Types.PublicWorkforceTaskPrice
import Network.AWS.SageMaker.Types.RedshiftDatasetDefinition
import Network.AWS.SageMaker.Types.RegisterModelStepMetadata
import Network.AWS.SageMaker.Types.RenderableTask
import Network.AWS.SageMaker.Types.RenderingError
import Network.AWS.SageMaker.Types.ResolvedAttributes
import Network.AWS.SageMaker.Types.ResourceConfig
import Network.AWS.SageMaker.Types.ResourceLimits
import Network.AWS.SageMaker.Types.ResourceSpec
import Network.AWS.SageMaker.Types.RetentionPolicy
import Network.AWS.SageMaker.Types.S3DataSource
import Network.AWS.SageMaker.Types.S3StorageConfig
import Network.AWS.SageMaker.Types.ScheduleConfig
import Network.AWS.SageMaker.Types.SearchExpression
import Network.AWS.SageMaker.Types.SearchRecord
import Network.AWS.SageMaker.Types.SecondaryStatusTransition
import Network.AWS.SageMaker.Types.ServiceCatalogProvisionedProductDetails
import Network.AWS.SageMaker.Types.ServiceCatalogProvisioningDetails
import Network.AWS.SageMaker.Types.SharingSettings
import Network.AWS.SageMaker.Types.ShuffleConfig
import Network.AWS.SageMaker.Types.SourceAlgorithm
import Network.AWS.SageMaker.Types.SourceAlgorithmSpecification
import Network.AWS.SageMaker.Types.SourceIpConfig
import Network.AWS.SageMaker.Types.StoppingCondition
import Network.AWS.SageMaker.Types.SubscribedWorkteam
import Network.AWS.SageMaker.Types.SuggestionQuery
import Network.AWS.SageMaker.Types.Tag
import Network.AWS.SageMaker.Types.TargetPlatform
import Network.AWS.SageMaker.Types.TensorBoardAppSettings
import Network.AWS.SageMaker.Types.TensorBoardOutputConfig
import Network.AWS.SageMaker.Types.TrafficRoutingConfig
import Network.AWS.SageMaker.Types.TrainingJob
import Network.AWS.SageMaker.Types.TrainingJobDefinition
import Network.AWS.SageMaker.Types.TrainingJobStatusCounters
import Network.AWS.SageMaker.Types.TrainingJobStepMetadata
import Network.AWS.SageMaker.Types.TrainingJobSummary
import Network.AWS.SageMaker.Types.TrainingSpecification
import Network.AWS.SageMaker.Types.TransformDataSource
import Network.AWS.SageMaker.Types.TransformInput
import Network.AWS.SageMaker.Types.TransformJob
import Network.AWS.SageMaker.Types.TransformJobDefinition
import Network.AWS.SageMaker.Types.TransformJobStepMetadata
import Network.AWS.SageMaker.Types.TransformJobSummary
import Network.AWS.SageMaker.Types.TransformOutput
import Network.AWS.SageMaker.Types.TransformResources
import Network.AWS.SageMaker.Types.TransformS3DataSource
import Network.AWS.SageMaker.Types.Trial
import Network.AWS.SageMaker.Types.TrialComponent
import Network.AWS.SageMaker.Types.TrialComponentArtifact
import Network.AWS.SageMaker.Types.TrialComponentMetricSummary
import Network.AWS.SageMaker.Types.TrialComponentParameterValue
import Network.AWS.SageMaker.Types.TrialComponentSimpleSummary
import Network.AWS.SageMaker.Types.TrialComponentSource
import Network.AWS.SageMaker.Types.TrialComponentSourceDetail
import Network.AWS.SageMaker.Types.TrialComponentStatus
import Network.AWS.SageMaker.Types.TrialComponentSummary
import Network.AWS.SageMaker.Types.TrialSource
import Network.AWS.SageMaker.Types.TrialSummary
import Network.AWS.SageMaker.Types.TuningJobCompletionCriteria
import Network.AWS.SageMaker.Types.USD
import Network.AWS.SageMaker.Types.UiConfig
import Network.AWS.SageMaker.Types.UiTemplate
import Network.AWS.SageMaker.Types.UiTemplateInfo
import Network.AWS.SageMaker.Types.UserContext
import Network.AWS.SageMaker.Types.UserProfileDetails
import Network.AWS.SageMaker.Types.UserSettings
import Network.AWS.SageMaker.Types.VariantProperty
import Network.AWS.SageMaker.Types.VpcConfig
import Network.AWS.SageMaker.Types.Workforce
import Network.AWS.SageMaker.Types.Workteam
import Network.AWS.SageMaker.UpdateAction
import Network.AWS.SageMaker.UpdateAppImageConfig
import Network.AWS.SageMaker.UpdateArtifact
import Network.AWS.SageMaker.UpdateCodeRepository
import Network.AWS.SageMaker.UpdateContext
import Network.AWS.SageMaker.UpdateDeviceFleet
import Network.AWS.SageMaker.UpdateDevices
import Network.AWS.SageMaker.UpdateDomain
import Network.AWS.SageMaker.UpdateEndpoint
import Network.AWS.SageMaker.UpdateEndpointWeightsAndCapacities
import Network.AWS.SageMaker.UpdateExperiment
import Network.AWS.SageMaker.UpdateImage
import Network.AWS.SageMaker.UpdateModelPackage
import Network.AWS.SageMaker.UpdateMonitoringSchedule
import Network.AWS.SageMaker.UpdateNotebookInstance
import Network.AWS.SageMaker.UpdateNotebookInstanceLifecycleConfig
import Network.AWS.SageMaker.UpdatePipeline
import Network.AWS.SageMaker.UpdatePipelineExecution
import Network.AWS.SageMaker.UpdateTrainingJob
import Network.AWS.SageMaker.UpdateTrial
import Network.AWS.SageMaker.UpdateTrialComponent
import Network.AWS.SageMaker.UpdateUserProfile
import Network.AWS.SageMaker.UpdateWorkforce
import Network.AWS.SageMaker.UpdateWorkteam
