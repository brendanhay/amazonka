{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Lens
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Lens
  ( -- * Operations

    -- ** AddAssociation
    addAssociation_associationType,
    addAssociation_sourceArn,
    addAssociation_destinationArn,
    addAssociationResponse_sourceArn,
    addAssociationResponse_destinationArn,
    addAssociationResponse_httpStatus,

    -- ** AddTags
    addTags_resourceArn,
    addTags_tags,
    addTagsResponse_tags,
    addTagsResponse_httpStatus,

    -- ** AssociateTrialComponent
    associateTrialComponent_trialComponentName,
    associateTrialComponent_trialName,
    associateTrialComponentResponse_trialComponentArn,
    associateTrialComponentResponse_trialArn,
    associateTrialComponentResponse_httpStatus,

    -- ** BatchDescribeModelPackage
    batchDescribeModelPackage_modelPackageArnList,
    batchDescribeModelPackageResponse_modelPackageSummaries,
    batchDescribeModelPackageResponse_batchDescribeModelPackageErrorMap,
    batchDescribeModelPackageResponse_httpStatus,

    -- ** CreateAction
    createAction_tags,
    createAction_metadataProperties,
    createAction_properties,
    createAction_status,
    createAction_description,
    createAction_actionName,
    createAction_source,
    createAction_actionType,
    createActionResponse_actionArn,
    createActionResponse_httpStatus,

    -- ** CreateAlgorithm
    createAlgorithm_tags,
    createAlgorithm_validationSpecification,
    createAlgorithm_certifyForMarketplace,
    createAlgorithm_inferenceSpecification,
    createAlgorithm_algorithmDescription,
    createAlgorithm_algorithmName,
    createAlgorithm_trainingSpecification,
    createAlgorithmResponse_httpStatus,
    createAlgorithmResponse_algorithmArn,

    -- ** CreateApp
    createApp_tags,
    createApp_resourceSpec,
    createApp_domainId,
    createApp_userProfileName,
    createApp_appType,
    createApp_appName,
    createAppResponse_appArn,
    createAppResponse_httpStatus,

    -- ** CreateAppImageConfig
    createAppImageConfig_tags,
    createAppImageConfig_kernelGatewayImageConfig,
    createAppImageConfig_appImageConfigName,
    createAppImageConfigResponse_appImageConfigArn,
    createAppImageConfigResponse_httpStatus,

    -- ** CreateArtifact
    createArtifact_artifactName,
    createArtifact_tags,
    createArtifact_metadataProperties,
    createArtifact_properties,
    createArtifact_source,
    createArtifact_artifactType,
    createArtifactResponse_artifactArn,
    createArtifactResponse_httpStatus,

    -- ** CreateAutoMLJob
    createAutoMLJob_tags,
    createAutoMLJob_autoMLJobConfig,
    createAutoMLJob_autoMLJobObjective,
    createAutoMLJob_modelDeployConfig,
    createAutoMLJob_problemType,
    createAutoMLJob_generateCandidateDefinitionsOnly,
    createAutoMLJob_autoMLJobName,
    createAutoMLJob_inputDataConfig,
    createAutoMLJob_outputDataConfig,
    createAutoMLJob_roleArn,
    createAutoMLJobResponse_httpStatus,
    createAutoMLJobResponse_autoMLJobArn,

    -- ** CreateCodeRepository
    createCodeRepository_tags,
    createCodeRepository_codeRepositoryName,
    createCodeRepository_gitConfig,
    createCodeRepositoryResponse_httpStatus,
    createCodeRepositoryResponse_codeRepositoryArn,

    -- ** CreateCompilationJob
    createCompilationJob_tags,
    createCompilationJob_vpcConfig,
    createCompilationJob_inputConfig,
    createCompilationJob_modelPackageVersionArn,
    createCompilationJob_compilationJobName,
    createCompilationJob_roleArn,
    createCompilationJob_outputConfig,
    createCompilationJob_stoppingCondition,
    createCompilationJobResponse_httpStatus,
    createCompilationJobResponse_compilationJobArn,

    -- ** CreateContext
    createContext_tags,
    createContext_properties,
    createContext_description,
    createContext_contextName,
    createContext_source,
    createContext_contextType,
    createContextResponse_contextArn,
    createContextResponse_httpStatus,

    -- ** CreateDataQualityJobDefinition
    createDataQualityJobDefinition_tags,
    createDataQualityJobDefinition_networkConfig,
    createDataQualityJobDefinition_dataQualityBaselineConfig,
    createDataQualityJobDefinition_stoppingCondition,
    createDataQualityJobDefinition_jobDefinitionName,
    createDataQualityJobDefinition_dataQualityAppSpecification,
    createDataQualityJobDefinition_dataQualityJobInput,
    createDataQualityJobDefinition_dataQualityJobOutputConfig,
    createDataQualityJobDefinition_jobResources,
    createDataQualityJobDefinition_roleArn,
    createDataQualityJobDefinitionResponse_httpStatus,
    createDataQualityJobDefinitionResponse_jobDefinitionArn,

    -- ** CreateDeviceFleet
    createDeviceFleet_tags,
    createDeviceFleet_roleArn,
    createDeviceFleet_description,
    createDeviceFleet_enableIotRoleAlias,
    createDeviceFleet_deviceFleetName,
    createDeviceFleet_outputConfig,

    -- ** CreateDomain
    createDomain_tags,
    createDomain_domainSettings,
    createDomain_kmsKeyId,
    createDomain_homeEfsFileSystemKmsKeyId,
    createDomain_appSecurityGroupManagement,
    createDomain_appNetworkAccessType,
    createDomain_domainName,
    createDomain_authMode,
    createDomain_defaultUserSettings,
    createDomain_subnetIds,
    createDomain_vpcId,
    createDomainResponse_domainArn,
    createDomainResponse_url,
    createDomainResponse_httpStatus,

    -- ** CreateEdgeDeploymentPlan
    createEdgeDeploymentPlan_tags,
    createEdgeDeploymentPlan_stages,
    createEdgeDeploymentPlan_edgeDeploymentPlanName,
    createEdgeDeploymentPlan_modelConfigs,
    createEdgeDeploymentPlan_deviceFleetName,
    createEdgeDeploymentPlanResponse_httpStatus,
    createEdgeDeploymentPlanResponse_edgeDeploymentPlanArn,

    -- ** CreateEdgeDeploymentStage
    createEdgeDeploymentStage_edgeDeploymentPlanName,
    createEdgeDeploymentStage_stages,

    -- ** CreateEdgePackagingJob
    createEdgePackagingJob_tags,
    createEdgePackagingJob_resourceKey,
    createEdgePackagingJob_edgePackagingJobName,
    createEdgePackagingJob_compilationJobName,
    createEdgePackagingJob_modelName,
    createEdgePackagingJob_modelVersion,
    createEdgePackagingJob_roleArn,
    createEdgePackagingJob_outputConfig,

    -- ** CreateEndpoint
    createEndpoint_tags,
    createEndpoint_deploymentConfig,
    createEndpoint_endpointName,
    createEndpoint_endpointConfigName,
    createEndpointResponse_httpStatus,
    createEndpointResponse_endpointArn,

    -- ** CreateEndpointConfig
    createEndpointConfig_tags,
    createEndpointConfig_asyncInferenceConfig,
    createEndpointConfig_dataCaptureConfig,
    createEndpointConfig_kmsKeyId,
    createEndpointConfig_explainerConfig,
    createEndpointConfig_endpointConfigName,
    createEndpointConfig_productionVariants,
    createEndpointConfigResponse_httpStatus,
    createEndpointConfigResponse_endpointConfigArn,

    -- ** CreateExperiment
    createExperiment_tags,
    createExperiment_displayName,
    createExperiment_description,
    createExperiment_experimentName,
    createExperimentResponse_experimentArn,
    createExperimentResponse_httpStatus,

    -- ** CreateFeatureGroup
    createFeatureGroup_tags,
    createFeatureGroup_roleArn,
    createFeatureGroup_description,
    createFeatureGroup_onlineStoreConfig,
    createFeatureGroup_offlineStoreConfig,
    createFeatureGroup_featureGroupName,
    createFeatureGroup_recordIdentifierFeatureName,
    createFeatureGroup_eventTimeFeatureName,
    createFeatureGroup_featureDefinitions,
    createFeatureGroupResponse_httpStatus,
    createFeatureGroupResponse_featureGroupArn,

    -- ** CreateFlowDefinition
    createFlowDefinition_tags,
    createFlowDefinition_humanLoopActivationConfig,
    createFlowDefinition_humanLoopRequestSource,
    createFlowDefinition_flowDefinitionName,
    createFlowDefinition_humanLoopConfig,
    createFlowDefinition_outputConfig,
    createFlowDefinition_roleArn,
    createFlowDefinitionResponse_httpStatus,
    createFlowDefinitionResponse_flowDefinitionArn,

    -- ** CreateHumanTaskUi
    createHumanTaskUi_tags,
    createHumanTaskUi_humanTaskUiName,
    createHumanTaskUi_uiTemplate,
    createHumanTaskUiResponse_httpStatus,
    createHumanTaskUiResponse_humanTaskUiArn,

    -- ** CreateHyperParameterTuningJob
    createHyperParameterTuningJob_tags,
    createHyperParameterTuningJob_trainingJobDefinitions,
    createHyperParameterTuningJob_warmStartConfig,
    createHyperParameterTuningJob_trainingJobDefinition,
    createHyperParameterTuningJob_hyperParameterTuningJobName,
    createHyperParameterTuningJob_hyperParameterTuningJobConfig,
    createHyperParameterTuningJobResponse_httpStatus,
    createHyperParameterTuningJobResponse_hyperParameterTuningJobArn,

    -- ** CreateImage
    createImage_tags,
    createImage_displayName,
    createImage_description,
    createImage_imageName,
    createImage_roleArn,
    createImageResponse_imageArn,
    createImageResponse_httpStatus,

    -- ** CreateImageVersion
    createImageVersion_baseImage,
    createImageVersion_clientToken,
    createImageVersion_imageName,
    createImageVersionResponse_imageVersionArn,
    createImageVersionResponse_httpStatus,

    -- ** CreateInferenceRecommendationsJob
    createInferenceRecommendationsJob_tags,
    createInferenceRecommendationsJob_stoppingConditions,
    createInferenceRecommendationsJob_outputConfig,
    createInferenceRecommendationsJob_jobDescription,
    createInferenceRecommendationsJob_jobName,
    createInferenceRecommendationsJob_jobType,
    createInferenceRecommendationsJob_roleArn,
    createInferenceRecommendationsJob_inputConfig,
    createInferenceRecommendationsJobResponse_httpStatus,
    createInferenceRecommendationsJobResponse_jobArn,

    -- ** CreateLabelingJob
    createLabelingJob_tags,
    createLabelingJob_labelingJobAlgorithmsConfig,
    createLabelingJob_stoppingConditions,
    createLabelingJob_labelCategoryConfigS3Uri,
    createLabelingJob_labelingJobName,
    createLabelingJob_labelAttributeName,
    createLabelingJob_inputConfig,
    createLabelingJob_outputConfig,
    createLabelingJob_roleArn,
    createLabelingJob_humanTaskConfig,
    createLabelingJobResponse_httpStatus,
    createLabelingJobResponse_labelingJobArn,

    -- ** CreateModel
    createModel_tags,
    createModel_vpcConfig,
    createModel_enableNetworkIsolation,
    createModel_containers,
    createModel_primaryContainer,
    createModel_inferenceExecutionConfig,
    createModel_modelName,
    createModel_executionRoleArn,
    createModelResponse_httpStatus,
    createModelResponse_modelArn,

    -- ** CreateModelBiasJobDefinition
    createModelBiasJobDefinition_tags,
    createModelBiasJobDefinition_networkConfig,
    createModelBiasJobDefinition_modelBiasBaselineConfig,
    createModelBiasJobDefinition_stoppingCondition,
    createModelBiasJobDefinition_jobDefinitionName,
    createModelBiasJobDefinition_modelBiasAppSpecification,
    createModelBiasJobDefinition_modelBiasJobInput,
    createModelBiasJobDefinition_modelBiasJobOutputConfig,
    createModelBiasJobDefinition_jobResources,
    createModelBiasJobDefinition_roleArn,
    createModelBiasJobDefinitionResponse_httpStatus,
    createModelBiasJobDefinitionResponse_jobDefinitionArn,

    -- ** CreateModelExplainabilityJobDefinition
    createModelExplainabilityJobDefinition_tags,
    createModelExplainabilityJobDefinition_networkConfig,
    createModelExplainabilityJobDefinition_modelExplainabilityBaselineConfig,
    createModelExplainabilityJobDefinition_stoppingCondition,
    createModelExplainabilityJobDefinition_jobDefinitionName,
    createModelExplainabilityJobDefinition_modelExplainabilityAppSpecification,
    createModelExplainabilityJobDefinition_modelExplainabilityJobInput,
    createModelExplainabilityJobDefinition_modelExplainabilityJobOutputConfig,
    createModelExplainabilityJobDefinition_jobResources,
    createModelExplainabilityJobDefinition_roleArn,
    createModelExplainabilityJobDefinitionResponse_httpStatus,
    createModelExplainabilityJobDefinitionResponse_jobDefinitionArn,

    -- ** CreateModelPackage
    createModelPackage_tags,
    createModelPackage_modelPackageGroupName,
    createModelPackage_sourceAlgorithmSpecification,
    createModelPackage_clientToken,
    createModelPackage_validationSpecification,
    createModelPackage_samplePayloadUrl,
    createModelPackage_task,
    createModelPackage_certifyForMarketplace,
    createModelPackage_inferenceSpecification,
    createModelPackage_modelApprovalStatus,
    createModelPackage_metadataProperties,
    createModelPackage_domain,
    createModelPackage_modelPackageDescription,
    createModelPackage_driftCheckBaselines,
    createModelPackage_modelMetrics,
    createModelPackage_additionalInferenceSpecifications,
    createModelPackage_customerMetadataProperties,
    createModelPackage_modelPackageName,
    createModelPackageResponse_httpStatus,
    createModelPackageResponse_modelPackageArn,

    -- ** CreateModelPackageGroup
    createModelPackageGroup_tags,
    createModelPackageGroup_modelPackageGroupDescription,
    createModelPackageGroup_modelPackageGroupName,
    createModelPackageGroupResponse_httpStatus,
    createModelPackageGroupResponse_modelPackageGroupArn,

    -- ** CreateModelQualityJobDefinition
    createModelQualityJobDefinition_tags,
    createModelQualityJobDefinition_modelQualityBaselineConfig,
    createModelQualityJobDefinition_networkConfig,
    createModelQualityJobDefinition_stoppingCondition,
    createModelQualityJobDefinition_jobDefinitionName,
    createModelQualityJobDefinition_modelQualityAppSpecification,
    createModelQualityJobDefinition_modelQualityJobInput,
    createModelQualityJobDefinition_modelQualityJobOutputConfig,
    createModelQualityJobDefinition_jobResources,
    createModelQualityJobDefinition_roleArn,
    createModelQualityJobDefinitionResponse_httpStatus,
    createModelQualityJobDefinitionResponse_jobDefinitionArn,

    -- ** CreateMonitoringSchedule
    createMonitoringSchedule_tags,
    createMonitoringSchedule_monitoringScheduleName,
    createMonitoringSchedule_monitoringScheduleConfig,
    createMonitoringScheduleResponse_httpStatus,
    createMonitoringScheduleResponse_monitoringScheduleArn,

    -- ** CreateNotebookInstance
    createNotebookInstance_tags,
    createNotebookInstance_securityGroupIds,
    createNotebookInstance_instanceMetadataServiceConfiguration,
    createNotebookInstance_subnetId,
    createNotebookInstance_acceleratorTypes,
    createNotebookInstance_directInternetAccess,
    createNotebookInstance_additionalCodeRepositories,
    createNotebookInstance_kmsKeyId,
    createNotebookInstance_platformIdentifier,
    createNotebookInstance_volumeSizeInGB,
    createNotebookInstance_lifecycleConfigName,
    createNotebookInstance_defaultCodeRepository,
    createNotebookInstance_rootAccess,
    createNotebookInstance_notebookInstanceName,
    createNotebookInstance_instanceType,
    createNotebookInstance_roleArn,
    createNotebookInstanceResponse_notebookInstanceArn,
    createNotebookInstanceResponse_httpStatus,

    -- ** CreateNotebookInstanceLifecycleConfig
    createNotebookInstanceLifecycleConfig_onCreate,
    createNotebookInstanceLifecycleConfig_onStart,
    createNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,
    createNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn,
    createNotebookInstanceLifecycleConfigResponse_httpStatus,

    -- ** CreatePipeline
    createPipeline_tags,
    createPipeline_pipelineDefinitionS3Location,
    createPipeline_pipelineDisplayName,
    createPipeline_pipelineDefinition,
    createPipeline_pipelineDescription,
    createPipeline_parallelismConfiguration,
    createPipeline_pipelineName,
    createPipeline_clientRequestToken,
    createPipeline_roleArn,
    createPipelineResponse_pipelineArn,
    createPipelineResponse_httpStatus,

    -- ** CreatePresignedDomainUrl
    createPresignedDomainUrl_expiresInSeconds,
    createPresignedDomainUrl_sessionExpirationDurationInSeconds,
    createPresignedDomainUrl_domainId,
    createPresignedDomainUrl_userProfileName,
    createPresignedDomainUrlResponse_authorizedUrl,
    createPresignedDomainUrlResponse_httpStatus,

    -- ** CreatePresignedNotebookInstanceUrl
    createPresignedNotebookInstanceUrl_sessionExpirationDurationInSeconds,
    createPresignedNotebookInstanceUrl_notebookInstanceName,
    createPresignedNotebookInstanceUrlResponse_authorizedUrl,
    createPresignedNotebookInstanceUrlResponse_httpStatus,

    -- ** CreateProcessingJob
    createProcessingJob_tags,
    createProcessingJob_environment,
    createProcessingJob_networkConfig,
    createProcessingJob_experimentConfig,
    createProcessingJob_processingInputs,
    createProcessingJob_stoppingCondition,
    createProcessingJob_processingOutputConfig,
    createProcessingJob_processingJobName,
    createProcessingJob_processingResources,
    createProcessingJob_appSpecification,
    createProcessingJob_roleArn,
    createProcessingJobResponse_httpStatus,
    createProcessingJobResponse_processingJobArn,

    -- ** CreateProject
    createProject_tags,
    createProject_projectDescription,
    createProject_projectName,
    createProject_serviceCatalogProvisioningDetails,
    createProjectResponse_httpStatus,
    createProjectResponse_projectArn,
    createProjectResponse_projectId,

    -- ** CreateStudioLifecycleConfig
    createStudioLifecycleConfig_tags,
    createStudioLifecycleConfig_studioLifecycleConfigName,
    createStudioLifecycleConfig_studioLifecycleConfigContent,
    createStudioLifecycleConfig_studioLifecycleConfigAppType,
    createStudioLifecycleConfigResponse_studioLifecycleConfigArn,
    createStudioLifecycleConfigResponse_httpStatus,

    -- ** CreateTrainingJob
    createTrainingJob_tags,
    createTrainingJob_profilerConfig,
    createTrainingJob_enableManagedSpotTraining,
    createTrainingJob_environment,
    createTrainingJob_retryStrategy,
    createTrainingJob_vpcConfig,
    createTrainingJob_enableNetworkIsolation,
    createTrainingJob_profilerRuleConfigurations,
    createTrainingJob_experimentConfig,
    createTrainingJob_checkpointConfig,
    createTrainingJob_debugHookConfig,
    createTrainingJob_enableInterContainerTrafficEncryption,
    createTrainingJob_debugRuleConfigurations,
    createTrainingJob_tensorBoardOutputConfig,
    createTrainingJob_inputDataConfig,
    createTrainingJob_hyperParameters,
    createTrainingJob_trainingJobName,
    createTrainingJob_algorithmSpecification,
    createTrainingJob_roleArn,
    createTrainingJob_outputDataConfig,
    createTrainingJob_resourceConfig,
    createTrainingJob_stoppingCondition,
    createTrainingJobResponse_httpStatus,
    createTrainingJobResponse_trainingJobArn,

    -- ** CreateTransformJob
    createTransformJob_tags,
    createTransformJob_maxConcurrentTransforms,
    createTransformJob_modelClientConfig,
    createTransformJob_environment,
    createTransformJob_experimentConfig,
    createTransformJob_maxPayloadInMB,
    createTransformJob_batchStrategy,
    createTransformJob_dataCaptureConfig,
    createTransformJob_dataProcessing,
    createTransformJob_transformJobName,
    createTransformJob_modelName,
    createTransformJob_transformInput,
    createTransformJob_transformOutput,
    createTransformJob_transformResources,
    createTransformJobResponse_httpStatus,
    createTransformJobResponse_transformJobArn,

    -- ** CreateTrial
    createTrial_tags,
    createTrial_metadataProperties,
    createTrial_displayName,
    createTrial_trialName,
    createTrial_experimentName,
    createTrialResponse_trialArn,
    createTrialResponse_httpStatus,

    -- ** CreateTrialComponent
    createTrialComponent_tags,
    createTrialComponent_metadataProperties,
    createTrialComponent_displayName,
    createTrialComponent_status,
    createTrialComponent_outputArtifacts,
    createTrialComponent_endTime,
    createTrialComponent_inputArtifacts,
    createTrialComponent_startTime,
    createTrialComponent_parameters,
    createTrialComponent_trialComponentName,
    createTrialComponentResponse_trialComponentArn,
    createTrialComponentResponse_httpStatus,

    -- ** CreateUserProfile
    createUserProfile_tags,
    createUserProfile_singleSignOnUserValue,
    createUserProfile_userSettings,
    createUserProfile_singleSignOnUserIdentifier,
    createUserProfile_domainId,
    createUserProfile_userProfileName,
    createUserProfileResponse_userProfileArn,
    createUserProfileResponse_httpStatus,

    -- ** CreateWorkforce
    createWorkforce_tags,
    createWorkforce_cognitoConfig,
    createWorkforce_sourceIpConfig,
    createWorkforce_workforceVpcConfig,
    createWorkforce_oidcConfig,
    createWorkforce_workforceName,
    createWorkforceResponse_httpStatus,
    createWorkforceResponse_workforceArn,

    -- ** CreateWorkteam
    createWorkteam_tags,
    createWorkteam_notificationConfiguration,
    createWorkteam_workforceName,
    createWorkteam_workteamName,
    createWorkteam_memberDefinitions,
    createWorkteam_description,
    createWorkteamResponse_workteamArn,
    createWorkteamResponse_httpStatus,

    -- ** DeleteAction
    deleteAction_actionName,
    deleteActionResponse_actionArn,
    deleteActionResponse_httpStatus,

    -- ** DeleteAlgorithm
    deleteAlgorithm_algorithmName,

    -- ** DeleteApp
    deleteApp_domainId,
    deleteApp_userProfileName,
    deleteApp_appType,
    deleteApp_appName,

    -- ** DeleteAppImageConfig
    deleteAppImageConfig_appImageConfigName,

    -- ** DeleteArtifact
    deleteArtifact_artifactArn,
    deleteArtifact_source,
    deleteArtifactResponse_artifactArn,
    deleteArtifactResponse_httpStatus,

    -- ** DeleteAssociation
    deleteAssociation_sourceArn,
    deleteAssociation_destinationArn,
    deleteAssociationResponse_sourceArn,
    deleteAssociationResponse_destinationArn,
    deleteAssociationResponse_httpStatus,

    -- ** DeleteCodeRepository
    deleteCodeRepository_codeRepositoryName,

    -- ** DeleteContext
    deleteContext_contextName,
    deleteContextResponse_contextArn,
    deleteContextResponse_httpStatus,

    -- ** DeleteDataQualityJobDefinition
    deleteDataQualityJobDefinition_jobDefinitionName,

    -- ** DeleteDeviceFleet
    deleteDeviceFleet_deviceFleetName,

    -- ** DeleteDomain
    deleteDomain_retentionPolicy,
    deleteDomain_domainId,

    -- ** DeleteEdgeDeploymentPlan
    deleteEdgeDeploymentPlan_edgeDeploymentPlanName,

    -- ** DeleteEdgeDeploymentStage
    deleteEdgeDeploymentStage_edgeDeploymentPlanName,
    deleteEdgeDeploymentStage_stageName,

    -- ** DeleteEndpoint
    deleteEndpoint_endpointName,

    -- ** DeleteEndpointConfig
    deleteEndpointConfig_endpointConfigName,

    -- ** DeleteExperiment
    deleteExperiment_experimentName,
    deleteExperimentResponse_experimentArn,
    deleteExperimentResponse_httpStatus,

    -- ** DeleteFeatureGroup
    deleteFeatureGroup_featureGroupName,

    -- ** DeleteFlowDefinition
    deleteFlowDefinition_flowDefinitionName,
    deleteFlowDefinitionResponse_httpStatus,

    -- ** DeleteHumanTaskUi
    deleteHumanTaskUi_humanTaskUiName,
    deleteHumanTaskUiResponse_httpStatus,

    -- ** DeleteImage
    deleteImage_imageName,
    deleteImageResponse_httpStatus,

    -- ** DeleteImageVersion
    deleteImageVersion_imageName,
    deleteImageVersion_version,
    deleteImageVersionResponse_httpStatus,

    -- ** DeleteModel
    deleteModel_modelName,

    -- ** DeleteModelBiasJobDefinition
    deleteModelBiasJobDefinition_jobDefinitionName,

    -- ** DeleteModelExplainabilityJobDefinition
    deleteModelExplainabilityJobDefinition_jobDefinitionName,

    -- ** DeleteModelPackage
    deleteModelPackage_modelPackageName,

    -- ** DeleteModelPackageGroup
    deleteModelPackageGroup_modelPackageGroupName,

    -- ** DeleteModelPackageGroupPolicy
    deleteModelPackageGroupPolicy_modelPackageGroupName,

    -- ** DeleteModelQualityJobDefinition
    deleteModelQualityJobDefinition_jobDefinitionName,

    -- ** DeleteMonitoringSchedule
    deleteMonitoringSchedule_monitoringScheduleName,

    -- ** DeleteNotebookInstance
    deleteNotebookInstance_notebookInstanceName,

    -- ** DeleteNotebookInstanceLifecycleConfig
    deleteNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,

    -- ** DeletePipeline
    deletePipeline_pipelineName,
    deletePipeline_clientRequestToken,
    deletePipelineResponse_pipelineArn,
    deletePipelineResponse_httpStatus,

    -- ** DeleteProject
    deleteProject_projectName,

    -- ** DeleteStudioLifecycleConfig
    deleteStudioLifecycleConfig_studioLifecycleConfigName,

    -- ** DeleteTags
    deleteTags_resourceArn,
    deleteTags_tagKeys,
    deleteTagsResponse_httpStatus,

    -- ** DeleteTrial
    deleteTrial_trialName,
    deleteTrialResponse_trialArn,
    deleteTrialResponse_httpStatus,

    -- ** DeleteTrialComponent
    deleteTrialComponent_trialComponentName,
    deleteTrialComponentResponse_trialComponentArn,
    deleteTrialComponentResponse_httpStatus,

    -- ** DeleteUserProfile
    deleteUserProfile_domainId,
    deleteUserProfile_userProfileName,

    -- ** DeleteWorkforce
    deleteWorkforce_workforceName,
    deleteWorkforceResponse_httpStatus,

    -- ** DeleteWorkteam
    deleteWorkteam_workteamName,
    deleteWorkteamResponse_httpStatus,
    deleteWorkteamResponse_success,

    -- ** DeregisterDevices
    deregisterDevices_deviceFleetName,
    deregisterDevices_deviceNames,

    -- ** DescribeAction
    describeAction_actionName,
    describeActionResponse_actionName,
    describeActionResponse_actionType,
    describeActionResponse_metadataProperties,
    describeActionResponse_properties,
    describeActionResponse_status,
    describeActionResponse_description,
    describeActionResponse_lastModifiedTime,
    describeActionResponse_source,
    describeActionResponse_actionArn,
    describeActionResponse_lineageGroupArn,
    describeActionResponse_creationTime,
    describeActionResponse_lastModifiedBy,
    describeActionResponse_createdBy,
    describeActionResponse_httpStatus,

    -- ** DescribeAlgorithm
    describeAlgorithm_algorithmName,
    describeAlgorithmResponse_validationSpecification,
    describeAlgorithmResponse_certifyForMarketplace,
    describeAlgorithmResponse_inferenceSpecification,
    describeAlgorithmResponse_productId,
    describeAlgorithmResponse_algorithmDescription,
    describeAlgorithmResponse_httpStatus,
    describeAlgorithmResponse_algorithmName,
    describeAlgorithmResponse_algorithmArn,
    describeAlgorithmResponse_creationTime,
    describeAlgorithmResponse_trainingSpecification,
    describeAlgorithmResponse_algorithmStatus,
    describeAlgorithmResponse_algorithmStatusDetails,

    -- ** DescribeApp
    describeApp_domainId,
    describeApp_userProfileName,
    describeApp_appType,
    describeApp_appName,
    describeAppResponse_resourceSpec,
    describeAppResponse_appName,
    describeAppResponse_appType,
    describeAppResponse_status,
    describeAppResponse_appArn,
    describeAppResponse_lastUserActivityTimestamp,
    describeAppResponse_userProfileName,
    describeAppResponse_creationTime,
    describeAppResponse_domainId,
    describeAppResponse_lastHealthCheckTimestamp,
    describeAppResponse_failureReason,
    describeAppResponse_httpStatus,

    -- ** DescribeAppImageConfig
    describeAppImageConfig_appImageConfigName,
    describeAppImageConfigResponse_appImageConfigArn,
    describeAppImageConfigResponse_appImageConfigName,
    describeAppImageConfigResponse_kernelGatewayImageConfig,
    describeAppImageConfigResponse_lastModifiedTime,
    describeAppImageConfigResponse_creationTime,
    describeAppImageConfigResponse_httpStatus,

    -- ** DescribeArtifact
    describeArtifact_artifactArn,
    describeArtifactResponse_artifactName,
    describeArtifactResponse_artifactType,
    describeArtifactResponse_metadataProperties,
    describeArtifactResponse_properties,
    describeArtifactResponse_artifactArn,
    describeArtifactResponse_lastModifiedTime,
    describeArtifactResponse_source,
    describeArtifactResponse_lineageGroupArn,
    describeArtifactResponse_creationTime,
    describeArtifactResponse_lastModifiedBy,
    describeArtifactResponse_createdBy,
    describeArtifactResponse_httpStatus,

    -- ** DescribeAutoMLJob
    describeAutoMLJob_autoMLJobName,
    describeAutoMLJobResponse_autoMLJobConfig,
    describeAutoMLJobResponse_autoMLJobArtifacts,
    describeAutoMLJobResponse_autoMLJobObjective,
    describeAutoMLJobResponse_partialFailureReasons,
    describeAutoMLJobResponse_endTime,
    describeAutoMLJobResponse_resolvedAttributes,
    describeAutoMLJobResponse_modelDeployConfig,
    describeAutoMLJobResponse_modelDeployResult,
    describeAutoMLJobResponse_problemType,
    describeAutoMLJobResponse_bestCandidate,
    describeAutoMLJobResponse_generateCandidateDefinitionsOnly,
    describeAutoMLJobResponse_failureReason,
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

    -- ** DescribeCodeRepository
    describeCodeRepository_codeRepositoryName,
    describeCodeRepositoryResponse_gitConfig,
    describeCodeRepositoryResponse_httpStatus,
    describeCodeRepositoryResponse_codeRepositoryName,
    describeCodeRepositoryResponse_codeRepositoryArn,
    describeCodeRepositoryResponse_creationTime,
    describeCodeRepositoryResponse_lastModifiedTime,

    -- ** DescribeCompilationJob
    describeCompilationJob_compilationJobName,
    describeCompilationJobResponse_compilationEndTime,
    describeCompilationJobResponse_inferenceImage,
    describeCompilationJobResponse_vpcConfig,
    describeCompilationJobResponse_modelDigests,
    describeCompilationJobResponse_compilationStartTime,
    describeCompilationJobResponse_modelPackageVersionArn,
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

    -- ** DescribeContext
    describeContext_contextName,
    describeContextResponse_properties,
    describeContextResponse_contextName,
    describeContextResponse_description,
    describeContextResponse_lastModifiedTime,
    describeContextResponse_source,
    describeContextResponse_lineageGroupArn,
    describeContextResponse_creationTime,
    describeContextResponse_lastModifiedBy,
    describeContextResponse_contextType,
    describeContextResponse_createdBy,
    describeContextResponse_contextArn,
    describeContextResponse_httpStatus,

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

    -- ** DescribeDevice
    describeDevice_nextToken,
    describeDevice_deviceName,
    describeDevice_deviceFleetName,
    describeDeviceResponse_nextToken,
    describeDeviceResponse_models,
    describeDeviceResponse_iotThingName,
    describeDeviceResponse_maxModels,
    describeDeviceResponse_description,
    describeDeviceResponse_latestHeartbeat,
    describeDeviceResponse_deviceArn,
    describeDeviceResponse_agentVersion,
    describeDeviceResponse_httpStatus,
    describeDeviceResponse_deviceName,
    describeDeviceResponse_deviceFleetName,
    describeDeviceResponse_registrationTime,

    -- ** DescribeDeviceFleet
    describeDeviceFleet_deviceFleetName,
    describeDeviceFleetResponse_iotRoleAlias,
    describeDeviceFleetResponse_roleArn,
    describeDeviceFleetResponse_description,
    describeDeviceFleetResponse_httpStatus,
    describeDeviceFleetResponse_deviceFleetName,
    describeDeviceFleetResponse_deviceFleetArn,
    describeDeviceFleetResponse_outputConfig,
    describeDeviceFleetResponse_creationTime,
    describeDeviceFleetResponse_lastModifiedTime,

    -- ** DescribeDomain
    describeDomain_domainId,
    describeDomainResponse_singleSignOnManagedApplicationInstanceId,
    describeDomainResponse_domainName,
    describeDomainResponse_domainArn,
    describeDomainResponse_url,
    describeDomainResponse_status,
    describeDomainResponse_lastModifiedTime,
    describeDomainResponse_domainSettings,
    describeDomainResponse_authMode,
    describeDomainResponse_securityGroupIdForDomainBoundary,
    describeDomainResponse_kmsKeyId,
    describeDomainResponse_creationTime,
    describeDomainResponse_domainId,
    describeDomainResponse_defaultUserSettings,
    describeDomainResponse_vpcId,
    describeDomainResponse_homeEfsFileSystemKmsKeyId,
    describeDomainResponse_appSecurityGroupManagement,
    describeDomainResponse_subnetIds,
    describeDomainResponse_failureReason,
    describeDomainResponse_appNetworkAccessType,
    describeDomainResponse_homeEfsFileSystemId,
    describeDomainResponse_httpStatus,

    -- ** DescribeEdgeDeploymentPlan
    describeEdgeDeploymentPlan_nextToken,
    describeEdgeDeploymentPlan_maxResults,
    describeEdgeDeploymentPlan_edgeDeploymentPlanName,
    describeEdgeDeploymentPlanResponse_nextToken,
    describeEdgeDeploymentPlanResponse_edgeDeploymentFailed,
    describeEdgeDeploymentPlanResponse_lastModifiedTime,
    describeEdgeDeploymentPlanResponse_creationTime,
    describeEdgeDeploymentPlanResponse_edgeDeploymentPending,
    describeEdgeDeploymentPlanResponse_edgeDeploymentSuccess,
    describeEdgeDeploymentPlanResponse_httpStatus,
    describeEdgeDeploymentPlanResponse_edgeDeploymentPlanArn,
    describeEdgeDeploymentPlanResponse_edgeDeploymentPlanName,
    describeEdgeDeploymentPlanResponse_modelConfigs,
    describeEdgeDeploymentPlanResponse_deviceFleetName,
    describeEdgeDeploymentPlanResponse_stages,

    -- ** DescribeEdgePackagingJob
    describeEdgePackagingJob_edgePackagingJobName,
    describeEdgePackagingJobResponse_modelSignature,
    describeEdgePackagingJobResponse_roleArn,
    describeEdgePackagingJobResponse_modelArtifact,
    describeEdgePackagingJobResponse_compilationJobName,
    describeEdgePackagingJobResponse_modelVersion,
    describeEdgePackagingJobResponse_presetDeploymentOutput,
    describeEdgePackagingJobResponse_lastModifiedTime,
    describeEdgePackagingJobResponse_resourceKey,
    describeEdgePackagingJobResponse_modelName,
    describeEdgePackagingJobResponse_edgePackagingJobStatusMessage,
    describeEdgePackagingJobResponse_creationTime,
    describeEdgePackagingJobResponse_outputConfig,
    describeEdgePackagingJobResponse_httpStatus,
    describeEdgePackagingJobResponse_edgePackagingJobArn,
    describeEdgePackagingJobResponse_edgePackagingJobName,
    describeEdgePackagingJobResponse_edgePackagingJobStatus,

    -- ** DescribeEndpoint
    describeEndpoint_endpointName,
    describeEndpointResponse_asyncInferenceConfig,
    describeEndpointResponse_pendingDeploymentSummary,
    describeEndpointResponse_dataCaptureConfig,
    describeEndpointResponse_lastDeploymentConfig,
    describeEndpointResponse_productionVariants,
    describeEndpointResponse_failureReason,
    describeEndpointResponse_explainerConfig,
    describeEndpointResponse_httpStatus,
    describeEndpointResponse_endpointName,
    describeEndpointResponse_endpointArn,
    describeEndpointResponse_endpointConfigName,
    describeEndpointResponse_endpointStatus,
    describeEndpointResponse_creationTime,
    describeEndpointResponse_lastModifiedTime,

    -- ** DescribeEndpointConfig
    describeEndpointConfig_endpointConfigName,
    describeEndpointConfigResponse_asyncInferenceConfig,
    describeEndpointConfigResponse_dataCaptureConfig,
    describeEndpointConfigResponse_kmsKeyId,
    describeEndpointConfigResponse_explainerConfig,
    describeEndpointConfigResponse_httpStatus,
    describeEndpointConfigResponse_endpointConfigName,
    describeEndpointConfigResponse_endpointConfigArn,
    describeEndpointConfigResponse_productionVariants,
    describeEndpointConfigResponse_creationTime,

    -- ** DescribeExperiment
    describeExperiment_experimentName,
    describeExperimentResponse_displayName,
    describeExperimentResponse_description,
    describeExperimentResponse_lastModifiedTime,
    describeExperimentResponse_source,
    describeExperimentResponse_experimentArn,
    describeExperimentResponse_creationTime,
    describeExperimentResponse_lastModifiedBy,
    describeExperimentResponse_createdBy,
    describeExperimentResponse_experimentName,
    describeExperimentResponse_httpStatus,

    -- ** DescribeFeatureGroup
    describeFeatureGroup_nextToken,
    describeFeatureGroup_featureGroupName,
    describeFeatureGroupResponse_roleArn,
    describeFeatureGroupResponse_description,
    describeFeatureGroupResponse_offlineStoreStatus,
    describeFeatureGroupResponse_onlineStoreConfig,
    describeFeatureGroupResponse_lastModifiedTime,
    describeFeatureGroupResponse_onlineStoreTotalSizeBytes,
    describeFeatureGroupResponse_lastUpdateStatus,
    describeFeatureGroupResponse_featureGroupStatus,
    describeFeatureGroupResponse_offlineStoreConfig,
    describeFeatureGroupResponse_failureReason,
    describeFeatureGroupResponse_httpStatus,
    describeFeatureGroupResponse_featureGroupArn,
    describeFeatureGroupResponse_featureGroupName,
    describeFeatureGroupResponse_recordIdentifierFeatureName,
    describeFeatureGroupResponse_eventTimeFeatureName,
    describeFeatureGroupResponse_featureDefinitions,
    describeFeatureGroupResponse_creationTime,
    describeFeatureGroupResponse_nextToken,

    -- ** DescribeFeatureMetadata
    describeFeatureMetadata_featureGroupName,
    describeFeatureMetadata_featureName,
    describeFeatureMetadataResponse_description,
    describeFeatureMetadataResponse_parameters,
    describeFeatureMetadataResponse_httpStatus,
    describeFeatureMetadataResponse_featureGroupArn,
    describeFeatureMetadataResponse_featureGroupName,
    describeFeatureMetadataResponse_featureName,
    describeFeatureMetadataResponse_featureType,
    describeFeatureMetadataResponse_creationTime,
    describeFeatureMetadataResponse_lastModifiedTime,

    -- ** DescribeFlowDefinition
    describeFlowDefinition_flowDefinitionName,
    describeFlowDefinitionResponse_humanLoopActivationConfig,
    describeFlowDefinitionResponse_humanLoopRequestSource,
    describeFlowDefinitionResponse_failureReason,
    describeFlowDefinitionResponse_httpStatus,
    describeFlowDefinitionResponse_flowDefinitionArn,
    describeFlowDefinitionResponse_flowDefinitionName,
    describeFlowDefinitionResponse_flowDefinitionStatus,
    describeFlowDefinitionResponse_creationTime,
    describeFlowDefinitionResponse_humanLoopConfig,
    describeFlowDefinitionResponse_outputConfig,
    describeFlowDefinitionResponse_roleArn,

    -- ** DescribeHumanTaskUi
    describeHumanTaskUi_humanTaskUiName,
    describeHumanTaskUiResponse_humanTaskUiStatus,
    describeHumanTaskUiResponse_httpStatus,
    describeHumanTaskUiResponse_humanTaskUiArn,
    describeHumanTaskUiResponse_humanTaskUiName,
    describeHumanTaskUiResponse_creationTime,
    describeHumanTaskUiResponse_uiTemplate,

    -- ** DescribeHyperParameterTuningJob
    describeHyperParameterTuningJob_hyperParameterTuningJobName,
    describeHyperParameterTuningJobResponse_overallBestTrainingJob,
    describeHyperParameterTuningJobResponse_bestTrainingJob,
    describeHyperParameterTuningJobResponse_trainingJobDefinitions,
    describeHyperParameterTuningJobResponse_lastModifiedTime,
    describeHyperParameterTuningJobResponse_warmStartConfig,
    describeHyperParameterTuningJobResponse_hyperParameterTuningEndTime,
    describeHyperParameterTuningJobResponse_trainingJobDefinition,
    describeHyperParameterTuningJobResponse_failureReason,
    describeHyperParameterTuningJobResponse_httpStatus,
    describeHyperParameterTuningJobResponse_hyperParameterTuningJobName,
    describeHyperParameterTuningJobResponse_hyperParameterTuningJobArn,
    describeHyperParameterTuningJobResponse_hyperParameterTuningJobConfig,
    describeHyperParameterTuningJobResponse_hyperParameterTuningJobStatus,
    describeHyperParameterTuningJobResponse_creationTime,
    describeHyperParameterTuningJobResponse_trainingJobStatusCounters,
    describeHyperParameterTuningJobResponse_objectiveStatusCounters,

    -- ** DescribeImage
    describeImage_imageName,
    describeImageResponse_roleArn,
    describeImageResponse_imageStatus,
    describeImageResponse_displayName,
    describeImageResponse_imageArn,
    describeImageResponse_description,
    describeImageResponse_lastModifiedTime,
    describeImageResponse_creationTime,
    describeImageResponse_failureReason,
    describeImageResponse_imageName,
    describeImageResponse_httpStatus,

    -- ** DescribeImageVersion
    describeImageVersion_version,
    describeImageVersion_imageName,
    describeImageVersionResponse_baseImage,
    describeImageVersionResponse_imageVersionArn,
    describeImageVersionResponse_imageArn,
    describeImageVersionResponse_lastModifiedTime,
    describeImageVersionResponse_containerImage,
    describeImageVersionResponse_creationTime,
    describeImageVersionResponse_imageVersionStatus,
    describeImageVersionResponse_failureReason,
    describeImageVersionResponse_version,
    describeImageVersionResponse_httpStatus,

    -- ** DescribeInferenceRecommendationsJob
    describeInferenceRecommendationsJob_jobName,
    describeInferenceRecommendationsJobResponse_endpointPerformances,
    describeInferenceRecommendationsJobResponse_inferenceRecommendations,
    describeInferenceRecommendationsJobResponse_stoppingConditions,
    describeInferenceRecommendationsJobResponse_completionTime,
    describeInferenceRecommendationsJobResponse_failureReason,
    describeInferenceRecommendationsJobResponse_jobDescription,
    describeInferenceRecommendationsJobResponse_httpStatus,
    describeInferenceRecommendationsJobResponse_jobName,
    describeInferenceRecommendationsJobResponse_jobType,
    describeInferenceRecommendationsJobResponse_jobArn,
    describeInferenceRecommendationsJobResponse_roleArn,
    describeInferenceRecommendationsJobResponse_status,
    describeInferenceRecommendationsJobResponse_creationTime,
    describeInferenceRecommendationsJobResponse_lastModifiedTime,
    describeInferenceRecommendationsJobResponse_inputConfig,

    -- ** DescribeLabelingJob
    describeLabelingJob_labelingJobName,
    describeLabelingJobResponse_tags,
    describeLabelingJobResponse_labelingJobAlgorithmsConfig,
    describeLabelingJobResponse_stoppingConditions,
    describeLabelingJobResponse_labelAttributeName,
    describeLabelingJobResponse_labelingJobOutput,
    describeLabelingJobResponse_labelCategoryConfigS3Uri,
    describeLabelingJobResponse_failureReason,
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

    -- ** DescribeLineageGroup
    describeLineageGroup_lineageGroupName,
    describeLineageGroupResponse_lineageGroupName,
    describeLineageGroupResponse_displayName,
    describeLineageGroupResponse_description,
    describeLineageGroupResponse_lastModifiedTime,
    describeLineageGroupResponse_lineageGroupArn,
    describeLineageGroupResponse_creationTime,
    describeLineageGroupResponse_lastModifiedBy,
    describeLineageGroupResponse_createdBy,
    describeLineageGroupResponse_httpStatus,

    -- ** DescribeModel
    describeModel_modelName,
    describeModelResponse_vpcConfig,
    describeModelResponse_enableNetworkIsolation,
    describeModelResponse_containers,
    describeModelResponse_primaryContainer,
    describeModelResponse_inferenceExecutionConfig,
    describeModelResponse_httpStatus,
    describeModelResponse_modelName,
    describeModelResponse_executionRoleArn,
    describeModelResponse_creationTime,
    describeModelResponse_modelArn,

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

    -- ** DescribeModelPackage
    describeModelPackage_modelPackageName,
    describeModelPackageResponse_modelPackageVersion,
    describeModelPackageResponse_modelPackageGroupName,
    describeModelPackageResponse_sourceAlgorithmSpecification,
    describeModelPackageResponse_validationSpecification,
    describeModelPackageResponse_samplePayloadUrl,
    describeModelPackageResponse_task,
    describeModelPackageResponse_certifyForMarketplace,
    describeModelPackageResponse_inferenceSpecification,
    describeModelPackageResponse_modelApprovalStatus,
    describeModelPackageResponse_metadataProperties,
    describeModelPackageResponse_domain,
    describeModelPackageResponse_modelPackageDescription,
    describeModelPackageResponse_driftCheckBaselines,
    describeModelPackageResponse_approvalDescription,
    describeModelPackageResponse_lastModifiedTime,
    describeModelPackageResponse_modelMetrics,
    describeModelPackageResponse_lastModifiedBy,
    describeModelPackageResponse_additionalInferenceSpecifications,
    describeModelPackageResponse_createdBy,
    describeModelPackageResponse_customerMetadataProperties,
    describeModelPackageResponse_httpStatus,
    describeModelPackageResponse_modelPackageName,
    describeModelPackageResponse_modelPackageArn,
    describeModelPackageResponse_creationTime,
    describeModelPackageResponse_modelPackageStatus,
    describeModelPackageResponse_modelPackageStatusDetails,

    -- ** DescribeModelPackageGroup
    describeModelPackageGroup_modelPackageGroupName,
    describeModelPackageGroupResponse_modelPackageGroupDescription,
    describeModelPackageGroupResponse_httpStatus,
    describeModelPackageGroupResponse_modelPackageGroupName,
    describeModelPackageGroupResponse_modelPackageGroupArn,
    describeModelPackageGroupResponse_creationTime,
    describeModelPackageGroupResponse_createdBy,
    describeModelPackageGroupResponse_modelPackageGroupStatus,

    -- ** DescribeModelQualityJobDefinition
    describeModelQualityJobDefinition_jobDefinitionName,
    describeModelQualityJobDefinitionResponse_modelQualityBaselineConfig,
    describeModelQualityJobDefinitionResponse_networkConfig,
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

    -- ** DescribeMonitoringSchedule
    describeMonitoringSchedule_monitoringScheduleName,
    describeMonitoringScheduleResponse_endpointName,
    describeMonitoringScheduleResponse_monitoringType,
    describeMonitoringScheduleResponse_lastMonitoringExecutionSummary,
    describeMonitoringScheduleResponse_failureReason,
    describeMonitoringScheduleResponse_httpStatus,
    describeMonitoringScheduleResponse_monitoringScheduleArn,
    describeMonitoringScheduleResponse_monitoringScheduleName,
    describeMonitoringScheduleResponse_monitoringScheduleStatus,
    describeMonitoringScheduleResponse_creationTime,
    describeMonitoringScheduleResponse_lastModifiedTime,
    describeMonitoringScheduleResponse_monitoringScheduleConfig,

    -- ** DescribeNotebookInstance
    describeNotebookInstance_notebookInstanceName,
    describeNotebookInstanceResponse_roleArn,
    describeNotebookInstanceResponse_notebookInstanceLifecycleConfigName,
    describeNotebookInstanceResponse_notebookInstanceStatus,
    describeNotebookInstanceResponse_instanceMetadataServiceConfiguration,
    describeNotebookInstanceResponse_subnetId,
    describeNotebookInstanceResponse_acceleratorTypes,
    describeNotebookInstanceResponse_url,
    describeNotebookInstanceResponse_instanceType,
    describeNotebookInstanceResponse_lastModifiedTime,
    describeNotebookInstanceResponse_directInternetAccess,
    describeNotebookInstanceResponse_networkInterfaceId,
    describeNotebookInstanceResponse_securityGroups,
    describeNotebookInstanceResponse_notebookInstanceArn,
    describeNotebookInstanceResponse_additionalCodeRepositories,
    describeNotebookInstanceResponse_kmsKeyId,
    describeNotebookInstanceResponse_creationTime,
    describeNotebookInstanceResponse_platformIdentifier,
    describeNotebookInstanceResponse_notebookInstanceName,
    describeNotebookInstanceResponse_volumeSizeInGB,
    describeNotebookInstanceResponse_failureReason,
    describeNotebookInstanceResponse_defaultCodeRepository,
    describeNotebookInstanceResponse_rootAccess,
    describeNotebookInstanceResponse_httpStatus,

    -- ** DescribeNotebookInstanceLifecycleConfig
    describeNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,
    describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigName,
    describeNotebookInstanceLifecycleConfigResponse_onCreate,
    describeNotebookInstanceLifecycleConfigResponse_lastModifiedTime,
    describeNotebookInstanceLifecycleConfigResponse_creationTime,
    describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn,
    describeNotebookInstanceLifecycleConfigResponse_onStart,
    describeNotebookInstanceLifecycleConfigResponse_httpStatus,

    -- ** DescribePipeline
    describePipeline_pipelineName,
    describePipelineResponse_roleArn,
    describePipelineResponse_pipelineArn,
    describePipelineResponse_pipelineDisplayName,
    describePipelineResponse_pipelineDefinition,
    describePipelineResponse_pipelineDescription,
    describePipelineResponse_lastModifiedTime,
    describePipelineResponse_parallelismConfiguration,
    describePipelineResponse_pipelineName,
    describePipelineResponse_lastRunTime,
    describePipelineResponse_creationTime,
    describePipelineResponse_lastModifiedBy,
    describePipelineResponse_createdBy,
    describePipelineResponse_pipelineStatus,
    describePipelineResponse_httpStatus,

    -- ** DescribePipelineDefinitionForExecution
    describePipelineDefinitionForExecution_pipelineExecutionArn,
    describePipelineDefinitionForExecutionResponse_pipelineDefinition,
    describePipelineDefinitionForExecutionResponse_creationTime,
    describePipelineDefinitionForExecutionResponse_httpStatus,

    -- ** DescribePipelineExecution
    describePipelineExecution_pipelineExecutionArn,
    describePipelineExecutionResponse_pipelineArn,
    describePipelineExecutionResponse_pipelineExperimentConfig,
    describePipelineExecutionResponse_lastModifiedTime,
    describePipelineExecutionResponse_parallelismConfiguration,
    describePipelineExecutionResponse_pipelineExecutionDescription,
    describePipelineExecutionResponse_creationTime,
    describePipelineExecutionResponse_lastModifiedBy,
    describePipelineExecutionResponse_createdBy,
    describePipelineExecutionResponse_pipelineExecutionStatus,
    describePipelineExecutionResponse_pipelineExecutionDisplayName,
    describePipelineExecutionResponse_pipelineExecutionArn,
    describePipelineExecutionResponse_failureReason,
    describePipelineExecutionResponse_httpStatus,

    -- ** DescribeProcessingJob
    describeProcessingJob_processingJobName,
    describeProcessingJobResponse_roleArn,
    describeProcessingJobResponse_environment,
    describeProcessingJobResponse_monitoringScheduleArn,
    describeProcessingJobResponse_networkConfig,
    describeProcessingJobResponse_experimentConfig,
    describeProcessingJobResponse_autoMLJobArn,
    describeProcessingJobResponse_processingInputs,
    describeProcessingJobResponse_lastModifiedTime,
    describeProcessingJobResponse_stoppingCondition,
    describeProcessingJobResponse_processingStartTime,
    describeProcessingJobResponse_processingEndTime,
    describeProcessingJobResponse_trainingJobArn,
    describeProcessingJobResponse_exitMessage,
    describeProcessingJobResponse_failureReason,
    describeProcessingJobResponse_processingOutputConfig,
    describeProcessingJobResponse_httpStatus,
    describeProcessingJobResponse_processingJobName,
    describeProcessingJobResponse_processingResources,
    describeProcessingJobResponse_appSpecification,
    describeProcessingJobResponse_processingJobArn,
    describeProcessingJobResponse_processingJobStatus,
    describeProcessingJobResponse_creationTime,

    -- ** DescribeProject
    describeProject_projectName,
    describeProjectResponse_serviceCatalogProvisionedProductDetails,
    describeProjectResponse_lastModifiedTime,
    describeProjectResponse_projectDescription,
    describeProjectResponse_lastModifiedBy,
    describeProjectResponse_createdBy,
    describeProjectResponse_httpStatus,
    describeProjectResponse_projectArn,
    describeProjectResponse_projectName,
    describeProjectResponse_projectId,
    describeProjectResponse_serviceCatalogProvisioningDetails,
    describeProjectResponse_projectStatus,
    describeProjectResponse_creationTime,

    -- ** DescribeStudioLifecycleConfig
    describeStudioLifecycleConfig_studioLifecycleConfigName,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigName,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigArn,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigAppType,
    describeStudioLifecycleConfigResponse_lastModifiedTime,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigContent,
    describeStudioLifecycleConfigResponse_creationTime,
    describeStudioLifecycleConfigResponse_httpStatus,

    -- ** DescribeSubscribedWorkteam
    describeSubscribedWorkteam_workteamArn,
    describeSubscribedWorkteamResponse_httpStatus,
    describeSubscribedWorkteamResponse_subscribedWorkteam,

    -- ** DescribeTrainingJob
    describeTrainingJob_trainingJobName,
    describeTrainingJobResponse_profilerConfig,
    describeTrainingJobResponse_profilerRuleEvaluationStatuses,
    describeTrainingJobResponse_outputDataConfig,
    describeTrainingJobResponse_enableManagedSpotTraining,
    describeTrainingJobResponse_roleArn,
    describeTrainingJobResponse_environment,
    describeTrainingJobResponse_trainingTimeInSeconds,
    describeTrainingJobResponse_debugRuleEvaluationStatuses,
    describeTrainingJobResponse_warmPoolStatus,
    describeTrainingJobResponse_retryStrategy,
    describeTrainingJobResponse_vpcConfig,
    describeTrainingJobResponse_secondaryStatusTransitions,
    describeTrainingJobResponse_enableNetworkIsolation,
    describeTrainingJobResponse_profilerRuleConfigurations,
    describeTrainingJobResponse_experimentConfig,
    describeTrainingJobResponse_tuningJobArn,
    describeTrainingJobResponse_trainingStartTime,
    describeTrainingJobResponse_checkpointConfig,
    describeTrainingJobResponse_autoMLJobArn,
    describeTrainingJobResponse_debugHookConfig,
    describeTrainingJobResponse_lastModifiedTime,
    describeTrainingJobResponse_enableInterContainerTrafficEncryption,
    describeTrainingJobResponse_finalMetricDataList,
    describeTrainingJobResponse_debugRuleConfigurations,
    describeTrainingJobResponse_labelingJobArn,
    describeTrainingJobResponse_profilingStatus,
    describeTrainingJobResponse_tensorBoardOutputConfig,
    describeTrainingJobResponse_billableTimeInSeconds,
    describeTrainingJobResponse_inputDataConfig,
    describeTrainingJobResponse_hyperParameters,
    describeTrainingJobResponse_failureReason,
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

    -- ** DescribeTransformJob
    describeTransformJob_transformJobName,
    describeTransformJobResponse_maxConcurrentTransforms,
    describeTransformJobResponse_modelClientConfig,
    describeTransformJobResponse_environment,
    describeTransformJobResponse_transformOutput,
    describeTransformJobResponse_experimentConfig,
    describeTransformJobResponse_autoMLJobArn,
    describeTransformJobResponse_transformEndTime,
    describeTransformJobResponse_maxPayloadInMB,
    describeTransformJobResponse_batchStrategy,
    describeTransformJobResponse_dataCaptureConfig,
    describeTransformJobResponse_labelingJobArn,
    describeTransformJobResponse_dataProcessing,
    describeTransformJobResponse_transformStartTime,
    describeTransformJobResponse_failureReason,
    describeTransformJobResponse_httpStatus,
    describeTransformJobResponse_transformJobName,
    describeTransformJobResponse_transformJobArn,
    describeTransformJobResponse_transformJobStatus,
    describeTransformJobResponse_modelName,
    describeTransformJobResponse_transformInput,
    describeTransformJobResponse_transformResources,
    describeTransformJobResponse_creationTime,

    -- ** DescribeTrial
    describeTrial_trialName,
    describeTrialResponse_metadataProperties,
    describeTrialResponse_displayName,
    describeTrialResponse_lastModifiedTime,
    describeTrialResponse_source,
    describeTrialResponse_trialName,
    describeTrialResponse_creationTime,
    describeTrialResponse_lastModifiedBy,
    describeTrialResponse_createdBy,
    describeTrialResponse_trialArn,
    describeTrialResponse_experimentName,
    describeTrialResponse_httpStatus,

    -- ** DescribeTrialComponent
    describeTrialComponent_trialComponentName,
    describeTrialComponentResponse_trialComponentArn,
    describeTrialComponentResponse_trialComponentName,
    describeTrialComponentResponse_metadataProperties,
    describeTrialComponentResponse_displayName,
    describeTrialComponentResponse_status,
    describeTrialComponentResponse_metrics,
    describeTrialComponentResponse_outputArtifacts,
    describeTrialComponentResponse_endTime,
    describeTrialComponentResponse_lastModifiedTime,
    describeTrialComponentResponse_source,
    describeTrialComponentResponse_lineageGroupArn,
    describeTrialComponentResponse_creationTime,
    describeTrialComponentResponse_inputArtifacts,
    describeTrialComponentResponse_lastModifiedBy,
    describeTrialComponentResponse_createdBy,
    describeTrialComponentResponse_startTime,
    describeTrialComponentResponse_parameters,
    describeTrialComponentResponse_httpStatus,

    -- ** DescribeUserProfile
    describeUserProfile_domainId,
    describeUserProfile_userProfileName,
    describeUserProfileResponse_userProfileArn,
    describeUserProfileResponse_singleSignOnUserValue,
    describeUserProfileResponse_status,
    describeUserProfileResponse_lastModifiedTime,
    describeUserProfileResponse_userSettings,
    describeUserProfileResponse_homeEfsFileSystemUid,
    describeUserProfileResponse_userProfileName,
    describeUserProfileResponse_creationTime,
    describeUserProfileResponse_domainId,
    describeUserProfileResponse_failureReason,
    describeUserProfileResponse_singleSignOnUserIdentifier,
    describeUserProfileResponse_httpStatus,

    -- ** DescribeWorkforce
    describeWorkforce_workforceName,
    describeWorkforceResponse_httpStatus,
    describeWorkforceResponse_workforce,

    -- ** DescribeWorkteam
    describeWorkteam_workteamName,
    describeWorkteamResponse_httpStatus,
    describeWorkteamResponse_workteam,

    -- ** DisableSagemakerServicecatalogPortfolio
    disableSagemakerServicecatalogPortfolioResponse_httpStatus,

    -- ** DisassociateTrialComponent
    disassociateTrialComponent_trialComponentName,
    disassociateTrialComponent_trialName,
    disassociateTrialComponentResponse_trialComponentArn,
    disassociateTrialComponentResponse_trialArn,
    disassociateTrialComponentResponse_httpStatus,

    -- ** EnableSagemakerServicecatalogPortfolio
    enableSagemakerServicecatalogPortfolioResponse_httpStatus,

    -- ** GetDeviceFleetReport
    getDeviceFleetReport_deviceFleetName,
    getDeviceFleetReportResponse_modelStats,
    getDeviceFleetReportResponse_description,
    getDeviceFleetReportResponse_agentVersions,
    getDeviceFleetReportResponse_reportGenerated,
    getDeviceFleetReportResponse_deviceStats,
    getDeviceFleetReportResponse_outputConfig,
    getDeviceFleetReportResponse_httpStatus,
    getDeviceFleetReportResponse_deviceFleetArn,
    getDeviceFleetReportResponse_deviceFleetName,

    -- ** GetLineageGroupPolicy
    getLineageGroupPolicy_lineageGroupName,
    getLineageGroupPolicyResponse_resourcePolicy,
    getLineageGroupPolicyResponse_lineageGroupArn,
    getLineageGroupPolicyResponse_httpStatus,

    -- ** GetModelPackageGroupPolicy
    getModelPackageGroupPolicy_modelPackageGroupName,
    getModelPackageGroupPolicyResponse_httpStatus,
    getModelPackageGroupPolicyResponse_resourcePolicy,

    -- ** GetSagemakerServicecatalogPortfolioStatus
    getSagemakerServicecatalogPortfolioStatusResponse_status,
    getSagemakerServicecatalogPortfolioStatusResponse_httpStatus,

    -- ** GetSearchSuggestions
    getSearchSuggestions_suggestionQuery,
    getSearchSuggestions_resource,
    getSearchSuggestionsResponse_propertyNameSuggestions,
    getSearchSuggestionsResponse_httpStatus,

    -- ** ListActions
    listActions_sortOrder,
    listActions_nextToken,
    listActions_sourceUri,
    listActions_actionType,
    listActions_createdBefore,
    listActions_sortBy,
    listActions_maxResults,
    listActions_createdAfter,
    listActionsResponse_nextToken,
    listActionsResponse_actionSummaries,
    listActionsResponse_httpStatus,

    -- ** ListAlgorithms
    listAlgorithms_sortOrder,
    listAlgorithms_nextToken,
    listAlgorithms_nameContains,
    listAlgorithms_creationTimeBefore,
    listAlgorithms_sortBy,
    listAlgorithms_maxResults,
    listAlgorithms_creationTimeAfter,
    listAlgorithmsResponse_nextToken,
    listAlgorithmsResponse_httpStatus,
    listAlgorithmsResponse_algorithmSummaryList,

    -- ** ListAppImageConfigs
    listAppImageConfigs_sortOrder,
    listAppImageConfigs_nextToken,
    listAppImageConfigs_nameContains,
    listAppImageConfigs_creationTimeBefore,
    listAppImageConfigs_sortBy,
    listAppImageConfigs_modifiedTimeBefore,
    listAppImageConfigs_modifiedTimeAfter,
    listAppImageConfigs_maxResults,
    listAppImageConfigs_creationTimeAfter,
    listAppImageConfigsResponse_nextToken,
    listAppImageConfigsResponse_appImageConfigs,
    listAppImageConfigsResponse_httpStatus,

    -- ** ListApps
    listApps_sortOrder,
    listApps_nextToken,
    listApps_userProfileNameEquals,
    listApps_sortBy,
    listApps_maxResults,
    listApps_domainIdEquals,
    listAppsResponse_nextToken,
    listAppsResponse_apps,
    listAppsResponse_httpStatus,

    -- ** ListArtifacts
    listArtifacts_sortOrder,
    listArtifacts_nextToken,
    listArtifacts_sourceUri,
    listArtifacts_artifactType,
    listArtifacts_createdBefore,
    listArtifacts_sortBy,
    listArtifacts_maxResults,
    listArtifacts_createdAfter,
    listArtifactsResponse_nextToken,
    listArtifactsResponse_artifactSummaries,
    listArtifactsResponse_httpStatus,

    -- ** ListAssociations
    listAssociations_sortOrder,
    listAssociations_nextToken,
    listAssociations_associationType,
    listAssociations_sourceArn,
    listAssociations_destinationType,
    listAssociations_createdBefore,
    listAssociations_sourceType,
    listAssociations_sortBy,
    listAssociations_maxResults,
    listAssociations_createdAfter,
    listAssociations_destinationArn,
    listAssociationsResponse_nextToken,
    listAssociationsResponse_associationSummaries,
    listAssociationsResponse_httpStatus,

    -- ** ListAutoMLJobs
    listAutoMLJobs_sortOrder,
    listAutoMLJobs_nextToken,
    listAutoMLJobs_lastModifiedTimeAfter,
    listAutoMLJobs_nameContains,
    listAutoMLJobs_lastModifiedTimeBefore,
    listAutoMLJobs_creationTimeBefore,
    listAutoMLJobs_sortBy,
    listAutoMLJobs_maxResults,
    listAutoMLJobs_statusEquals,
    listAutoMLJobs_creationTimeAfter,
    listAutoMLJobsResponse_nextToken,
    listAutoMLJobsResponse_httpStatus,
    listAutoMLJobsResponse_autoMLJobSummaries,

    -- ** ListCandidatesForAutoMLJob
    listCandidatesForAutoMLJob_sortOrder,
    listCandidatesForAutoMLJob_nextToken,
    listCandidatesForAutoMLJob_candidateNameEquals,
    listCandidatesForAutoMLJob_sortBy,
    listCandidatesForAutoMLJob_maxResults,
    listCandidatesForAutoMLJob_statusEquals,
    listCandidatesForAutoMLJob_autoMLJobName,
    listCandidatesForAutoMLJobResponse_nextToken,
    listCandidatesForAutoMLJobResponse_httpStatus,
    listCandidatesForAutoMLJobResponse_candidates,

    -- ** ListCodeRepositories
    listCodeRepositories_sortOrder,
    listCodeRepositories_nextToken,
    listCodeRepositories_lastModifiedTimeAfter,
    listCodeRepositories_nameContains,
    listCodeRepositories_lastModifiedTimeBefore,
    listCodeRepositories_creationTimeBefore,
    listCodeRepositories_sortBy,
    listCodeRepositories_maxResults,
    listCodeRepositories_creationTimeAfter,
    listCodeRepositoriesResponse_nextToken,
    listCodeRepositoriesResponse_httpStatus,
    listCodeRepositoriesResponse_codeRepositorySummaryList,

    -- ** ListCompilationJobs
    listCompilationJobs_sortOrder,
    listCompilationJobs_nextToken,
    listCompilationJobs_lastModifiedTimeAfter,
    listCompilationJobs_nameContains,
    listCompilationJobs_lastModifiedTimeBefore,
    listCompilationJobs_creationTimeBefore,
    listCompilationJobs_sortBy,
    listCompilationJobs_maxResults,
    listCompilationJobs_statusEquals,
    listCompilationJobs_creationTimeAfter,
    listCompilationJobsResponse_nextToken,
    listCompilationJobsResponse_httpStatus,
    listCompilationJobsResponse_compilationJobSummaries,

    -- ** ListContexts
    listContexts_sortOrder,
    listContexts_nextToken,
    listContexts_sourceUri,
    listContexts_createdBefore,
    listContexts_sortBy,
    listContexts_maxResults,
    listContexts_createdAfter,
    listContexts_contextType,
    listContextsResponse_nextToken,
    listContextsResponse_contextSummaries,
    listContextsResponse_httpStatus,

    -- ** ListDataQualityJobDefinitions
    listDataQualityJobDefinitions_sortOrder,
    listDataQualityJobDefinitions_nextToken,
    listDataQualityJobDefinitions_endpointName,
    listDataQualityJobDefinitions_nameContains,
    listDataQualityJobDefinitions_creationTimeBefore,
    listDataQualityJobDefinitions_sortBy,
    listDataQualityJobDefinitions_maxResults,
    listDataQualityJobDefinitions_creationTimeAfter,
    listDataQualityJobDefinitionsResponse_nextToken,
    listDataQualityJobDefinitionsResponse_httpStatus,
    listDataQualityJobDefinitionsResponse_jobDefinitionSummaries,

    -- ** ListDeviceFleets
    listDeviceFleets_sortOrder,
    listDeviceFleets_nextToken,
    listDeviceFleets_lastModifiedTimeAfter,
    listDeviceFleets_nameContains,
    listDeviceFleets_lastModifiedTimeBefore,
    listDeviceFleets_creationTimeBefore,
    listDeviceFleets_sortBy,
    listDeviceFleets_maxResults,
    listDeviceFleets_creationTimeAfter,
    listDeviceFleetsResponse_nextToken,
    listDeviceFleetsResponse_httpStatus,
    listDeviceFleetsResponse_deviceFleetSummaries,

    -- ** ListDevices
    listDevices_nextToken,
    listDevices_latestHeartbeatAfter,
    listDevices_deviceFleetName,
    listDevices_maxResults,
    listDevices_modelName,
    listDevicesResponse_nextToken,
    listDevicesResponse_httpStatus,
    listDevicesResponse_deviceSummaries,

    -- ** ListDomains
    listDomains_nextToken,
    listDomains_maxResults,
    listDomainsResponse_domains,
    listDomainsResponse_nextToken,
    listDomainsResponse_httpStatus,

    -- ** ListEdgeDeploymentPlans
    listEdgeDeploymentPlans_sortOrder,
    listEdgeDeploymentPlans_nextToken,
    listEdgeDeploymentPlans_lastModifiedTimeAfter,
    listEdgeDeploymentPlans_nameContains,
    listEdgeDeploymentPlans_lastModifiedTimeBefore,
    listEdgeDeploymentPlans_creationTimeBefore,
    listEdgeDeploymentPlans_sortBy,
    listEdgeDeploymentPlans_maxResults,
    listEdgeDeploymentPlans_deviceFleetNameContains,
    listEdgeDeploymentPlans_creationTimeAfter,
    listEdgeDeploymentPlansResponse_nextToken,
    listEdgeDeploymentPlansResponse_httpStatus,
    listEdgeDeploymentPlansResponse_edgeDeploymentPlanSummaries,

    -- ** ListEdgePackagingJobs
    listEdgePackagingJobs_sortOrder,
    listEdgePackagingJobs_nextToken,
    listEdgePackagingJobs_lastModifiedTimeAfter,
    listEdgePackagingJobs_nameContains,
    listEdgePackagingJobs_modelNameContains,
    listEdgePackagingJobs_lastModifiedTimeBefore,
    listEdgePackagingJobs_creationTimeBefore,
    listEdgePackagingJobs_sortBy,
    listEdgePackagingJobs_maxResults,
    listEdgePackagingJobs_statusEquals,
    listEdgePackagingJobs_creationTimeAfter,
    listEdgePackagingJobsResponse_nextToken,
    listEdgePackagingJobsResponse_httpStatus,
    listEdgePackagingJobsResponse_edgePackagingJobSummaries,

    -- ** ListEndpointConfigs
    listEndpointConfigs_sortOrder,
    listEndpointConfigs_nextToken,
    listEndpointConfigs_nameContains,
    listEndpointConfigs_creationTimeBefore,
    listEndpointConfigs_sortBy,
    listEndpointConfigs_maxResults,
    listEndpointConfigs_creationTimeAfter,
    listEndpointConfigsResponse_nextToken,
    listEndpointConfigsResponse_httpStatus,
    listEndpointConfigsResponse_endpointConfigs,

    -- ** ListEndpoints
    listEndpoints_sortOrder,
    listEndpoints_nextToken,
    listEndpoints_lastModifiedTimeAfter,
    listEndpoints_nameContains,
    listEndpoints_lastModifiedTimeBefore,
    listEndpoints_creationTimeBefore,
    listEndpoints_sortBy,
    listEndpoints_maxResults,
    listEndpoints_statusEquals,
    listEndpoints_creationTimeAfter,
    listEndpointsResponse_nextToken,
    listEndpointsResponse_httpStatus,
    listEndpointsResponse_endpoints,

    -- ** ListExperiments
    listExperiments_sortOrder,
    listExperiments_nextToken,
    listExperiments_createdBefore,
    listExperiments_sortBy,
    listExperiments_maxResults,
    listExperiments_createdAfter,
    listExperimentsResponse_nextToken,
    listExperimentsResponse_experimentSummaries,
    listExperimentsResponse_httpStatus,

    -- ** ListFeatureGroups
    listFeatureGroups_sortOrder,
    listFeatureGroups_nextToken,
    listFeatureGroups_nameContains,
    listFeatureGroups_offlineStoreStatusEquals,
    listFeatureGroups_creationTimeBefore,
    listFeatureGroups_sortBy,
    listFeatureGroups_maxResults,
    listFeatureGroups_creationTimeAfter,
    listFeatureGroups_featureGroupStatusEquals,
    listFeatureGroupsResponse_nextToken,
    listFeatureGroupsResponse_httpStatus,
    listFeatureGroupsResponse_featureGroupSummaries,

    -- ** ListFlowDefinitions
    listFlowDefinitions_sortOrder,
    listFlowDefinitions_nextToken,
    listFlowDefinitions_creationTimeBefore,
    listFlowDefinitions_maxResults,
    listFlowDefinitions_creationTimeAfter,
    listFlowDefinitionsResponse_nextToken,
    listFlowDefinitionsResponse_httpStatus,
    listFlowDefinitionsResponse_flowDefinitionSummaries,

    -- ** ListHumanTaskUis
    listHumanTaskUis_sortOrder,
    listHumanTaskUis_nextToken,
    listHumanTaskUis_creationTimeBefore,
    listHumanTaskUis_maxResults,
    listHumanTaskUis_creationTimeAfter,
    listHumanTaskUisResponse_nextToken,
    listHumanTaskUisResponse_httpStatus,
    listHumanTaskUisResponse_humanTaskUiSummaries,

    -- ** ListHyperParameterTuningJobs
    listHyperParameterTuningJobs_sortOrder,
    listHyperParameterTuningJobs_nextToken,
    listHyperParameterTuningJobs_lastModifiedTimeAfter,
    listHyperParameterTuningJobs_nameContains,
    listHyperParameterTuningJobs_lastModifiedTimeBefore,
    listHyperParameterTuningJobs_creationTimeBefore,
    listHyperParameterTuningJobs_sortBy,
    listHyperParameterTuningJobs_maxResults,
    listHyperParameterTuningJobs_statusEquals,
    listHyperParameterTuningJobs_creationTimeAfter,
    listHyperParameterTuningJobsResponse_nextToken,
    listHyperParameterTuningJobsResponse_httpStatus,
    listHyperParameterTuningJobsResponse_hyperParameterTuningJobSummaries,

    -- ** ListImageVersions
    listImageVersions_sortOrder,
    listImageVersions_nextToken,
    listImageVersions_lastModifiedTimeAfter,
    listImageVersions_lastModifiedTimeBefore,
    listImageVersions_creationTimeBefore,
    listImageVersions_sortBy,
    listImageVersions_maxResults,
    listImageVersions_creationTimeAfter,
    listImageVersions_imageName,
    listImageVersionsResponse_imageVersions,
    listImageVersionsResponse_nextToken,
    listImageVersionsResponse_httpStatus,

    -- ** ListImages
    listImages_sortOrder,
    listImages_nextToken,
    listImages_lastModifiedTimeAfter,
    listImages_nameContains,
    listImages_lastModifiedTimeBefore,
    listImages_creationTimeBefore,
    listImages_sortBy,
    listImages_maxResults,
    listImages_creationTimeAfter,
    listImagesResponse_nextToken,
    listImagesResponse_images,
    listImagesResponse_httpStatus,

    -- ** ListInferenceRecommendationsJobSteps
    listInferenceRecommendationsJobSteps_nextToken,
    listInferenceRecommendationsJobSteps_status,
    listInferenceRecommendationsJobSteps_maxResults,
    listInferenceRecommendationsJobSteps_stepType,
    listInferenceRecommendationsJobSteps_jobName,
    listInferenceRecommendationsJobStepsResponse_nextToken,
    listInferenceRecommendationsJobStepsResponse_steps,
    listInferenceRecommendationsJobStepsResponse_httpStatus,

    -- ** ListInferenceRecommendationsJobs
    listInferenceRecommendationsJobs_sortOrder,
    listInferenceRecommendationsJobs_nextToken,
    listInferenceRecommendationsJobs_lastModifiedTimeAfter,
    listInferenceRecommendationsJobs_nameContains,
    listInferenceRecommendationsJobs_lastModifiedTimeBefore,
    listInferenceRecommendationsJobs_creationTimeBefore,
    listInferenceRecommendationsJobs_sortBy,
    listInferenceRecommendationsJobs_maxResults,
    listInferenceRecommendationsJobs_statusEquals,
    listInferenceRecommendationsJobs_creationTimeAfter,
    listInferenceRecommendationsJobsResponse_nextToken,
    listInferenceRecommendationsJobsResponse_httpStatus,
    listInferenceRecommendationsJobsResponse_inferenceRecommendationsJobs,

    -- ** ListLabelingJobs
    listLabelingJobs_sortOrder,
    listLabelingJobs_nextToken,
    listLabelingJobs_lastModifiedTimeAfter,
    listLabelingJobs_nameContains,
    listLabelingJobs_lastModifiedTimeBefore,
    listLabelingJobs_creationTimeBefore,
    listLabelingJobs_sortBy,
    listLabelingJobs_maxResults,
    listLabelingJobs_statusEquals,
    listLabelingJobs_creationTimeAfter,
    listLabelingJobsResponse_nextToken,
    listLabelingJobsResponse_labelingJobSummaryList,
    listLabelingJobsResponse_httpStatus,

    -- ** ListLabelingJobsForWorkteam
    listLabelingJobsForWorkteam_sortOrder,
    listLabelingJobsForWorkteam_nextToken,
    listLabelingJobsForWorkteam_creationTimeBefore,
    listLabelingJobsForWorkteam_sortBy,
    listLabelingJobsForWorkteam_maxResults,
    listLabelingJobsForWorkteam_jobReferenceCodeContains,
    listLabelingJobsForWorkteam_creationTimeAfter,
    listLabelingJobsForWorkteam_workteamArn,
    listLabelingJobsForWorkteamResponse_nextToken,
    listLabelingJobsForWorkteamResponse_httpStatus,
    listLabelingJobsForWorkteamResponse_labelingJobSummaryList,

    -- ** ListLineageGroups
    listLineageGroups_sortOrder,
    listLineageGroups_nextToken,
    listLineageGroups_createdBefore,
    listLineageGroups_sortBy,
    listLineageGroups_maxResults,
    listLineageGroups_createdAfter,
    listLineageGroupsResponse_nextToken,
    listLineageGroupsResponse_lineageGroupSummaries,
    listLineageGroupsResponse_httpStatus,

    -- ** ListModelBiasJobDefinitions
    listModelBiasJobDefinitions_sortOrder,
    listModelBiasJobDefinitions_nextToken,
    listModelBiasJobDefinitions_endpointName,
    listModelBiasJobDefinitions_nameContains,
    listModelBiasJobDefinitions_creationTimeBefore,
    listModelBiasJobDefinitions_sortBy,
    listModelBiasJobDefinitions_maxResults,
    listModelBiasJobDefinitions_creationTimeAfter,
    listModelBiasJobDefinitionsResponse_nextToken,
    listModelBiasJobDefinitionsResponse_httpStatus,
    listModelBiasJobDefinitionsResponse_jobDefinitionSummaries,

    -- ** ListModelExplainabilityJobDefinitions
    listModelExplainabilityJobDefinitions_sortOrder,
    listModelExplainabilityJobDefinitions_nextToken,
    listModelExplainabilityJobDefinitions_endpointName,
    listModelExplainabilityJobDefinitions_nameContains,
    listModelExplainabilityJobDefinitions_creationTimeBefore,
    listModelExplainabilityJobDefinitions_sortBy,
    listModelExplainabilityJobDefinitions_maxResults,
    listModelExplainabilityJobDefinitions_creationTimeAfter,
    listModelExplainabilityJobDefinitionsResponse_nextToken,
    listModelExplainabilityJobDefinitionsResponse_httpStatus,
    listModelExplainabilityJobDefinitionsResponse_jobDefinitionSummaries,

    -- ** ListModelMetadata
    listModelMetadata_nextToken,
    listModelMetadata_maxResults,
    listModelMetadata_searchExpression,
    listModelMetadataResponse_nextToken,
    listModelMetadataResponse_httpStatus,
    listModelMetadataResponse_modelMetadataSummaries,

    -- ** ListModelPackageGroups
    listModelPackageGroups_sortOrder,
    listModelPackageGroups_nextToken,
    listModelPackageGroups_nameContains,
    listModelPackageGroups_creationTimeBefore,
    listModelPackageGroups_sortBy,
    listModelPackageGroups_maxResults,
    listModelPackageGroups_creationTimeAfter,
    listModelPackageGroupsResponse_nextToken,
    listModelPackageGroupsResponse_httpStatus,
    listModelPackageGroupsResponse_modelPackageGroupSummaryList,

    -- ** ListModelPackages
    listModelPackages_modelPackageGroupName,
    listModelPackages_sortOrder,
    listModelPackages_nextToken,
    listModelPackages_nameContains,
    listModelPackages_modelApprovalStatus,
    listModelPackages_creationTimeBefore,
    listModelPackages_sortBy,
    listModelPackages_maxResults,
    listModelPackages_modelPackageType,
    listModelPackages_creationTimeAfter,
    listModelPackagesResponse_nextToken,
    listModelPackagesResponse_httpStatus,
    listModelPackagesResponse_modelPackageSummaryList,

    -- ** ListModelQualityJobDefinitions
    listModelQualityJobDefinitions_sortOrder,
    listModelQualityJobDefinitions_nextToken,
    listModelQualityJobDefinitions_endpointName,
    listModelQualityJobDefinitions_nameContains,
    listModelQualityJobDefinitions_creationTimeBefore,
    listModelQualityJobDefinitions_sortBy,
    listModelQualityJobDefinitions_maxResults,
    listModelQualityJobDefinitions_creationTimeAfter,
    listModelQualityJobDefinitionsResponse_nextToken,
    listModelQualityJobDefinitionsResponse_httpStatus,
    listModelQualityJobDefinitionsResponse_jobDefinitionSummaries,

    -- ** ListModels
    listModels_sortOrder,
    listModels_nextToken,
    listModels_nameContains,
    listModels_creationTimeBefore,
    listModels_sortBy,
    listModels_maxResults,
    listModels_creationTimeAfter,
    listModelsResponse_nextToken,
    listModelsResponse_httpStatus,
    listModelsResponse_models,

    -- ** ListMonitoringExecutions
    listMonitoringExecutions_sortOrder,
    listMonitoringExecutions_nextToken,
    listMonitoringExecutions_lastModifiedTimeAfter,
    listMonitoringExecutions_endpointName,
    listMonitoringExecutions_lastModifiedTimeBefore,
    listMonitoringExecutions_scheduledTimeAfter,
    listMonitoringExecutions_creationTimeBefore,
    listMonitoringExecutions_monitoringTypeEquals,
    listMonitoringExecutions_sortBy,
    listMonitoringExecutions_monitoringScheduleName,
    listMonitoringExecutions_maxResults,
    listMonitoringExecutions_scheduledTimeBefore,
    listMonitoringExecutions_statusEquals,
    listMonitoringExecutions_creationTimeAfter,
    listMonitoringExecutions_monitoringJobDefinitionName,
    listMonitoringExecutionsResponse_nextToken,
    listMonitoringExecutionsResponse_httpStatus,
    listMonitoringExecutionsResponse_monitoringExecutionSummaries,

    -- ** ListMonitoringSchedules
    listMonitoringSchedules_sortOrder,
    listMonitoringSchedules_nextToken,
    listMonitoringSchedules_lastModifiedTimeAfter,
    listMonitoringSchedules_endpointName,
    listMonitoringSchedules_nameContains,
    listMonitoringSchedules_lastModifiedTimeBefore,
    listMonitoringSchedules_creationTimeBefore,
    listMonitoringSchedules_monitoringTypeEquals,
    listMonitoringSchedules_sortBy,
    listMonitoringSchedules_maxResults,
    listMonitoringSchedules_statusEquals,
    listMonitoringSchedules_creationTimeAfter,
    listMonitoringSchedules_monitoringJobDefinitionName,
    listMonitoringSchedulesResponse_nextToken,
    listMonitoringSchedulesResponse_httpStatus,
    listMonitoringSchedulesResponse_monitoringScheduleSummaries,

    -- ** ListNotebookInstanceLifecycleConfigs
    listNotebookInstanceLifecycleConfigs_sortOrder,
    listNotebookInstanceLifecycleConfigs_nextToken,
    listNotebookInstanceLifecycleConfigs_lastModifiedTimeAfter,
    listNotebookInstanceLifecycleConfigs_nameContains,
    listNotebookInstanceLifecycleConfigs_lastModifiedTimeBefore,
    listNotebookInstanceLifecycleConfigs_creationTimeBefore,
    listNotebookInstanceLifecycleConfigs_sortBy,
    listNotebookInstanceLifecycleConfigs_maxResults,
    listNotebookInstanceLifecycleConfigs_creationTimeAfter,
    listNotebookInstanceLifecycleConfigsResponse_nextToken,
    listNotebookInstanceLifecycleConfigsResponse_notebookInstanceLifecycleConfigs,
    listNotebookInstanceLifecycleConfigsResponse_httpStatus,

    -- ** ListNotebookInstances
    listNotebookInstances_sortOrder,
    listNotebookInstances_nextToken,
    listNotebookInstances_defaultCodeRepositoryContains,
    listNotebookInstances_lastModifiedTimeAfter,
    listNotebookInstances_nameContains,
    listNotebookInstances_lastModifiedTimeBefore,
    listNotebookInstances_creationTimeBefore,
    listNotebookInstances_sortBy,
    listNotebookInstances_maxResults,
    listNotebookInstances_notebookInstanceLifecycleConfigNameContains,
    listNotebookInstances_statusEquals,
    listNotebookInstances_creationTimeAfter,
    listNotebookInstances_additionalCodeRepositoryEquals,
    listNotebookInstancesResponse_nextToken,
    listNotebookInstancesResponse_notebookInstances,
    listNotebookInstancesResponse_httpStatus,

    -- ** ListPipelineExecutionSteps
    listPipelineExecutionSteps_sortOrder,
    listPipelineExecutionSteps_nextToken,
    listPipelineExecutionSteps_maxResults,
    listPipelineExecutionSteps_pipelineExecutionArn,
    listPipelineExecutionStepsResponse_nextToken,
    listPipelineExecutionStepsResponse_pipelineExecutionSteps,
    listPipelineExecutionStepsResponse_httpStatus,

    -- ** ListPipelineExecutions
    listPipelineExecutions_sortOrder,
    listPipelineExecutions_nextToken,
    listPipelineExecutions_createdBefore,
    listPipelineExecutions_sortBy,
    listPipelineExecutions_maxResults,
    listPipelineExecutions_createdAfter,
    listPipelineExecutions_pipelineName,
    listPipelineExecutionsResponse_nextToken,
    listPipelineExecutionsResponse_pipelineExecutionSummaries,
    listPipelineExecutionsResponse_httpStatus,

    -- ** ListPipelineParametersForExecution
    listPipelineParametersForExecution_nextToken,
    listPipelineParametersForExecution_maxResults,
    listPipelineParametersForExecution_pipelineExecutionArn,
    listPipelineParametersForExecutionResponse_nextToken,
    listPipelineParametersForExecutionResponse_pipelineParameters,
    listPipelineParametersForExecutionResponse_httpStatus,

    -- ** ListPipelines
    listPipelines_sortOrder,
    listPipelines_nextToken,
    listPipelines_createdBefore,
    listPipelines_sortBy,
    listPipelines_maxResults,
    listPipelines_pipelineNamePrefix,
    listPipelines_createdAfter,
    listPipelinesResponse_nextToken,
    listPipelinesResponse_pipelineSummaries,
    listPipelinesResponse_httpStatus,

    -- ** ListProcessingJobs
    listProcessingJobs_sortOrder,
    listProcessingJobs_nextToken,
    listProcessingJobs_lastModifiedTimeAfter,
    listProcessingJobs_nameContains,
    listProcessingJobs_lastModifiedTimeBefore,
    listProcessingJobs_creationTimeBefore,
    listProcessingJobs_sortBy,
    listProcessingJobs_maxResults,
    listProcessingJobs_statusEquals,
    listProcessingJobs_creationTimeAfter,
    listProcessingJobsResponse_nextToken,
    listProcessingJobsResponse_httpStatus,
    listProcessingJobsResponse_processingJobSummaries,

    -- ** ListProjects
    listProjects_sortOrder,
    listProjects_nextToken,
    listProjects_nameContains,
    listProjects_creationTimeBefore,
    listProjects_sortBy,
    listProjects_maxResults,
    listProjects_creationTimeAfter,
    listProjectsResponse_nextToken,
    listProjectsResponse_httpStatus,
    listProjectsResponse_projectSummaryList,

    -- ** ListStageDevices
    listStageDevices_nextToken,
    listStageDevices_excludeDevicesDeployedInOtherStage,
    listStageDevices_maxResults,
    listStageDevices_edgeDeploymentPlanName,
    listStageDevices_stageName,
    listStageDevicesResponse_nextToken,
    listStageDevicesResponse_httpStatus,
    listStageDevicesResponse_deviceDeploymentSummaries,

    -- ** ListStudioLifecycleConfigs
    listStudioLifecycleConfigs_appTypeEquals,
    listStudioLifecycleConfigs_sortOrder,
    listStudioLifecycleConfigs_nextToken,
    listStudioLifecycleConfigs_nameContains,
    listStudioLifecycleConfigs_creationTimeBefore,
    listStudioLifecycleConfigs_sortBy,
    listStudioLifecycleConfigs_modifiedTimeBefore,
    listStudioLifecycleConfigs_modifiedTimeAfter,
    listStudioLifecycleConfigs_maxResults,
    listStudioLifecycleConfigs_creationTimeAfter,
    listStudioLifecycleConfigsResponse_nextToken,
    listStudioLifecycleConfigsResponse_studioLifecycleConfigs,
    listStudioLifecycleConfigsResponse_httpStatus,

    -- ** ListSubscribedWorkteams
    listSubscribedWorkteams_nextToken,
    listSubscribedWorkteams_nameContains,
    listSubscribedWorkteams_maxResults,
    listSubscribedWorkteamsResponse_nextToken,
    listSubscribedWorkteamsResponse_httpStatus,
    listSubscribedWorkteamsResponse_subscribedWorkteams,

    -- ** ListTags
    listTags_nextToken,
    listTags_maxResults,
    listTags_resourceArn,
    listTagsResponse_tags,
    listTagsResponse_nextToken,
    listTagsResponse_httpStatus,

    -- ** ListTrainingJobs
    listTrainingJobs_sortOrder,
    listTrainingJobs_nextToken,
    listTrainingJobs_lastModifiedTimeAfter,
    listTrainingJobs_nameContains,
    listTrainingJobs_lastModifiedTimeBefore,
    listTrainingJobs_creationTimeBefore,
    listTrainingJobs_sortBy,
    listTrainingJobs_maxResults,
    listTrainingJobs_statusEquals,
    listTrainingJobs_warmPoolStatusEquals,
    listTrainingJobs_creationTimeAfter,
    listTrainingJobsResponse_nextToken,
    listTrainingJobsResponse_httpStatus,
    listTrainingJobsResponse_trainingJobSummaries,

    -- ** ListTrainingJobsForHyperParameterTuningJob
    listTrainingJobsForHyperParameterTuningJob_sortOrder,
    listTrainingJobsForHyperParameterTuningJob_nextToken,
    listTrainingJobsForHyperParameterTuningJob_sortBy,
    listTrainingJobsForHyperParameterTuningJob_maxResults,
    listTrainingJobsForHyperParameterTuningJob_statusEquals,
    listTrainingJobsForHyperParameterTuningJob_hyperParameterTuningJobName,
    listTrainingJobsForHyperParameterTuningJobResponse_nextToken,
    listTrainingJobsForHyperParameterTuningJobResponse_httpStatus,
    listTrainingJobsForHyperParameterTuningJobResponse_trainingJobSummaries,

    -- ** ListTransformJobs
    listTransformJobs_sortOrder,
    listTransformJobs_nextToken,
    listTransformJobs_lastModifiedTimeAfter,
    listTransformJobs_nameContains,
    listTransformJobs_lastModifiedTimeBefore,
    listTransformJobs_creationTimeBefore,
    listTransformJobs_sortBy,
    listTransformJobs_maxResults,
    listTransformJobs_statusEquals,
    listTransformJobs_creationTimeAfter,
    listTransformJobsResponse_nextToken,
    listTransformJobsResponse_httpStatus,
    listTransformJobsResponse_transformJobSummaries,

    -- ** ListTrialComponents
    listTrialComponents_sortOrder,
    listTrialComponents_nextToken,
    listTrialComponents_sourceArn,
    listTrialComponents_createdBefore,
    listTrialComponents_sortBy,
    listTrialComponents_maxResults,
    listTrialComponents_trialName,
    listTrialComponents_createdAfter,
    listTrialComponents_experimentName,
    listTrialComponentsResponse_nextToken,
    listTrialComponentsResponse_trialComponentSummaries,
    listTrialComponentsResponse_httpStatus,

    -- ** ListTrials
    listTrials_sortOrder,
    listTrials_nextToken,
    listTrials_trialComponentName,
    listTrials_createdBefore,
    listTrials_sortBy,
    listTrials_maxResults,
    listTrials_createdAfter,
    listTrials_experimentName,
    listTrialsResponse_nextToken,
    listTrialsResponse_trialSummaries,
    listTrialsResponse_httpStatus,

    -- ** ListUserProfiles
    listUserProfiles_sortOrder,
    listUserProfiles_nextToken,
    listUserProfiles_sortBy,
    listUserProfiles_userProfileNameContains,
    listUserProfiles_maxResults,
    listUserProfiles_domainIdEquals,
    listUserProfilesResponse_userProfiles,
    listUserProfilesResponse_nextToken,
    listUserProfilesResponse_httpStatus,

    -- ** ListWorkforces
    listWorkforces_sortOrder,
    listWorkforces_nextToken,
    listWorkforces_nameContains,
    listWorkforces_sortBy,
    listWorkforces_maxResults,
    listWorkforcesResponse_nextToken,
    listWorkforcesResponse_httpStatus,
    listWorkforcesResponse_workforces,

    -- ** ListWorkteams
    listWorkteams_sortOrder,
    listWorkteams_nextToken,
    listWorkteams_nameContains,
    listWorkteams_sortBy,
    listWorkteams_maxResults,
    listWorkteamsResponse_nextToken,
    listWorkteamsResponse_httpStatus,
    listWorkteamsResponse_workteams,

    -- ** PutModelPackageGroupPolicy
    putModelPackageGroupPolicy_modelPackageGroupName,
    putModelPackageGroupPolicy_resourcePolicy,
    putModelPackageGroupPolicyResponse_httpStatus,
    putModelPackageGroupPolicyResponse_modelPackageGroupArn,

    -- ** QueryLineage
    queryLineage_nextToken,
    queryLineage_filters,
    queryLineage_maxDepth,
    queryLineage_maxResults,
    queryLineage_startArns,
    queryLineage_direction,
    queryLineage_includeEdges,
    queryLineageResponse_edges,
    queryLineageResponse_nextToken,
    queryLineageResponse_vertices,
    queryLineageResponse_httpStatus,

    -- ** RegisterDevices
    registerDevices_tags,
    registerDevices_deviceFleetName,
    registerDevices_devices,

    -- ** RenderUiTemplate
    renderUiTemplate_uiTemplate,
    renderUiTemplate_humanTaskUiArn,
    renderUiTemplate_task,
    renderUiTemplate_roleArn,
    renderUiTemplateResponse_httpStatus,
    renderUiTemplateResponse_renderedContent,
    renderUiTemplateResponse_errors,

    -- ** RetryPipelineExecution
    retryPipelineExecution_parallelismConfiguration,
    retryPipelineExecution_pipelineExecutionArn,
    retryPipelineExecution_clientRequestToken,
    retryPipelineExecutionResponse_pipelineExecutionArn,
    retryPipelineExecutionResponse_httpStatus,

    -- ** Search
    search_sortOrder,
    search_nextToken,
    search_sortBy,
    search_maxResults,
    search_searchExpression,
    search_resource,
    searchResponse_nextToken,
    searchResponse_results,
    searchResponse_httpStatus,

    -- ** SendPipelineExecutionStepFailure
    sendPipelineExecutionStepFailure_clientRequestToken,
    sendPipelineExecutionStepFailure_failureReason,
    sendPipelineExecutionStepFailure_callbackToken,
    sendPipelineExecutionStepFailureResponse_pipelineExecutionArn,
    sendPipelineExecutionStepFailureResponse_httpStatus,

    -- ** SendPipelineExecutionStepSuccess
    sendPipelineExecutionStepSuccess_clientRequestToken,
    sendPipelineExecutionStepSuccess_outputParameters,
    sendPipelineExecutionStepSuccess_callbackToken,
    sendPipelineExecutionStepSuccessResponse_pipelineExecutionArn,
    sendPipelineExecutionStepSuccessResponse_httpStatus,

    -- ** StartEdgeDeploymentStage
    startEdgeDeploymentStage_edgeDeploymentPlanName,
    startEdgeDeploymentStage_stageName,

    -- ** StartMonitoringSchedule
    startMonitoringSchedule_monitoringScheduleName,

    -- ** StartNotebookInstance
    startNotebookInstance_notebookInstanceName,

    -- ** StartPipelineExecution
    startPipelineExecution_pipelineParameters,
    startPipelineExecution_parallelismConfiguration,
    startPipelineExecution_pipelineExecutionDescription,
    startPipelineExecution_pipelineExecutionDisplayName,
    startPipelineExecution_pipelineName,
    startPipelineExecution_clientRequestToken,
    startPipelineExecutionResponse_pipelineExecutionArn,
    startPipelineExecutionResponse_httpStatus,

    -- ** StopAutoMLJob
    stopAutoMLJob_autoMLJobName,

    -- ** StopCompilationJob
    stopCompilationJob_compilationJobName,

    -- ** StopEdgeDeploymentStage
    stopEdgeDeploymentStage_edgeDeploymentPlanName,
    stopEdgeDeploymentStage_stageName,

    -- ** StopEdgePackagingJob
    stopEdgePackagingJob_edgePackagingJobName,

    -- ** StopHyperParameterTuningJob
    stopHyperParameterTuningJob_hyperParameterTuningJobName,

    -- ** StopInferenceRecommendationsJob
    stopInferenceRecommendationsJob_jobName,

    -- ** StopLabelingJob
    stopLabelingJob_labelingJobName,

    -- ** StopMonitoringSchedule
    stopMonitoringSchedule_monitoringScheduleName,

    -- ** StopNotebookInstance
    stopNotebookInstance_notebookInstanceName,

    -- ** StopPipelineExecution
    stopPipelineExecution_pipelineExecutionArn,
    stopPipelineExecution_clientRequestToken,
    stopPipelineExecutionResponse_pipelineExecutionArn,
    stopPipelineExecutionResponse_httpStatus,

    -- ** StopProcessingJob
    stopProcessingJob_processingJobName,

    -- ** StopTrainingJob
    stopTrainingJob_trainingJobName,

    -- ** StopTransformJob
    stopTransformJob_transformJobName,

    -- ** UpdateAction
    updateAction_propertiesToRemove,
    updateAction_properties,
    updateAction_status,
    updateAction_description,
    updateAction_actionName,
    updateActionResponse_actionArn,
    updateActionResponse_httpStatus,

    -- ** UpdateAppImageConfig
    updateAppImageConfig_kernelGatewayImageConfig,
    updateAppImageConfig_appImageConfigName,
    updateAppImageConfigResponse_appImageConfigArn,
    updateAppImageConfigResponse_httpStatus,

    -- ** UpdateArtifact
    updateArtifact_artifactName,
    updateArtifact_propertiesToRemove,
    updateArtifact_properties,
    updateArtifact_artifactArn,
    updateArtifactResponse_artifactArn,
    updateArtifactResponse_httpStatus,

    -- ** UpdateCodeRepository
    updateCodeRepository_gitConfig,
    updateCodeRepository_codeRepositoryName,
    updateCodeRepositoryResponse_httpStatus,
    updateCodeRepositoryResponse_codeRepositoryArn,

    -- ** UpdateContext
    updateContext_propertiesToRemove,
    updateContext_properties,
    updateContext_description,
    updateContext_contextName,
    updateContextResponse_contextArn,
    updateContextResponse_httpStatus,

    -- ** UpdateDeviceFleet
    updateDeviceFleet_roleArn,
    updateDeviceFleet_description,
    updateDeviceFleet_enableIotRoleAlias,
    updateDeviceFleet_deviceFleetName,
    updateDeviceFleet_outputConfig,

    -- ** UpdateDevices
    updateDevices_deviceFleetName,
    updateDevices_devices,

    -- ** UpdateDomain
    updateDomain_domainSettingsForUpdate,
    updateDomain_defaultUserSettings,
    updateDomain_domainId,
    updateDomainResponse_domainArn,
    updateDomainResponse_httpStatus,

    -- ** UpdateEndpoint
    updateEndpoint_retainDeploymentConfig,
    updateEndpoint_retainAllVariantProperties,
    updateEndpoint_deploymentConfig,
    updateEndpoint_excludeRetainedVariantProperties,
    updateEndpoint_endpointName,
    updateEndpoint_endpointConfigName,
    updateEndpointResponse_httpStatus,
    updateEndpointResponse_endpointArn,

    -- ** UpdateEndpointWeightsAndCapacities
    updateEndpointWeightsAndCapacities_endpointName,
    updateEndpointWeightsAndCapacities_desiredWeightsAndCapacities,
    updateEndpointWeightsAndCapacitiesResponse_httpStatus,
    updateEndpointWeightsAndCapacitiesResponse_endpointArn,

    -- ** UpdateExperiment
    updateExperiment_displayName,
    updateExperiment_description,
    updateExperiment_experimentName,
    updateExperimentResponse_experimentArn,
    updateExperimentResponse_httpStatus,

    -- ** UpdateFeatureGroup
    updateFeatureGroup_featureAdditions,
    updateFeatureGroup_featureGroupName,
    updateFeatureGroupResponse_httpStatus,
    updateFeatureGroupResponse_featureGroupArn,

    -- ** UpdateFeatureMetadata
    updateFeatureMetadata_parameterRemovals,
    updateFeatureMetadata_description,
    updateFeatureMetadata_parameterAdditions,
    updateFeatureMetadata_featureGroupName,
    updateFeatureMetadata_featureName,

    -- ** UpdateImage
    updateImage_roleArn,
    updateImage_displayName,
    updateImage_description,
    updateImage_deleteProperties,
    updateImage_imageName,
    updateImageResponse_imageArn,
    updateImageResponse_httpStatus,

    -- ** UpdateModelPackage
    updateModelPackage_customerMetadataPropertiesToRemove,
    updateModelPackage_modelApprovalStatus,
    updateModelPackage_approvalDescription,
    updateModelPackage_customerMetadataProperties,
    updateModelPackage_additionalInferenceSpecificationsToAdd,
    updateModelPackage_modelPackageArn,
    updateModelPackageResponse_httpStatus,
    updateModelPackageResponse_modelPackageArn,

    -- ** UpdateMonitoringSchedule
    updateMonitoringSchedule_monitoringScheduleName,
    updateMonitoringSchedule_monitoringScheduleConfig,
    updateMonitoringScheduleResponse_httpStatus,
    updateMonitoringScheduleResponse_monitoringScheduleArn,

    -- ** UpdateNotebookInstance
    updateNotebookInstance_roleArn,
    updateNotebookInstance_disassociateLifecycleConfig,
    updateNotebookInstance_instanceMetadataServiceConfiguration,
    updateNotebookInstance_acceleratorTypes,
    updateNotebookInstance_disassociateAdditionalCodeRepositories,
    updateNotebookInstance_disassociateDefaultCodeRepository,
    updateNotebookInstance_instanceType,
    updateNotebookInstance_disassociateAcceleratorTypes,
    updateNotebookInstance_additionalCodeRepositories,
    updateNotebookInstance_volumeSizeInGB,
    updateNotebookInstance_lifecycleConfigName,
    updateNotebookInstance_defaultCodeRepository,
    updateNotebookInstance_rootAccess,
    updateNotebookInstance_notebookInstanceName,
    updateNotebookInstanceResponse_httpStatus,

    -- ** UpdateNotebookInstanceLifecycleConfig
    updateNotebookInstanceLifecycleConfig_onCreate,
    updateNotebookInstanceLifecycleConfig_onStart,
    updateNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,
    updateNotebookInstanceLifecycleConfigResponse_httpStatus,

    -- ** UpdatePipeline
    updatePipeline_pipelineDefinitionS3Location,
    updatePipeline_roleArn,
    updatePipeline_pipelineDisplayName,
    updatePipeline_pipelineDefinition,
    updatePipeline_pipelineDescription,
    updatePipeline_parallelismConfiguration,
    updatePipeline_pipelineName,
    updatePipelineResponse_pipelineArn,
    updatePipelineResponse_httpStatus,

    -- ** UpdatePipelineExecution
    updatePipelineExecution_parallelismConfiguration,
    updatePipelineExecution_pipelineExecutionDescription,
    updatePipelineExecution_pipelineExecutionDisplayName,
    updatePipelineExecution_pipelineExecutionArn,
    updatePipelineExecutionResponse_pipelineExecutionArn,
    updatePipelineExecutionResponse_httpStatus,

    -- ** UpdateProject
    updateProject_tags,
    updateProject_projectDescription,
    updateProject_serviceCatalogProvisioningUpdateDetails,
    updateProject_projectName,
    updateProjectResponse_httpStatus,
    updateProjectResponse_projectArn,

    -- ** UpdateTrainingJob
    updateTrainingJob_profilerConfig,
    updateTrainingJob_profilerRuleConfigurations,
    updateTrainingJob_resourceConfig,
    updateTrainingJob_trainingJobName,
    updateTrainingJobResponse_httpStatus,
    updateTrainingJobResponse_trainingJobArn,

    -- ** UpdateTrial
    updateTrial_displayName,
    updateTrial_trialName,
    updateTrialResponse_trialArn,
    updateTrialResponse_httpStatus,

    -- ** UpdateTrialComponent
    updateTrialComponent_outputArtifactsToRemove,
    updateTrialComponent_displayName,
    updateTrialComponent_status,
    updateTrialComponent_outputArtifacts,
    updateTrialComponent_endTime,
    updateTrialComponent_parametersToRemove,
    updateTrialComponent_inputArtifacts,
    updateTrialComponent_startTime,
    updateTrialComponent_inputArtifactsToRemove,
    updateTrialComponent_parameters,
    updateTrialComponent_trialComponentName,
    updateTrialComponentResponse_trialComponentArn,
    updateTrialComponentResponse_httpStatus,

    -- ** UpdateUserProfile
    updateUserProfile_userSettings,
    updateUserProfile_domainId,
    updateUserProfile_userProfileName,
    updateUserProfileResponse_userProfileArn,
    updateUserProfileResponse_httpStatus,

    -- ** UpdateWorkforce
    updateWorkforce_sourceIpConfig,
    updateWorkforce_workforceVpcConfig,
    updateWorkforce_oidcConfig,
    updateWorkforce_workforceName,
    updateWorkforceResponse_httpStatus,
    updateWorkforceResponse_workforce,

    -- ** UpdateWorkteam
    updateWorkteam_notificationConfiguration,
    updateWorkteam_description,
    updateWorkteam_memberDefinitions,
    updateWorkteam_workteamName,
    updateWorkteamResponse_httpStatus,
    updateWorkteamResponse_workteam,

    -- * Types

    -- ** ActionSource
    actionSource_sourceId,
    actionSource_sourceType,
    actionSource_sourceUri,

    -- ** ActionSummary
    actionSummary_actionName,
    actionSummary_actionType,
    actionSummary_status,
    actionSummary_lastModifiedTime,
    actionSummary_source,
    actionSummary_actionArn,
    actionSummary_creationTime,

    -- ** AdditionalInferenceSpecificationDefinition
    additionalInferenceSpecificationDefinition_description,
    additionalInferenceSpecificationDefinition_supportedContentTypes,
    additionalInferenceSpecificationDefinition_supportedResponseMIMETypes,
    additionalInferenceSpecificationDefinition_supportedRealtimeInferenceInstanceTypes,
    additionalInferenceSpecificationDefinition_supportedTransformInstanceTypes,
    additionalInferenceSpecificationDefinition_name,
    additionalInferenceSpecificationDefinition_containers,

    -- ** AgentVersion
    agentVersion_version,
    agentVersion_agentCount,

    -- ** Alarm
    alarm_alarmName,

    -- ** AlgorithmSpecification
    algorithmSpecification_containerEntrypoint,
    algorithmSpecification_algorithmName,
    algorithmSpecification_metricDefinitions,
    algorithmSpecification_containerArguments,
    algorithmSpecification_trainingImage,
    algorithmSpecification_enableSageMakerMetricsTimeSeries,
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
    appDetails_appName,
    appDetails_appType,
    appDetails_status,
    appDetails_userProfileName,
    appDetails_creationTime,
    appDetails_domainId,

    -- ** AppImageConfigDetails
    appImageConfigDetails_appImageConfigArn,
    appImageConfigDetails_appImageConfigName,
    appImageConfigDetails_kernelGatewayImageConfig,
    appImageConfigDetails_lastModifiedTime,
    appImageConfigDetails_creationTime,

    -- ** AppSpecification
    appSpecification_containerEntrypoint,
    appSpecification_containerArguments,
    appSpecification_imageUri,

    -- ** ArtifactSource
    artifactSource_sourceTypes,
    artifactSource_sourceUri,

    -- ** ArtifactSourceType
    artifactSourceType_sourceIdType,
    artifactSourceType_value,

    -- ** ArtifactSummary
    artifactSummary_artifactName,
    artifactSummary_artifactType,
    artifactSummary_artifactArn,
    artifactSummary_lastModifiedTime,
    artifactSummary_source,
    artifactSummary_creationTime,

    -- ** AssociationSummary
    associationSummary_associationType,
    associationSummary_sourceArn,
    associationSummary_sourceName,
    associationSummary_destinationName,
    associationSummary_destinationType,
    associationSummary_sourceType,
    associationSummary_creationTime,
    associationSummary_createdBy,
    associationSummary_destinationArn,

    -- ** AsyncInferenceClientConfig
    asyncInferenceClientConfig_maxConcurrentInvocationsPerInstance,

    -- ** AsyncInferenceConfig
    asyncInferenceConfig_clientConfig,
    asyncInferenceConfig_outputConfig,

    -- ** AsyncInferenceNotificationConfig
    asyncInferenceNotificationConfig_errorTopic,
    asyncInferenceNotificationConfig_successTopic,

    -- ** AsyncInferenceOutputConfig
    asyncInferenceOutputConfig_kmsKeyId,
    asyncInferenceOutputConfig_notificationConfig,
    asyncInferenceOutputConfig_s3OutputPath,

    -- ** AthenaDatasetDefinition
    athenaDatasetDefinition_workGroup,
    athenaDatasetDefinition_kmsKeyId,
    athenaDatasetDefinition_outputCompression,
    athenaDatasetDefinition_catalog,
    athenaDatasetDefinition_database,
    athenaDatasetDefinition_queryString,
    athenaDatasetDefinition_outputS3Uri,
    athenaDatasetDefinition_outputFormat,

    -- ** AutoMLCandidate
    autoMLCandidate_finalAutoMLJobObjectiveMetric,
    autoMLCandidate_candidateProperties,
    autoMLCandidate_inferenceContainers,
    autoMLCandidate_endTime,
    autoMLCandidate_failureReason,
    autoMLCandidate_candidateName,
    autoMLCandidate_objectiveStatus,
    autoMLCandidate_candidateSteps,
    autoMLCandidate_candidateStatus,
    autoMLCandidate_creationTime,
    autoMLCandidate_lastModifiedTime,

    -- ** AutoMLCandidateGenerationConfig
    autoMLCandidateGenerationConfig_featureSpecificationS3Uri,

    -- ** AutoMLCandidateStep
    autoMLCandidateStep_candidateStepType,
    autoMLCandidateStep_candidateStepArn,
    autoMLCandidateStep_candidateStepName,

    -- ** AutoMLChannel
    autoMLChannel_compressionType,
    autoMLChannel_channelType,
    autoMLChannel_contentType,
    autoMLChannel_dataSource,
    autoMLChannel_targetAttributeName,

    -- ** AutoMLContainerDefinition
    autoMLContainerDefinition_environment,
    autoMLContainerDefinition_image,
    autoMLContainerDefinition_modelDataUrl,

    -- ** AutoMLDataSource
    autoMLDataSource_s3DataSource,

    -- ** AutoMLDataSplitConfig
    autoMLDataSplitConfig_validationFraction,

    -- ** AutoMLJobArtifacts
    autoMLJobArtifacts_dataExplorationNotebookLocation,
    autoMLJobArtifacts_candidateDefinitionNotebookLocation,

    -- ** AutoMLJobCompletionCriteria
    autoMLJobCompletionCriteria_maxCandidates,
    autoMLJobCompletionCriteria_maxRuntimePerTrainingJobInSeconds,
    autoMLJobCompletionCriteria_maxAutoMLJobRuntimeInSeconds,

    -- ** AutoMLJobConfig
    autoMLJobConfig_dataSplitConfig,
    autoMLJobConfig_completionCriteria,
    autoMLJobConfig_candidateGenerationConfig,
    autoMLJobConfig_securityConfig,
    autoMLJobConfig_mode,

    -- ** AutoMLJobObjective
    autoMLJobObjective_metricName,

    -- ** AutoMLJobSummary
    autoMLJobSummary_partialFailureReasons,
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

    -- ** AutoMLPartialFailureReason
    autoMLPartialFailureReason_partialFailureMessage,

    -- ** AutoMLS3DataSource
    autoMLS3DataSource_s3DataType,
    autoMLS3DataSource_s3Uri,

    -- ** AutoMLSecurityConfig
    autoMLSecurityConfig_vpcConfig,
    autoMLSecurityConfig_volumeKmsKeyId,
    autoMLSecurityConfig_enableInterContainerTrafficEncryption,

    -- ** AutoRollbackConfig
    autoRollbackConfig_alarms,

    -- ** BatchDataCaptureConfig
    batchDataCaptureConfig_kmsKeyId,
    batchDataCaptureConfig_generateInferenceId,
    batchDataCaptureConfig_destinationS3Uri,

    -- ** BatchDescribeModelPackageError
    batchDescribeModelPackageError_errorCode,
    batchDescribeModelPackageError_errorResponse,

    -- ** BatchDescribeModelPackageSummary
    batchDescribeModelPackageSummary_modelPackageVersion,
    batchDescribeModelPackageSummary_modelApprovalStatus,
    batchDescribeModelPackageSummary_modelPackageDescription,
    batchDescribeModelPackageSummary_modelPackageGroupName,
    batchDescribeModelPackageSummary_modelPackageArn,
    batchDescribeModelPackageSummary_creationTime,
    batchDescribeModelPackageSummary_inferenceSpecification,
    batchDescribeModelPackageSummary_modelPackageStatus,

    -- ** BatchTransformInput
    batchTransformInput_probabilityThresholdAttribute,
    batchTransformInput_s3InputMode,
    batchTransformInput_s3DataDistributionType,
    batchTransformInput_probabilityAttribute,
    batchTransformInput_startTimeOffset,
    batchTransformInput_featuresAttribute,
    batchTransformInput_inferenceAttribute,
    batchTransformInput_endTimeOffset,
    batchTransformInput_dataCapturedDestinationS3Uri,
    batchTransformInput_datasetFormat,
    batchTransformInput_localPath,

    -- ** Bias
    bias_preTrainingReport,
    bias_postTrainingReport,
    bias_report,

    -- ** BlueGreenUpdatePolicy
    blueGreenUpdatePolicy_terminationWaitInSeconds,
    blueGreenUpdatePolicy_maximumExecutionTimeoutInSeconds,
    blueGreenUpdatePolicy_trafficRoutingConfiguration,

    -- ** CacheHitResult
    cacheHitResult_sourcePipelineExecutionArn,

    -- ** CallbackStepMetadata
    callbackStepMetadata_outputParameters,
    callbackStepMetadata_callbackToken,
    callbackStepMetadata_sqsQueueUrl,

    -- ** CandidateArtifactLocations
    candidateArtifactLocations_modelInsights,
    candidateArtifactLocations_explainability,

    -- ** CandidateProperties
    candidateProperties_candidateMetrics,
    candidateProperties_candidateArtifactLocations,

    -- ** CanvasAppSettings
    canvasAppSettings_timeSeriesForecastingSettings,

    -- ** CapacitySize
    capacitySize_type,
    capacitySize_value,

    -- ** CaptureContentTypeHeader
    captureContentTypeHeader_csvContentTypes,
    captureContentTypeHeader_jsonContentTypes,

    -- ** CaptureOption
    captureOption_captureMode,

    -- ** CategoricalParameter
    categoricalParameter_name,
    categoricalParameter_value,

    -- ** CategoricalParameterRange
    categoricalParameterRange_name,
    categoricalParameterRange_values,

    -- ** CategoricalParameterRangeSpecification
    categoricalParameterRangeSpecification_values,

    -- ** Channel
    channel_shuffleConfig,
    channel_inputMode,
    channel_compressionType,
    channel_recordWrapperType,
    channel_contentType,
    channel_channelName,
    channel_dataSource,

    -- ** ChannelSpecification
    channelSpecification_supportedCompressionTypes,
    channelSpecification_description,
    channelSpecification_isRequired,
    channelSpecification_name,
    channelSpecification_supportedContentTypes,
    channelSpecification_supportedInputModes,

    -- ** CheckpointConfig
    checkpointConfig_localPath,
    checkpointConfig_s3Uri,

    -- ** ClarifyCheckStepMetadata
    clarifyCheckStepMetadata_modelPackageGroupName,
    clarifyCheckStepMetadata_checkJobArn,
    clarifyCheckStepMetadata_checkType,
    clarifyCheckStepMetadata_registerNewBaseline,
    clarifyCheckStepMetadata_skipCheck,
    clarifyCheckStepMetadata_baselineUsedForDriftCheckConstraints,
    clarifyCheckStepMetadata_calculatedBaselineConstraints,
    clarifyCheckStepMetadata_violationReport,

    -- ** ClarifyExplainerConfig
    clarifyExplainerConfig_enableExplanations,
    clarifyExplainerConfig_inferenceConfig,
    clarifyExplainerConfig_shapConfig,

    -- ** ClarifyInferenceConfig
    clarifyInferenceConfig_probabilityIndex,
    clarifyInferenceConfig_maxRecordCount,
    clarifyInferenceConfig_labelAttribute,
    clarifyInferenceConfig_labelIndex,
    clarifyInferenceConfig_contentTemplate,
    clarifyInferenceConfig_probabilityAttribute,
    clarifyInferenceConfig_featureTypes,
    clarifyInferenceConfig_featuresAttribute,
    clarifyInferenceConfig_maxPayloadInMB,
    clarifyInferenceConfig_labelHeaders,
    clarifyInferenceConfig_featureHeaders,

    -- ** ClarifyShapBaselineConfig
    clarifyShapBaselineConfig_shapBaseline,
    clarifyShapBaselineConfig_shapBaselineUri,
    clarifyShapBaselineConfig_mimeType,

    -- ** ClarifyShapConfig
    clarifyShapConfig_seed,
    clarifyShapConfig_textConfig,
    clarifyShapConfig_useLogit,
    clarifyShapConfig_numberOfSamples,
    clarifyShapConfig_shapBaselineConfig,

    -- ** ClarifyTextConfig
    clarifyTextConfig_language,
    clarifyTextConfig_granularity,

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
    collectionConfiguration_collectionName,
    collectionConfiguration_collectionParameters,

    -- ** CompilationJobSummary
    compilationJobSummary_compilationEndTime,
    compilationJobSummary_compilationTargetDevice,
    compilationJobSummary_compilationTargetPlatformOs,
    compilationJobSummary_compilationStartTime,
    compilationJobSummary_lastModifiedTime,
    compilationJobSummary_compilationTargetPlatformArch,
    compilationJobSummary_compilationTargetPlatformAccelerator,
    compilationJobSummary_compilationJobName,
    compilationJobSummary_compilationJobArn,
    compilationJobSummary_creationTime,
    compilationJobSummary_compilationJobStatus,

    -- ** ConditionStepMetadata
    conditionStepMetadata_outcome,

    -- ** ContainerDefinition
    containerDefinition_imageConfig,
    containerDefinition_environment,
    containerDefinition_containerHostname,
    containerDefinition_modelDataUrl,
    containerDefinition_multiModelConfig,
    containerDefinition_inferenceSpecificationName,
    containerDefinition_mode,
    containerDefinition_image,
    containerDefinition_modelPackageName,

    -- ** ContextSource
    contextSource_sourceId,
    contextSource_sourceType,
    contextSource_sourceUri,

    -- ** ContextSummary
    contextSummary_contextName,
    contextSummary_lastModifiedTime,
    contextSummary_source,
    contextSummary_creationTime,
    contextSummary_contextType,
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
    dataProcessing_inputFilter,
    dataProcessing_joinSource,

    -- ** DataQualityAppSpecification
    dataQualityAppSpecification_containerEntrypoint,
    dataQualityAppSpecification_recordPreprocessorSourceUri,
    dataQualityAppSpecification_environment,
    dataQualityAppSpecification_containerArguments,
    dataQualityAppSpecification_postAnalyticsProcessorSourceUri,
    dataQualityAppSpecification_imageUri,

    -- ** DataQualityBaselineConfig
    dataQualityBaselineConfig_baseliningJobName,
    dataQualityBaselineConfig_constraintsResource,
    dataQualityBaselineConfig_statisticsResource,

    -- ** DataQualityJobInput
    dataQualityJobInput_endpointInput,
    dataQualityJobInput_batchTransformInput,

    -- ** DataSource
    dataSource_s3DataSource,
    dataSource_fileSystemDataSource,

    -- ** DatasetDefinition
    datasetDefinition_inputMode,
    datasetDefinition_athenaDatasetDefinition,
    datasetDefinition_dataDistributionType,
    datasetDefinition_localPath,
    datasetDefinition_redshiftDatasetDefinition,

    -- ** DebugHookConfig
    debugHookConfig_collectionConfigurations,
    debugHookConfig_localPath,
    debugHookConfig_hookParameters,
    debugHookConfig_s3OutputPath,

    -- ** DebugRuleConfiguration
    debugRuleConfiguration_s3OutputPath,
    debugRuleConfiguration_instanceType,
    debugRuleConfiguration_ruleParameters,
    debugRuleConfiguration_localPath,
    debugRuleConfiguration_volumeSizeInGB,
    debugRuleConfiguration_ruleConfigurationName,
    debugRuleConfiguration_ruleEvaluatorImage,

    -- ** DebugRuleEvaluationStatus
    debugRuleEvaluationStatus_statusDetails,
    debugRuleEvaluationStatus_lastModifiedTime,
    debugRuleEvaluationStatus_ruleEvaluationJobArn,
    debugRuleEvaluationStatus_ruleConfigurationName,
    debugRuleEvaluationStatus_ruleEvaluationStatus,

    -- ** DeployedImage
    deployedImage_specifiedImage,
    deployedImage_resolvedImage,
    deployedImage_resolutionTime,

    -- ** DeploymentConfig
    deploymentConfig_autoRollbackConfiguration,
    deploymentConfig_blueGreenUpdatePolicy,

    -- ** DeploymentStage
    deploymentStage_deploymentConfig,
    deploymentStage_stageName,
    deploymentStage_deviceSelectionConfig,

    -- ** DeploymentStageStatusSummary
    deploymentStageStatusSummary_stageName,
    deploymentStageStatusSummary_deviceSelectionConfig,
    deploymentStageStatusSummary_deploymentConfig,
    deploymentStageStatusSummary_deploymentStatus,

    -- ** DesiredWeightAndCapacity
    desiredWeightAndCapacity_desiredWeight,
    desiredWeightAndCapacity_desiredInstanceCount,
    desiredWeightAndCapacity_variantName,

    -- ** Device
    device_iotThingName,
    device_description,
    device_deviceName,

    -- ** DeviceDeploymentSummary
    deviceDeploymentSummary_deviceDeploymentStatus,
    deviceDeploymentSummary_deviceDeploymentStatusMessage,
    deviceDeploymentSummary_deployedStageName,
    deviceDeploymentSummary_deviceFleetName,
    deviceDeploymentSummary_description,
    deviceDeploymentSummary_deploymentStartTime,
    deviceDeploymentSummary_edgeDeploymentPlanArn,
    deviceDeploymentSummary_edgeDeploymentPlanName,
    deviceDeploymentSummary_stageName,
    deviceDeploymentSummary_deviceName,
    deviceDeploymentSummary_deviceArn,

    -- ** DeviceFleetSummary
    deviceFleetSummary_lastModifiedTime,
    deviceFleetSummary_creationTime,
    deviceFleetSummary_deviceFleetArn,
    deviceFleetSummary_deviceFleetName,

    -- ** DeviceSelectionConfig
    deviceSelectionConfig_deviceNameContains,
    deviceSelectionConfig_percentage,
    deviceSelectionConfig_deviceNames,
    deviceSelectionConfig_deviceSubsetType,

    -- ** DeviceStats
    deviceStats_connectedDeviceCount,
    deviceStats_registeredDeviceCount,

    -- ** DeviceSummary
    deviceSummary_models,
    deviceSummary_iotThingName,
    deviceSummary_deviceFleetName,
    deviceSummary_description,
    deviceSummary_registrationTime,
    deviceSummary_latestHeartbeat,
    deviceSummary_agentVersion,
    deviceSummary_deviceName,
    deviceSummary_deviceArn,

    -- ** DomainDetails
    domainDetails_domainName,
    domainDetails_domainArn,
    domainDetails_url,
    domainDetails_status,
    domainDetails_lastModifiedTime,
    domainDetails_creationTime,
    domainDetails_domainId,

    -- ** DomainSettings
    domainSettings_securityGroupIds,
    domainSettings_executionRoleIdentityConfig,
    domainSettings_rStudioServerProDomainSettings,

    -- ** DomainSettingsForUpdate
    domainSettingsForUpdate_executionRoleIdentityConfig,
    domainSettingsForUpdate_rStudioServerProDomainSettingsForUpdate,

    -- ** DriftCheckBaselines
    driftCheckBaselines_modelDataQuality,
    driftCheckBaselines_modelQuality,
    driftCheckBaselines_bias,
    driftCheckBaselines_explainability,

    -- ** DriftCheckBias
    driftCheckBias_postTrainingConstraints,
    driftCheckBias_preTrainingConstraints,
    driftCheckBias_configFile,

    -- ** DriftCheckExplainability
    driftCheckExplainability_constraints,
    driftCheckExplainability_configFile,

    -- ** DriftCheckModelDataQuality
    driftCheckModelDataQuality_constraints,
    driftCheckModelDataQuality_statistics,

    -- ** DriftCheckModelQuality
    driftCheckModelQuality_constraints,
    driftCheckModelQuality_statistics,

    -- ** EMRStepMetadata
    eMRStepMetadata_logFilePath,
    eMRStepMetadata_stepName,
    eMRStepMetadata_clusterId,
    eMRStepMetadata_stepId,

    -- ** Edge
    edge_associationType,
    edge_sourceArn,
    edge_destinationArn,

    -- ** EdgeDeploymentConfig
    edgeDeploymentConfig_failureHandlingPolicy,

    -- ** EdgeDeploymentModelConfig
    edgeDeploymentModelConfig_modelHandle,
    edgeDeploymentModelConfig_edgePackagingJobName,

    -- ** EdgeDeploymentPlanSummary
    edgeDeploymentPlanSummary_lastModifiedTime,
    edgeDeploymentPlanSummary_creationTime,
    edgeDeploymentPlanSummary_edgeDeploymentPlanArn,
    edgeDeploymentPlanSummary_edgeDeploymentPlanName,
    edgeDeploymentPlanSummary_deviceFleetName,
    edgeDeploymentPlanSummary_edgeDeploymentSuccess,
    edgeDeploymentPlanSummary_edgeDeploymentPending,
    edgeDeploymentPlanSummary_edgeDeploymentFailed,

    -- ** EdgeDeploymentStatus
    edgeDeploymentStatus_edgeDeploymentStatusMessage,
    edgeDeploymentStatus_edgeDeploymentStageStartTime,
    edgeDeploymentStatus_stageStatus,
    edgeDeploymentStatus_edgeDeploymentSuccessInStage,
    edgeDeploymentStatus_edgeDeploymentPendingInStage,
    edgeDeploymentStatus_edgeDeploymentFailedInStage,

    -- ** EdgeModel
    edgeModel_latestSampleTime,
    edgeModel_latestInference,
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
    edgeOutputConfig_presetDeploymentType,
    edgeOutputConfig_presetDeploymentConfig,
    edgeOutputConfig_kmsKeyId,
    edgeOutputConfig_s3OutputLocation,

    -- ** EdgePackagingJobSummary
    edgePackagingJobSummary_compilationJobName,
    edgePackagingJobSummary_modelVersion,
    edgePackagingJobSummary_lastModifiedTime,
    edgePackagingJobSummary_modelName,
    edgePackagingJobSummary_creationTime,
    edgePackagingJobSummary_edgePackagingJobArn,
    edgePackagingJobSummary_edgePackagingJobName,
    edgePackagingJobSummary_edgePackagingJobStatus,

    -- ** EdgePresetDeploymentOutput
    edgePresetDeploymentOutput_status,
    edgePresetDeploymentOutput_artifact,
    edgePresetDeploymentOutput_statusMessage,
    edgePresetDeploymentOutput_type,

    -- ** Endpoint
    endpoint_tags,
    endpoint_monitoringSchedules,
    endpoint_dataCaptureConfig,
    endpoint_productionVariants,
    endpoint_failureReason,
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

    -- ** EndpointInfo
    endpointInfo_endpointName,

    -- ** EndpointInput
    endpointInput_probabilityThresholdAttribute,
    endpointInput_s3InputMode,
    endpointInput_s3DataDistributionType,
    endpointInput_probabilityAttribute,
    endpointInput_startTimeOffset,
    endpointInput_featuresAttribute,
    endpointInput_inferenceAttribute,
    endpointInput_endTimeOffset,
    endpointInput_endpointName,
    endpointInput_localPath,

    -- ** EndpointInputConfiguration
    endpointInputConfiguration_inferenceSpecificationName,
    endpointInputConfiguration_environmentParameterRanges,
    endpointInputConfiguration_instanceType,

    -- ** EndpointOutputConfiguration
    endpointOutputConfiguration_endpointName,
    endpointOutputConfiguration_variantName,
    endpointOutputConfiguration_instanceType,
    endpointOutputConfiguration_initialInstanceCount,

    -- ** EndpointPerformance
    endpointPerformance_metrics,
    endpointPerformance_endpointInfo,

    -- ** EndpointSummary
    endpointSummary_endpointName,
    endpointSummary_endpointArn,
    endpointSummary_creationTime,
    endpointSummary_lastModifiedTime,
    endpointSummary_endpointStatus,

    -- ** EnvironmentParameter
    environmentParameter_key,
    environmentParameter_valueType,
    environmentParameter_value,

    -- ** EnvironmentParameterRanges
    environmentParameterRanges_categoricalParameterRanges,

    -- ** Experiment
    experiment_tags,
    experiment_displayName,
    experiment_description,
    experiment_lastModifiedTime,
    experiment_source,
    experiment_experimentArn,
    experiment_creationTime,
    experiment_lastModifiedBy,
    experiment_createdBy,
    experiment_experimentName,

    -- ** ExperimentConfig
    experimentConfig_trialName,
    experimentConfig_trialComponentDisplayName,
    experimentConfig_experimentName,

    -- ** ExperimentSource
    experimentSource_sourceType,
    experimentSource_sourceArn,

    -- ** ExperimentSummary
    experimentSummary_displayName,
    experimentSummary_lastModifiedTime,
    experimentSummary_experimentSource,
    experimentSummary_experimentArn,
    experimentSummary_creationTime,
    experimentSummary_experimentName,

    -- ** Explainability
    explainability_report,

    -- ** ExplainerConfig
    explainerConfig_clarifyExplainerConfig,

    -- ** FailStepMetadata
    failStepMetadata_errorMessage,

    -- ** FeatureDefinition
    featureDefinition_featureType,
    featureDefinition_featureName,

    -- ** FeatureGroup
    featureGroup_tags,
    featureGroup_recordIdentifierFeatureName,
    featureGroup_roleArn,
    featureGroup_description,
    featureGroup_offlineStoreStatus,
    featureGroup_onlineStoreConfig,
    featureGroup_lastModifiedTime,
    featureGroup_featureGroupName,
    featureGroup_lastUpdateStatus,
    featureGroup_creationTime,
    featureGroup_featureGroupStatus,
    featureGroup_offlineStoreConfig,
    featureGroup_eventTimeFeatureName,
    featureGroup_featureGroupArn,
    featureGroup_failureReason,
    featureGroup_featureDefinitions,

    -- ** FeatureGroupSummary
    featureGroupSummary_offlineStoreStatus,
    featureGroupSummary_featureGroupStatus,
    featureGroupSummary_featureGroupName,
    featureGroupSummary_featureGroupArn,
    featureGroupSummary_creationTime,

    -- ** FeatureMetadata
    featureMetadata_featureType,
    featureMetadata_featureName,
    featureMetadata_description,
    featureMetadata_lastModifiedTime,
    featureMetadata_featureGroupName,
    featureMetadata_creationTime,
    featureMetadata_featureGroupArn,
    featureMetadata_parameters,

    -- ** FeatureParameter
    featureParameter_key,
    featureParameter_value,

    -- ** FileSource
    fileSource_contentDigest,
    fileSource_contentType,
    fileSource_s3Uri,

    -- ** FileSystemConfig
    fileSystemConfig_defaultGid,
    fileSystemConfig_defaultUid,
    fileSystemConfig_mountPath,

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
    gitConfig_branch,
    gitConfig_secretArn,
    gitConfig_repositoryUrl,

    -- ** GitConfigForUpdate
    gitConfigForUpdate_secretArn,

    -- ** HumanLoopActivationConditionsConfig
    humanLoopActivationConditionsConfig_humanLoopActivationConditions,

    -- ** HumanLoopActivationConfig
    humanLoopActivationConfig_humanLoopActivationConditionsConfig,

    -- ** HumanLoopConfig
    humanLoopConfig_publicWorkforceTaskPrice,
    humanLoopConfig_taskKeywords,
    humanLoopConfig_taskTimeLimitInSeconds,
    humanLoopConfig_taskAvailabilityLifetimeInSeconds,
    humanLoopConfig_workteamArn,
    humanLoopConfig_humanTaskUiArn,
    humanLoopConfig_taskTitle,
    humanLoopConfig_taskDescription,
    humanLoopConfig_taskCount,

    -- ** HumanLoopRequestSource
    humanLoopRequestSource_awsManagedHumanLoopRequestSource,

    -- ** HumanTaskConfig
    humanTaskConfig_maxConcurrentTaskCount,
    humanTaskConfig_publicWorkforceTaskPrice,
    humanTaskConfig_taskKeywords,
    humanTaskConfig_taskAvailabilityLifetimeInSeconds,
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
    hyperParameterAlgorithmSpecification_algorithmName,
    hyperParameterAlgorithmSpecification_metricDefinitions,
    hyperParameterAlgorithmSpecification_trainingImage,
    hyperParameterAlgorithmSpecification_trainingInputMode,

    -- ** HyperParameterSpecification
    hyperParameterSpecification_defaultValue,
    hyperParameterSpecification_description,
    hyperParameterSpecification_isTunable,
    hyperParameterSpecification_range,
    hyperParameterSpecification_isRequired,
    hyperParameterSpecification_name,
    hyperParameterSpecification_type,

    -- ** HyperParameterTrainingJobDefinition
    hyperParameterTrainingJobDefinition_tuningObjective,
    hyperParameterTrainingJobDefinition_enableManagedSpotTraining,
    hyperParameterTrainingJobDefinition_hyperParameterTuningResourceConfig,
    hyperParameterTrainingJobDefinition_hyperParameterRanges,
    hyperParameterTrainingJobDefinition_retryStrategy,
    hyperParameterTrainingJobDefinition_vpcConfig,
    hyperParameterTrainingJobDefinition_enableNetworkIsolation,
    hyperParameterTrainingJobDefinition_resourceConfig,
    hyperParameterTrainingJobDefinition_definitionName,
    hyperParameterTrainingJobDefinition_staticHyperParameters,
    hyperParameterTrainingJobDefinition_checkpointConfig,
    hyperParameterTrainingJobDefinition_enableInterContainerTrafficEncryption,
    hyperParameterTrainingJobDefinition_inputDataConfig,
    hyperParameterTrainingJobDefinition_algorithmSpecification,
    hyperParameterTrainingJobDefinition_roleArn,
    hyperParameterTrainingJobDefinition_outputDataConfig,
    hyperParameterTrainingJobDefinition_stoppingCondition,

    -- ** HyperParameterTrainingJobSummary
    hyperParameterTrainingJobSummary_trainingStartTime,
    hyperParameterTrainingJobSummary_objectiveStatus,
    hyperParameterTrainingJobSummary_tuningJobName,
    hyperParameterTrainingJobSummary_trainingJobDefinitionName,
    hyperParameterTrainingJobSummary_finalHyperParameterTuningJobObjectiveMetric,
    hyperParameterTrainingJobSummary_failureReason,
    hyperParameterTrainingJobSummary_trainingEndTime,
    hyperParameterTrainingJobSummary_trainingJobName,
    hyperParameterTrainingJobSummary_trainingJobArn,
    hyperParameterTrainingJobSummary_creationTime,
    hyperParameterTrainingJobSummary_trainingJobStatus,
    hyperParameterTrainingJobSummary_tunedHyperParameters,

    -- ** HyperParameterTuningInstanceConfig
    hyperParameterTuningInstanceConfig_instanceType,
    hyperParameterTuningInstanceConfig_instanceCount,
    hyperParameterTuningInstanceConfig_volumeSizeInGB,

    -- ** HyperParameterTuningJobConfig
    hyperParameterTuningJobConfig_tuningJobCompletionCriteria,
    hyperParameterTuningJobConfig_trainingJobEarlyStoppingType,
    hyperParameterTuningJobConfig_hyperParameterTuningJobObjective,
    hyperParameterTuningJobConfig_parameterRanges,
    hyperParameterTuningJobConfig_strategyConfig,
    hyperParameterTuningJobConfig_strategy,
    hyperParameterTuningJobConfig_resourceLimits,

    -- ** HyperParameterTuningJobObjective
    hyperParameterTuningJobObjective_type,
    hyperParameterTuningJobObjective_metricName,

    -- ** HyperParameterTuningJobSearchEntity
    hyperParameterTuningJobSearchEntity_tags,
    hyperParameterTuningJobSearchEntity_overallBestTrainingJob,
    hyperParameterTuningJobSearchEntity_bestTrainingJob,
    hyperParameterTuningJobSearchEntity_trainingJobDefinitions,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningJobStatus,
    hyperParameterTuningJobSearchEntity_lastModifiedTime,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningJobName,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningJobArn,
    hyperParameterTuningJobSearchEntity_creationTime,
    hyperParameterTuningJobSearchEntity_warmStartConfig,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningJobConfig,
    hyperParameterTuningJobSearchEntity_trainingJobStatusCounters,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningEndTime,
    hyperParameterTuningJobSearchEntity_objectiveStatusCounters,
    hyperParameterTuningJobSearchEntity_trainingJobDefinition,
    hyperParameterTuningJobSearchEntity_failureReason,

    -- ** HyperParameterTuningJobStrategyConfig
    hyperParameterTuningJobStrategyConfig_hyperbandStrategyConfig,

    -- ** HyperParameterTuningJobSummary
    hyperParameterTuningJobSummary_resourceLimits,
    hyperParameterTuningJobSummary_lastModifiedTime,
    hyperParameterTuningJobSummary_hyperParameterTuningEndTime,
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

    -- ** HyperParameterTuningResourceConfig
    hyperParameterTuningResourceConfig_volumeKmsKeyId,
    hyperParameterTuningResourceConfig_instanceType,
    hyperParameterTuningResourceConfig_allocationStrategy,
    hyperParameterTuningResourceConfig_instanceCount,
    hyperParameterTuningResourceConfig_instanceConfigs,
    hyperParameterTuningResourceConfig_volumeSizeInGB,

    -- ** HyperbandStrategyConfig
    hyperbandStrategyConfig_minResource,
    hyperbandStrategyConfig_maxResource,

    -- ** Image
    image_displayName,
    image_description,
    image_failureReason,
    image_creationTime,
    image_imageArn,
    image_imageName,
    image_imageStatus,
    image_lastModifiedTime,

    -- ** ImageConfig
    imageConfig_repositoryAuthConfig,
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

    -- ** InferenceMetrics
    inferenceMetrics_maxInvocations,
    inferenceMetrics_modelLatency,

    -- ** InferenceRecommendation
    inferenceRecommendation_metrics,
    inferenceRecommendation_endpointConfiguration,
    inferenceRecommendation_modelConfiguration,

    -- ** InferenceRecommendationsJob
    inferenceRecommendationsJob_completionTime,
    inferenceRecommendationsJob_failureReason,
    inferenceRecommendationsJob_jobName,
    inferenceRecommendationsJob_jobDescription,
    inferenceRecommendationsJob_jobType,
    inferenceRecommendationsJob_jobArn,
    inferenceRecommendationsJob_status,
    inferenceRecommendationsJob_creationTime,
    inferenceRecommendationsJob_roleArn,
    inferenceRecommendationsJob_lastModifiedTime,

    -- ** InferenceRecommendationsJobStep
    inferenceRecommendationsJobStep_inferenceBenchmark,
    inferenceRecommendationsJobStep_stepType,
    inferenceRecommendationsJobStep_jobName,
    inferenceRecommendationsJobStep_status,

    -- ** InferenceSpecification
    inferenceSpecification_supportedRealtimeInferenceInstanceTypes,
    inferenceSpecification_supportedTransformInstanceTypes,
    inferenceSpecification_containers,
    inferenceSpecification_supportedContentTypes,
    inferenceSpecification_supportedResponseMIMETypes,

    -- ** InputConfig
    inputConfig_frameworkVersion,
    inputConfig_s3Uri,
    inputConfig_dataInputConfig,
    inputConfig_framework,

    -- ** InstanceGroup
    instanceGroup_instanceType,
    instanceGroup_instanceCount,
    instanceGroup_instanceGroupName,

    -- ** InstanceMetadataServiceConfiguration
    instanceMetadataServiceConfiguration_minimumInstanceMetadataServiceVersion,

    -- ** IntegerParameterRange
    integerParameterRange_scalingType,
    integerParameterRange_name,
    integerParameterRange_minValue,
    integerParameterRange_maxValue,

    -- ** IntegerParameterRangeSpecification
    integerParameterRangeSpecification_minValue,
    integerParameterRangeSpecification_maxValue,

    -- ** JupyterServerAppSettings
    jupyterServerAppSettings_lifecycleConfigArns,
    jupyterServerAppSettings_defaultResourceSpec,

    -- ** KernelGatewayAppSettings
    kernelGatewayAppSettings_lifecycleConfigArns,
    kernelGatewayAppSettings_defaultResourceSpec,
    kernelGatewayAppSettings_customImages,

    -- ** KernelGatewayImageConfig
    kernelGatewayImageConfig_fileSystemConfig,
    kernelGatewayImageConfig_kernelSpecs,

    -- ** KernelSpec
    kernelSpec_displayName,
    kernelSpec_name,

    -- ** LabelCounters
    labelCounters_machineLabeled,
    labelCounters_totalLabeled,
    labelCounters_unlabeled,
    labelCounters_failedNonRetryableError,
    labelCounters_humanLabeled,

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
    labelingJobDataSource_s3DataSource,
    labelingJobDataSource_snsDataSource,

    -- ** LabelingJobForWorkteamSummary
    labelingJobForWorkteamSummary_numberOfHumanWorkersPerDataObject,
    labelingJobForWorkteamSummary_labelingJobName,
    labelingJobForWorkteamSummary_labelCounters,
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
    labelingJobOutputConfig_snsTopicArn,
    labelingJobOutputConfig_kmsKeyId,
    labelingJobOutputConfig_s3OutputPath,

    -- ** LabelingJobResourceConfig
    labelingJobResourceConfig_vpcConfig,
    labelingJobResourceConfig_volumeKmsKeyId,

    -- ** LabelingJobS3DataSource
    labelingJobS3DataSource_manifestS3Uri,

    -- ** LabelingJobSnsDataSource
    labelingJobSnsDataSource_snsTopicArn,

    -- ** LabelingJobStoppingConditions
    labelingJobStoppingConditions_maxPercentageOfInputDatasetLabeled,
    labelingJobStoppingConditions_maxHumanLabeledObjectCount,

    -- ** LabelingJobSummary
    labelingJobSummary_labelingJobOutput,
    labelingJobSummary_annotationConsolidationLambdaArn,
    labelingJobSummary_inputConfig,
    labelingJobSummary_failureReason,
    labelingJobSummary_labelingJobName,
    labelingJobSummary_labelingJobArn,
    labelingJobSummary_creationTime,
    labelingJobSummary_lastModifiedTime,
    labelingJobSummary_labelingJobStatus,
    labelingJobSummary_labelCounters,
    labelingJobSummary_workteamArn,
    labelingJobSummary_preHumanTaskLambdaArn,

    -- ** LambdaStepMetadata
    lambdaStepMetadata_outputParameters,
    lambdaStepMetadata_arn,

    -- ** LastUpdateStatus
    lastUpdateStatus_failureReason,
    lastUpdateStatus_status,

    -- ** LineageGroupSummary
    lineageGroupSummary_lineageGroupName,
    lineageGroupSummary_displayName,
    lineageGroupSummary_lastModifiedTime,
    lineageGroupSummary_lineageGroupArn,
    lineageGroupSummary_creationTime,

    -- ** MemberDefinition
    memberDefinition_cognitoMemberDefinition,
    memberDefinition_oidcMemberDefinition,

    -- ** MetadataProperties
    metadataProperties_commitId,
    metadataProperties_repository,
    metadataProperties_generatedBy,
    metadataProperties_projectId,

    -- ** MetricData
    metricData_timestamp,
    metricData_metricName,
    metricData_value,

    -- ** MetricDatum
    metricDatum_metricName,
    metricDatum_set,
    metricDatum_standardMetricName,
    metricDatum_value,

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
    modelBiasBaselineConfig_baseliningJobName,
    modelBiasBaselineConfig_constraintsResource,

    -- ** ModelBiasJobInput
    modelBiasJobInput_endpointInput,
    modelBiasJobInput_batchTransformInput,
    modelBiasJobInput_groundTruthS3Input,

    -- ** ModelClientConfig
    modelClientConfig_invocationsMaxRetries,
    modelClientConfig_invocationsTimeoutInSeconds,

    -- ** ModelConfiguration
    modelConfiguration_inferenceSpecificationName,
    modelConfiguration_environmentParameters,

    -- ** ModelDataQuality
    modelDataQuality_constraints,
    modelDataQuality_statistics,

    -- ** ModelDeployConfig
    modelDeployConfig_endpointName,
    modelDeployConfig_autoGenerateEndpointName,

    -- ** ModelDeployResult
    modelDeployResult_endpointName,

    -- ** ModelDigests
    modelDigests_artifactDigest,

    -- ** ModelExplainabilityAppSpecification
    modelExplainabilityAppSpecification_environment,
    modelExplainabilityAppSpecification_imageUri,
    modelExplainabilityAppSpecification_configUri,

    -- ** ModelExplainabilityBaselineConfig
    modelExplainabilityBaselineConfig_baseliningJobName,
    modelExplainabilityBaselineConfig_constraintsResource,

    -- ** ModelExplainabilityJobInput
    modelExplainabilityJobInput_endpointInput,
    modelExplainabilityJobInput_batchTransformInput,

    -- ** ModelInput
    modelInput_dataInputConfig,

    -- ** ModelLatencyThreshold
    modelLatencyThreshold_valueInMilliseconds,
    modelLatencyThreshold_percentile,

    -- ** ModelMetadataFilter
    modelMetadataFilter_name,
    modelMetadataFilter_value,

    -- ** ModelMetadataSearchExpression
    modelMetadataSearchExpression_filters,

    -- ** ModelMetadataSummary
    modelMetadataSummary_domain,
    modelMetadataSummary_framework,
    modelMetadataSummary_task,
    modelMetadataSummary_model,
    modelMetadataSummary_frameworkVersion,

    -- ** ModelMetrics
    modelMetrics_modelDataQuality,
    modelMetrics_modelQuality,
    modelMetrics_bias,
    modelMetrics_explainability,

    -- ** ModelPackage
    modelPackage_tags,
    modelPackage_modelPackageVersion,
    modelPackage_modelPackageGroupName,
    modelPackage_sourceAlgorithmSpecification,
    modelPackage_validationSpecification,
    modelPackage_samplePayloadUrl,
    modelPackage_task,
    modelPackage_certifyForMarketplace,
    modelPackage_inferenceSpecification,
    modelPackage_modelApprovalStatus,
    modelPackage_metadataProperties,
    modelPackage_domain,
    modelPackage_modelPackageDescription,
    modelPackage_driftCheckBaselines,
    modelPackage_approvalDescription,
    modelPackage_modelPackageArn,
    modelPackage_lastModifiedTime,
    modelPackage_modelPackageStatus,
    modelPackage_modelMetrics,
    modelPackage_modelPackageStatusDetails,
    modelPackage_creationTime,
    modelPackage_lastModifiedBy,
    modelPackage_additionalInferenceSpecifications,
    modelPackage_createdBy,
    modelPackage_customerMetadataProperties,
    modelPackage_modelPackageName,

    -- ** ModelPackageContainerDefinition
    modelPackageContainerDefinition_environment,
    modelPackageContainerDefinition_containerHostname,
    modelPackageContainerDefinition_modelDataUrl,
    modelPackageContainerDefinition_modelInput,
    modelPackageContainerDefinition_productId,
    modelPackageContainerDefinition_nearestModelName,
    modelPackageContainerDefinition_frameworkVersion,
    modelPackageContainerDefinition_imageDigest,
    modelPackageContainerDefinition_framework,
    modelPackageContainerDefinition_image,

    -- ** ModelPackageGroup
    modelPackageGroup_tags,
    modelPackageGroup_modelPackageGroupName,
    modelPackageGroup_modelPackageGroupArn,
    modelPackageGroup_modelPackageGroupStatus,
    modelPackageGroup_modelPackageGroupDescription,
    modelPackageGroup_creationTime,
    modelPackageGroup_createdBy,

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
    modelPackageSummary_modelPackageGroupName,
    modelPackageSummary_modelApprovalStatus,
    modelPackageSummary_modelPackageDescription,
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
    modelQualityAppSpecification_containerEntrypoint,
    modelQualityAppSpecification_recordPreprocessorSourceUri,
    modelQualityAppSpecification_environment,
    modelQualityAppSpecification_containerArguments,
    modelQualityAppSpecification_postAnalyticsProcessorSourceUri,
    modelQualityAppSpecification_problemType,
    modelQualityAppSpecification_imageUri,

    -- ** ModelQualityBaselineConfig
    modelQualityBaselineConfig_baseliningJobName,
    modelQualityBaselineConfig_constraintsResource,

    -- ** ModelQualityJobInput
    modelQualityJobInput_endpointInput,
    modelQualityJobInput_batchTransformInput,
    modelQualityJobInput_groundTruthS3Input,

    -- ** ModelStepMetadata
    modelStepMetadata_arn,

    -- ** ModelSummary
    modelSummary_modelName,
    modelSummary_modelArn,
    modelSummary_creationTime,

    -- ** MonitoringAppSpecification
    monitoringAppSpecification_containerEntrypoint,
    monitoringAppSpecification_recordPreprocessorSourceUri,
    monitoringAppSpecification_containerArguments,
    monitoringAppSpecification_postAnalyticsProcessorSourceUri,
    monitoringAppSpecification_imageUri,

    -- ** MonitoringBaselineConfig
    monitoringBaselineConfig_baseliningJobName,
    monitoringBaselineConfig_constraintsResource,
    monitoringBaselineConfig_statisticsResource,

    -- ** MonitoringClusterConfig
    monitoringClusterConfig_volumeKmsKeyId,
    monitoringClusterConfig_instanceCount,
    monitoringClusterConfig_instanceType,
    monitoringClusterConfig_volumeSizeInGB,

    -- ** MonitoringConstraintsResource
    monitoringConstraintsResource_s3Uri,

    -- ** MonitoringCsvDatasetFormat
    monitoringCsvDatasetFormat_header,

    -- ** MonitoringDatasetFormat
    monitoringDatasetFormat_parquet,
    monitoringDatasetFormat_json,
    monitoringDatasetFormat_csv,

    -- ** MonitoringExecutionSummary
    monitoringExecutionSummary_endpointName,
    monitoringExecutionSummary_processingJobArn,
    monitoringExecutionSummary_monitoringType,
    monitoringExecutionSummary_failureReason,
    monitoringExecutionSummary_monitoringJobDefinitionName,
    monitoringExecutionSummary_monitoringScheduleName,
    monitoringExecutionSummary_scheduledTime,
    monitoringExecutionSummary_creationTime,
    monitoringExecutionSummary_lastModifiedTime,
    monitoringExecutionSummary_monitoringExecutionStatus,

    -- ** MonitoringGroundTruthS3Input
    monitoringGroundTruthS3Input_s3Uri,

    -- ** MonitoringInput
    monitoringInput_endpointInput,
    monitoringInput_batchTransformInput,

    -- ** MonitoringJobDefinition
    monitoringJobDefinition_environment,
    monitoringJobDefinition_baselineConfig,
    monitoringJobDefinition_networkConfig,
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

    -- ** MonitoringJsonDatasetFormat
    monitoringJsonDatasetFormat_line,

    -- ** MonitoringNetworkConfig
    monitoringNetworkConfig_vpcConfig,
    monitoringNetworkConfig_enableNetworkIsolation,
    monitoringNetworkConfig_enableInterContainerTrafficEncryption,

    -- ** MonitoringOutput
    monitoringOutput_s3Output,

    -- ** MonitoringOutputConfig
    monitoringOutputConfig_kmsKeyId,
    monitoringOutputConfig_monitoringOutputs,

    -- ** MonitoringParquetDatasetFormat

    -- ** MonitoringResources
    monitoringResources_clusterConfig,

    -- ** MonitoringS3Output
    monitoringS3Output_s3UploadMode,
    monitoringS3Output_s3Uri,
    monitoringS3Output_localPath,

    -- ** MonitoringSchedule
    monitoringSchedule_tags,
    monitoringSchedule_monitoringScheduleArn,
    monitoringSchedule_endpointName,
    monitoringSchedule_monitoringScheduleStatus,
    monitoringSchedule_monitoringScheduleConfig,
    monitoringSchedule_monitoringScheduleName,
    monitoringSchedule_lastModifiedTime,
    monitoringSchedule_monitoringType,
    monitoringSchedule_creationTime,
    monitoringSchedule_lastMonitoringExecutionSummary,
    monitoringSchedule_failureReason,

    -- ** MonitoringScheduleConfig
    monitoringScheduleConfig_scheduleConfig,
    monitoringScheduleConfig_monitoringJobDefinition,
    monitoringScheduleConfig_monitoringType,
    monitoringScheduleConfig_monitoringJobDefinitionName,

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

    -- ** NeoVpcConfig
    neoVpcConfig_securityGroupIds,
    neoVpcConfig_subnets,

    -- ** NestedFilters
    nestedFilters_nestedPropertyName,
    nestedFilters_filters,

    -- ** NetworkConfig
    networkConfig_vpcConfig,
    networkConfig_enableNetworkIsolation,
    networkConfig_enableInterContainerTrafficEncryption,

    -- ** NotebookInstanceLifecycleConfigSummary
    notebookInstanceLifecycleConfigSummary_lastModifiedTime,
    notebookInstanceLifecycleConfigSummary_creationTime,
    notebookInstanceLifecycleConfigSummary_notebookInstanceLifecycleConfigName,
    notebookInstanceLifecycleConfigSummary_notebookInstanceLifecycleConfigArn,

    -- ** NotebookInstanceLifecycleHook
    notebookInstanceLifecycleHook_content,

    -- ** NotebookInstanceSummary
    notebookInstanceSummary_notebookInstanceLifecycleConfigName,
    notebookInstanceSummary_notebookInstanceStatus,
    notebookInstanceSummary_url,
    notebookInstanceSummary_instanceType,
    notebookInstanceSummary_lastModifiedTime,
    notebookInstanceSummary_additionalCodeRepositories,
    notebookInstanceSummary_creationTime,
    notebookInstanceSummary_defaultCodeRepository,
    notebookInstanceSummary_notebookInstanceName,
    notebookInstanceSummary_notebookInstanceArn,

    -- ** NotificationConfiguration
    notificationConfiguration_notificationTopicArn,

    -- ** ObjectiveStatusCounters
    objectiveStatusCounters_failed,
    objectiveStatusCounters_succeeded,
    objectiveStatusCounters_pending,

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
    oidcConfigForResponse_issuer,
    oidcConfigForResponse_authorizationEndpoint,
    oidcConfigForResponse_userInfoEndpoint,
    oidcConfigForResponse_clientId,
    oidcConfigForResponse_logoutEndpoint,
    oidcConfigForResponse_jwksUri,
    oidcConfigForResponse_tokenEndpoint,

    -- ** OidcMemberDefinition
    oidcMemberDefinition_groups,

    -- ** OnlineStoreConfig
    onlineStoreConfig_enableOnlineStore,
    onlineStoreConfig_securityConfig,

    -- ** OnlineStoreSecurityConfig
    onlineStoreSecurityConfig_kmsKeyId,

    -- ** OutputConfig
    outputConfig_targetDevice,
    outputConfig_targetPlatform,
    outputConfig_kmsKeyId,
    outputConfig_compilerOptions,
    outputConfig_s3OutputLocation,

    -- ** OutputDataConfig
    outputDataConfig_kmsKeyId,
    outputDataConfig_s3OutputPath,

    -- ** OutputParameter
    outputParameter_name,
    outputParameter_value,

    -- ** ParallelismConfiguration
    parallelismConfiguration_maxParallelExecutionSteps,

    -- ** Parameter
    parameter_name,
    parameter_value,

    -- ** ParameterRange
    parameterRange_continuousParameterRangeSpecification,
    parameterRange_integerParameterRangeSpecification,
    parameterRange_categoricalParameterRangeSpecification,

    -- ** ParameterRanges
    parameterRanges_categoricalParameterRanges,
    parameterRanges_integerParameterRanges,
    parameterRanges_continuousParameterRanges,

    -- ** Parent
    parent_trialName,
    parent_experimentName,

    -- ** ParentHyperParameterTuningJob
    parentHyperParameterTuningJob_hyperParameterTuningJobName,

    -- ** PendingDeploymentSummary
    pendingDeploymentSummary_productionVariants,
    pendingDeploymentSummary_startTime,
    pendingDeploymentSummary_endpointConfigName,

    -- ** PendingProductionVariantSummary
    pendingProductionVariantSummary_desiredServerlessConfig,
    pendingProductionVariantSummary_desiredWeight,
    pendingProductionVariantSummary_acceleratorType,
    pendingProductionVariantSummary_currentServerlessConfig,
    pendingProductionVariantSummary_variantStatus,
    pendingProductionVariantSummary_desiredInstanceCount,
    pendingProductionVariantSummary_instanceType,
    pendingProductionVariantSummary_currentWeight,
    pendingProductionVariantSummary_deployedImages,
    pendingProductionVariantSummary_currentInstanceCount,
    pendingProductionVariantSummary_variantName,

    -- ** Phase
    phase_spawnRate,
    phase_initialNumberOfUsers,
    phase_durationInSeconds,

    -- ** Pipeline
    pipeline_tags,
    pipeline_roleArn,
    pipeline_pipelineArn,
    pipeline_pipelineDisplayName,
    pipeline_pipelineDescription,
    pipeline_lastModifiedTime,
    pipeline_parallelismConfiguration,
    pipeline_pipelineName,
    pipeline_lastRunTime,
    pipeline_creationTime,
    pipeline_lastModifiedBy,
    pipeline_createdBy,
    pipeline_pipelineStatus,

    -- ** PipelineDefinitionS3Location
    pipelineDefinitionS3Location_versionId,
    pipelineDefinitionS3Location_bucket,
    pipelineDefinitionS3Location_objectKey,

    -- ** PipelineExecution
    pipelineExecution_pipelineArn,
    pipelineExecution_pipelineExperimentConfig,
    pipelineExecution_pipelineParameters,
    pipelineExecution_lastModifiedTime,
    pipelineExecution_parallelismConfiguration,
    pipelineExecution_pipelineExecutionDescription,
    pipelineExecution_creationTime,
    pipelineExecution_lastModifiedBy,
    pipelineExecution_createdBy,
    pipelineExecution_pipelineExecutionStatus,
    pipelineExecution_pipelineExecutionDisplayName,
    pipelineExecution_pipelineExecutionArn,
    pipelineExecution_failureReason,

    -- ** PipelineExecutionStep
    pipelineExecutionStep_attemptCount,
    pipelineExecutionStep_metadata,
    pipelineExecutionStep_endTime,
    pipelineExecutionStep_stepName,
    pipelineExecutionStep_stepDisplayName,
    pipelineExecutionStep_cacheHitResult,
    pipelineExecutionStep_stepStatus,
    pipelineExecutionStep_stepDescription,
    pipelineExecutionStep_startTime,
    pipelineExecutionStep_failureReason,

    -- ** PipelineExecutionStepMetadata
    pipelineExecutionStepMetadata_trainingJob,
    pipelineExecutionStepMetadata_model,
    pipelineExecutionStepMetadata_processingJob,
    pipelineExecutionStepMetadata_fail,
    pipelineExecutionStepMetadata_clarifyCheck,
    pipelineExecutionStepMetadata_registerModel,
    pipelineExecutionStepMetadata_emr,
    pipelineExecutionStepMetadata_condition,
    pipelineExecutionStepMetadata_lambda,
    pipelineExecutionStepMetadata_qualityCheck,
    pipelineExecutionStepMetadata_transformJob,
    pipelineExecutionStepMetadata_callback,
    pipelineExecutionStepMetadata_tuningJob,

    -- ** PipelineExecutionSummary
    pipelineExecutionSummary_pipelineExecutionDescription,
    pipelineExecutionSummary_pipelineExecutionFailureReason,
    pipelineExecutionSummary_pipelineExecutionStatus,
    pipelineExecutionSummary_pipelineExecutionDisplayName,
    pipelineExecutionSummary_pipelineExecutionArn,
    pipelineExecutionSummary_startTime,

    -- ** PipelineExperimentConfig
    pipelineExperimentConfig_trialName,
    pipelineExperimentConfig_experimentName,

    -- ** PipelineSummary
    pipelineSummary_roleArn,
    pipelineSummary_lastExecutionTime,
    pipelineSummary_pipelineArn,
    pipelineSummary_pipelineDisplayName,
    pipelineSummary_pipelineDescription,
    pipelineSummary_lastModifiedTime,
    pipelineSummary_pipelineName,
    pipelineSummary_creationTime,

    -- ** ProcessingClusterConfig
    processingClusterConfig_volumeKmsKeyId,
    processingClusterConfig_instanceCount,
    processingClusterConfig_instanceType,
    processingClusterConfig_volumeSizeInGB,

    -- ** ProcessingFeatureStoreOutput
    processingFeatureStoreOutput_featureGroupName,

    -- ** ProcessingInput
    processingInput_s3Input,
    processingInput_datasetDefinition,
    processingInput_appManaged,
    processingInput_inputName,

    -- ** ProcessingJob
    processingJob_tags,
    processingJob_processingJobName,
    processingJob_roleArn,
    processingJob_environment,
    processingJob_processingJobStatus,
    processingJob_monitoringScheduleArn,
    processingJob_networkConfig,
    processingJob_experimentConfig,
    processingJob_appSpecification,
    processingJob_autoMLJobArn,
    processingJob_processingInputs,
    processingJob_lastModifiedTime,
    processingJob_stoppingCondition,
    processingJob_processingJobArn,
    processingJob_processingStartTime,
    processingJob_creationTime,
    processingJob_processingEndTime,
    processingJob_processingResources,
    processingJob_trainingJobArn,
    processingJob_exitMessage,
    processingJob_failureReason,
    processingJob_processingOutputConfig,

    -- ** ProcessingJobStepMetadata
    processingJobStepMetadata_arn,

    -- ** ProcessingJobSummary
    processingJobSummary_lastModifiedTime,
    processingJobSummary_processingEndTime,
    processingJobSummary_exitMessage,
    processingJobSummary_failureReason,
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
    processingS3Input_s3InputMode,
    processingS3Input_s3DataDistributionType,
    processingS3Input_localPath,
    processingS3Input_s3CompressionType,
    processingS3Input_s3Uri,
    processingS3Input_s3DataType,

    -- ** ProcessingS3Output
    processingS3Output_s3Uri,
    processingS3Output_localPath,
    processingS3Output_s3UploadMode,

    -- ** ProcessingStoppingCondition
    processingStoppingCondition_maxRuntimeInSeconds,

    -- ** ProductionVariant
    productionVariant_acceleratorType,
    productionVariant_containerStartupHealthCheckTimeoutInSeconds,
    productionVariant_initialInstanceCount,
    productionVariant_instanceType,
    productionVariant_serverlessConfig,
    productionVariant_coreDumpConfig,
    productionVariant_initialVariantWeight,
    productionVariant_modelDataDownloadTimeoutInSeconds,
    productionVariant_volumeSizeInGB,
    productionVariant_variantName,
    productionVariant_modelName,

    -- ** ProductionVariantCoreDumpConfig
    productionVariantCoreDumpConfig_kmsKeyId,
    productionVariantCoreDumpConfig_destinationS3Uri,

    -- ** ProductionVariantServerlessConfig
    productionVariantServerlessConfig_memorySizeInMB,
    productionVariantServerlessConfig_maxConcurrency,

    -- ** ProductionVariantStatus
    productionVariantStatus_statusMessage,
    productionVariantStatus_startTime,
    productionVariantStatus_status,

    -- ** ProductionVariantSummary
    productionVariantSummary_desiredServerlessConfig,
    productionVariantSummary_desiredWeight,
    productionVariantSummary_currentServerlessConfig,
    productionVariantSummary_variantStatus,
    productionVariantSummary_desiredInstanceCount,
    productionVariantSummary_currentWeight,
    productionVariantSummary_deployedImages,
    productionVariantSummary_currentInstanceCount,
    productionVariantSummary_variantName,

    -- ** ProfilerConfig
    profilerConfig_profilingIntervalInMilliseconds,
    profilerConfig_s3OutputPath,
    profilerConfig_profilingParameters,
    profilerConfig_disableProfiler,

    -- ** ProfilerConfigForUpdate
    profilerConfigForUpdate_profilingIntervalInMilliseconds,
    profilerConfigForUpdate_s3OutputPath,
    profilerConfigForUpdate_profilingParameters,
    profilerConfigForUpdate_disableProfiler,

    -- ** ProfilerRuleConfiguration
    profilerRuleConfiguration_s3OutputPath,
    profilerRuleConfiguration_instanceType,
    profilerRuleConfiguration_ruleParameters,
    profilerRuleConfiguration_localPath,
    profilerRuleConfiguration_volumeSizeInGB,
    profilerRuleConfiguration_ruleConfigurationName,
    profilerRuleConfiguration_ruleEvaluatorImage,

    -- ** ProfilerRuleEvaluationStatus
    profilerRuleEvaluationStatus_statusDetails,
    profilerRuleEvaluationStatus_lastModifiedTime,
    profilerRuleEvaluationStatus_ruleEvaluationJobArn,
    profilerRuleEvaluationStatus_ruleConfigurationName,
    profilerRuleEvaluationStatus_ruleEvaluationStatus,

    -- ** Project
    project_tags,
    project_serviceCatalogProvisionedProductDetails,
    project_projectId,
    project_lastModifiedTime,
    project_projectDescription,
    project_projectStatus,
    project_creationTime,
    project_lastModifiedBy,
    project_projectName,
    project_createdBy,
    project_projectArn,
    project_serviceCatalogProvisioningDetails,

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

    -- ** QualityCheckStepMetadata
    qualityCheckStepMetadata_modelPackageGroupName,
    qualityCheckStepMetadata_baselineUsedForDriftCheckStatistics,
    qualityCheckStepMetadata_checkJobArn,
    qualityCheckStepMetadata_checkType,
    qualityCheckStepMetadata_registerNewBaseline,
    qualityCheckStepMetadata_skipCheck,
    qualityCheckStepMetadata_baselineUsedForDriftCheckConstraints,
    qualityCheckStepMetadata_calculatedBaselineStatistics,
    qualityCheckStepMetadata_calculatedBaselineConstraints,
    qualityCheckStepMetadata_violationReport,

    -- ** QueryFilters
    queryFilters_modifiedAfter,
    queryFilters_properties,
    queryFilters_createdBefore,
    queryFilters_types,
    queryFilters_lineageTypes,
    queryFilters_createdAfter,
    queryFilters_modifiedBefore,

    -- ** RSessionAppSettings
    rSessionAppSettings_defaultResourceSpec,
    rSessionAppSettings_customImages,

    -- ** RStudioServerProAppSettings
    rStudioServerProAppSettings_accessStatus,
    rStudioServerProAppSettings_userGroup,

    -- ** RStudioServerProDomainSettings
    rStudioServerProDomainSettings_defaultResourceSpec,
    rStudioServerProDomainSettings_rStudioConnectUrl,
    rStudioServerProDomainSettings_rStudioPackageManagerUrl,
    rStudioServerProDomainSettings_domainExecutionRoleArn,

    -- ** RStudioServerProDomainSettingsForUpdate
    rStudioServerProDomainSettingsForUpdate_defaultResourceSpec,
    rStudioServerProDomainSettingsForUpdate_domainExecutionRoleArn,

    -- ** RecommendationJobCompiledOutputConfig
    recommendationJobCompiledOutputConfig_s3OutputUri,

    -- ** RecommendationJobContainerConfig
    recommendationJobContainerConfig_task,
    recommendationJobContainerConfig_supportedInstanceTypes,
    recommendationJobContainerConfig_domain,
    recommendationJobContainerConfig_nearestModelName,
    recommendationJobContainerConfig_frameworkVersion,
    recommendationJobContainerConfig_payloadConfig,
    recommendationJobContainerConfig_framework,

    -- ** RecommendationJobInferenceBenchmark
    recommendationJobInferenceBenchmark_metrics,
    recommendationJobInferenceBenchmark_endpointConfiguration,
    recommendationJobInferenceBenchmark_failureReason,
    recommendationJobInferenceBenchmark_modelConfiguration,

    -- ** RecommendationJobInputConfig
    recommendationJobInputConfig_trafficPattern,
    recommendationJobInputConfig_jobDurationInSeconds,
    recommendationJobInputConfig_volumeKmsKeyId,
    recommendationJobInputConfig_endpoints,
    recommendationJobInputConfig_endpointConfigurations,
    recommendationJobInputConfig_resourceLimit,
    recommendationJobInputConfig_containerConfig,
    recommendationJobInputConfig_modelPackageVersionArn,

    -- ** RecommendationJobOutputConfig
    recommendationJobOutputConfig_compiledOutputConfig,
    recommendationJobOutputConfig_kmsKeyId,

    -- ** RecommendationJobPayloadConfig
    recommendationJobPayloadConfig_samplePayloadUrl,
    recommendationJobPayloadConfig_supportedContentTypes,

    -- ** RecommendationJobResourceLimit
    recommendationJobResourceLimit_maxNumberOfTests,
    recommendationJobResourceLimit_maxParallelOfTests,

    -- ** RecommendationJobStoppingConditions
    recommendationJobStoppingConditions_maxInvocations,
    recommendationJobStoppingConditions_modelLatencyThresholds,

    -- ** RecommendationMetrics
    recommendationMetrics_costPerHour,
    recommendationMetrics_costPerInference,
    recommendationMetrics_maxInvocations,
    recommendationMetrics_modelLatency,

    -- ** RedshiftDatasetDefinition
    redshiftDatasetDefinition_kmsKeyId,
    redshiftDatasetDefinition_outputCompression,
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

    -- ** RepositoryAuthConfig
    repositoryAuthConfig_repositoryCredentialsProviderArn,

    -- ** ResolvedAttributes
    resolvedAttributes_completionCriteria,
    resolvedAttributes_autoMLJobObjective,
    resolvedAttributes_problemType,

    -- ** ResourceConfig
    resourceConfig_keepAlivePeriodInSeconds,
    resourceConfig_volumeKmsKeyId,
    resourceConfig_instanceType,
    resourceConfig_instanceCount,
    resourceConfig_instanceGroups,
    resourceConfig_volumeSizeInGB,

    -- ** ResourceConfigForUpdate
    resourceConfigForUpdate_keepAlivePeriodInSeconds,

    -- ** ResourceLimits
    resourceLimits_maxNumberOfTrainingJobs,
    resourceLimits_maxParallelTrainingJobs,

    -- ** ResourceSpec
    resourceSpec_lifecycleConfigArn,
    resourceSpec_sageMakerImageVersionArn,
    resourceSpec_instanceType,
    resourceSpec_sageMakerImageArn,

    -- ** RetentionPolicy
    retentionPolicy_homeEfsFileSystem,

    -- ** RetryStrategy
    retryStrategy_maximumRetryAttempts,

    -- ** S3DataSource
    s3DataSource_attributeNames,
    s3DataSource_s3DataDistributionType,
    s3DataSource_instanceGroupNames,
    s3DataSource_s3DataType,
    s3DataSource_s3Uri,

    -- ** S3StorageConfig
    s3StorageConfig_resolvedOutputS3Uri,
    s3StorageConfig_kmsKeyId,
    s3StorageConfig_s3Uri,

    -- ** ScheduleConfig
    scheduleConfig_scheduleExpression,

    -- ** SearchExpression
    searchExpression_filters,
    searchExpression_subExpressions,
    searchExpression_operator,
    searchExpression_nestedFilters,

    -- ** SearchRecord
    searchRecord_modelPackageGroup,
    searchRecord_trainingJob,
    searchRecord_project,
    searchRecord_trialComponent,
    searchRecord_experiment,
    searchRecord_hyperParameterTuningJob,
    searchRecord_endpoint,
    searchRecord_featureMetadata,
    searchRecord_trial,
    searchRecord_modelPackage,
    searchRecord_pipeline,
    searchRecord_featureGroup,
    searchRecord_pipelineExecution,

    -- ** SecondaryStatusTransition
    secondaryStatusTransition_endTime,
    secondaryStatusTransition_statusMessage,
    secondaryStatusTransition_status,
    secondaryStatusTransition_startTime,

    -- ** ServiceCatalogProvisionedProductDetails
    serviceCatalogProvisionedProductDetails_provisionedProductStatusMessage,
    serviceCatalogProvisionedProductDetails_provisionedProductId,

    -- ** ServiceCatalogProvisioningDetails
    serviceCatalogProvisioningDetails_pathId,
    serviceCatalogProvisioningDetails_provisioningParameters,
    serviceCatalogProvisioningDetails_provisioningArtifactId,
    serviceCatalogProvisioningDetails_productId,

    -- ** ServiceCatalogProvisioningUpdateDetails
    serviceCatalogProvisioningUpdateDetails_provisioningParameters,
    serviceCatalogProvisioningUpdateDetails_provisioningArtifactId,

    -- ** SharingSettings
    sharingSettings_s3OutputPath,
    sharingSettings_notebookOutputOption,
    sharingSettings_s3KmsKeyId,

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
    stoppingCondition_maxWaitTimeInSeconds,
    stoppingCondition_maxRuntimeInSeconds,

    -- ** StudioLifecycleConfigDetails
    studioLifecycleConfigDetails_studioLifecycleConfigName,
    studioLifecycleConfigDetails_studioLifecycleConfigArn,
    studioLifecycleConfigDetails_studioLifecycleConfigAppType,
    studioLifecycleConfigDetails_lastModifiedTime,
    studioLifecycleConfigDetails_creationTime,

    -- ** SubscribedWorkteam
    subscribedWorkteam_listingId,
    subscribedWorkteam_marketplaceDescription,
    subscribedWorkteam_marketplaceTitle,
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

    -- ** TimeSeriesForecastingSettings
    timeSeriesForecastingSettings_status,
    timeSeriesForecastingSettings_amazonForecastRoleArn,

    -- ** TrafficPattern
    trafficPattern_trafficType,
    trafficPattern_phases,

    -- ** TrafficRoutingConfig
    trafficRoutingConfig_linearStepSize,
    trafficRoutingConfig_canarySize,
    trafficRoutingConfig_type,
    trafficRoutingConfig_waitIntervalInSeconds,

    -- ** TrainingJob
    trainingJob_tags,
    trainingJob_outputDataConfig,
    trainingJob_enableManagedSpotTraining,
    trainingJob_roleArn,
    trainingJob_environment,
    trainingJob_trainingTimeInSeconds,
    trainingJob_debugRuleEvaluationStatuses,
    trainingJob_retryStrategy,
    trainingJob_vpcConfig,
    trainingJob_secondaryStatusTransitions,
    trainingJob_enableNetworkIsolation,
    trainingJob_resourceConfig,
    trainingJob_experimentConfig,
    trainingJob_tuningJobArn,
    trainingJob_trainingStartTime,
    trainingJob_checkpointConfig,
    trainingJob_autoMLJobArn,
    trainingJob_debugHookConfig,
    trainingJob_lastModifiedTime,
    trainingJob_secondaryStatus,
    trainingJob_enableInterContainerTrafficEncryption,
    trainingJob_finalMetricDataList,
    trainingJob_trainingJobStatus,
    trainingJob_modelArtifacts,
    trainingJob_stoppingCondition,
    trainingJob_algorithmSpecification,
    trainingJob_debugRuleConfigurations,
    trainingJob_labelingJobArn,
    trainingJob_creationTime,
    trainingJob_trainingJobName,
    trainingJob_tensorBoardOutputConfig,
    trainingJob_billableTimeInSeconds,
    trainingJob_trainingJobArn,
    trainingJob_inputDataConfig,
    trainingJob_hyperParameters,
    trainingJob_failureReason,
    trainingJob_trainingEndTime,

    -- ** TrainingJobDefinition
    trainingJobDefinition_hyperParameters,
    trainingJobDefinition_trainingInputMode,
    trainingJobDefinition_inputDataConfig,
    trainingJobDefinition_outputDataConfig,
    trainingJobDefinition_resourceConfig,
    trainingJobDefinition_stoppingCondition,

    -- ** TrainingJobStatusCounters
    trainingJobStatusCounters_retryableError,
    trainingJobStatusCounters_completed,
    trainingJobStatusCounters_stopped,
    trainingJobStatusCounters_nonRetryableError,
    trainingJobStatusCounters_inProgress,

    -- ** TrainingJobStepMetadata
    trainingJobStepMetadata_arn,

    -- ** TrainingJobSummary
    trainingJobSummary_warmPoolStatus,
    trainingJobSummary_lastModifiedTime,
    trainingJobSummary_trainingEndTime,
    trainingJobSummary_trainingJobName,
    trainingJobSummary_trainingJobArn,
    trainingJobSummary_creationTime,
    trainingJobSummary_trainingJobStatus,

    -- ** TrainingSpecification
    trainingSpecification_supportsDistributedTraining,
    trainingSpecification_supportedTuningJobObjectiveMetrics,
    trainingSpecification_supportedHyperParameters,
    trainingSpecification_metricDefinitions,
    trainingSpecification_trainingImageDigest,
    trainingSpecification_trainingImage,
    trainingSpecification_supportedTrainingInstanceTypes,
    trainingSpecification_trainingChannels,

    -- ** TransformDataSource
    transformDataSource_s3DataSource,

    -- ** TransformInput
    transformInput_splitType,
    transformInput_compressionType,
    transformInput_contentType,
    transformInput_dataSource,

    -- ** TransformJob
    transformJob_tags,
    transformJob_maxConcurrentTransforms,
    transformJob_transformJobArn,
    transformJob_modelClientConfig,
    transformJob_environment,
    transformJob_transformOutput,
    transformJob_experimentConfig,
    transformJob_transformInput,
    transformJob_transformJobName,
    transformJob_autoMLJobArn,
    transformJob_transformJobStatus,
    transformJob_transformEndTime,
    transformJob_maxPayloadInMB,
    transformJob_batchStrategy,
    transformJob_labelingJobArn,
    transformJob_modelName,
    transformJob_transformResources,
    transformJob_creationTime,
    transformJob_dataProcessing,
    transformJob_transformStartTime,
    transformJob_failureReason,

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
    transformJobSummary_transformEndTime,
    transformJobSummary_lastModifiedTime,
    transformJobSummary_failureReason,
    transformJobSummary_transformJobName,
    transformJobSummary_transformJobArn,
    transformJobSummary_creationTime,
    transformJobSummary_transformJobStatus,

    -- ** TransformOutput
    transformOutput_assembleWith,
    transformOutput_accept,
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
    trial_tags,
    trial_metadataProperties,
    trial_displayName,
    trial_trialComponentSummaries,
    trial_lastModifiedTime,
    trial_source,
    trial_trialName,
    trial_creationTime,
    trial_lastModifiedBy,
    trial_createdBy,
    trial_trialArn,
    trial_experimentName,

    -- ** TrialComponent
    trialComponent_tags,
    trialComponent_trialComponentArn,
    trialComponent_trialComponentName,
    trialComponent_sourceDetail,
    trialComponent_metadataProperties,
    trialComponent_displayName,
    trialComponent_status,
    trialComponent_metrics,
    trialComponent_outputArtifacts,
    trialComponent_endTime,
    trialComponent_lastModifiedTime,
    trialComponent_source,
    trialComponent_parents,
    trialComponent_lineageGroupArn,
    trialComponent_creationTime,
    trialComponent_inputArtifacts,
    trialComponent_lastModifiedBy,
    trialComponent_createdBy,
    trialComponent_startTime,
    trialComponent_parameters,

    -- ** TrialComponentArtifact
    trialComponentArtifact_mediaType,
    trialComponentArtifact_value,

    -- ** TrialComponentMetricSummary
    trialComponentMetricSummary_sourceArn,
    trialComponentMetricSummary_max,
    trialComponentMetricSummary_timeStamp,
    trialComponentMetricSummary_avg,
    trialComponentMetricSummary_count,
    trialComponentMetricSummary_last,
    trialComponentMetricSummary_min,
    trialComponentMetricSummary_metricName,
    trialComponentMetricSummary_stdDev,

    -- ** TrialComponentParameterValue
    trialComponentParameterValue_numberValue,
    trialComponentParameterValue_stringValue,

    -- ** TrialComponentSimpleSummary
    trialComponentSimpleSummary_trialComponentArn,
    trialComponentSimpleSummary_trialComponentName,
    trialComponentSimpleSummary_trialComponentSource,
    trialComponentSimpleSummary_creationTime,
    trialComponentSimpleSummary_createdBy,

    -- ** TrialComponentSource
    trialComponentSource_sourceType,
    trialComponentSource_sourceArn,

    -- ** TrialComponentSourceDetail
    trialComponentSourceDetail_trainingJob,
    trialComponentSourceDetail_sourceArn,
    trialComponentSourceDetail_processingJob,
    trialComponentSourceDetail_transformJob,

    -- ** TrialComponentStatus
    trialComponentStatus_message,
    trialComponentStatus_primaryStatus,

    -- ** TrialComponentSummary
    trialComponentSummary_trialComponentArn,
    trialComponentSummary_trialComponentName,
    trialComponentSummary_displayName,
    trialComponentSummary_status,
    trialComponentSummary_endTime,
    trialComponentSummary_lastModifiedTime,
    trialComponentSummary_trialComponentSource,
    trialComponentSummary_creationTime,
    trialComponentSummary_lastModifiedBy,
    trialComponentSummary_createdBy,
    trialComponentSummary_startTime,

    -- ** TrialSource
    trialSource_sourceType,
    trialSource_sourceArn,

    -- ** TrialSummary
    trialSummary_displayName,
    trialSummary_lastModifiedTime,
    trialSummary_trialName,
    trialSummary_creationTime,
    trialSummary_trialSource,
    trialSummary_trialArn,

    -- ** TuningJobCompletionCriteria
    tuningJobCompletionCriteria_targetObjectiveMetricValue,

    -- ** TuningJobStepMetaData
    tuningJobStepMetaData_arn,

    -- ** USD
    usd_tenthFractionsOfACent,
    usd_dollars,
    usd_cents,

    -- ** UiConfig
    uiConfig_humanTaskUiArn,
    uiConfig_uiTemplateS3Uri,

    -- ** UiTemplate
    uiTemplate_content,

    -- ** UiTemplateInfo
    uiTemplateInfo_url,
    uiTemplateInfo_contentSha256,

    -- ** UserContext
    userContext_userProfileArn,
    userContext_userProfileName,
    userContext_domainId,

    -- ** UserProfileDetails
    userProfileDetails_status,
    userProfileDetails_lastModifiedTime,
    userProfileDetails_userProfileName,
    userProfileDetails_creationTime,
    userProfileDetails_domainId,

    -- ** UserSettings
    userSettings_executionRole,
    userSettings_rSessionAppSettings,
    userSettings_tensorBoardAppSettings,
    userSettings_kernelGatewayAppSettings,
    userSettings_securityGroups,
    userSettings_canvasAppSettings,
    userSettings_jupyterServerAppSettings,
    userSettings_rStudioServerProAppSettings,
    userSettings_sharingSettings,

    -- ** VariantProperty
    variantProperty_variantPropertyType,

    -- ** Vertex
    vertex_type,
    vertex_arn,
    vertex_lineageType,

    -- ** VpcConfig
    vpcConfig_securityGroupIds,
    vpcConfig_subnets,

    -- ** WarmPoolStatus
    warmPoolStatus_reusedByJob,
    warmPoolStatus_resourceRetainedBillableTimeInSeconds,
    warmPoolStatus_status,

    -- ** Workforce
    workforce_cognitoConfig,
    workforce_lastUpdatedDate,
    workforce_subDomain,
    workforce_status,
    workforce_sourceIpConfig,
    workforce_workforceVpcConfig,
    workforce_createDate,
    workforce_oidcConfig,
    workforce_failureReason,
    workforce_workforceName,
    workforce_workforceArn,

    -- ** WorkforceVpcConfigRequest
    workforceVpcConfigRequest_securityGroupIds,
    workforceVpcConfigRequest_subnets,
    workforceVpcConfigRequest_vpcId,

    -- ** WorkforceVpcConfigResponse
    workforceVpcConfigResponse_vpcEndpointId,
    workforceVpcConfigResponse_vpcId,
    workforceVpcConfigResponse_securityGroupIds,
    workforceVpcConfigResponse_subnets,

    -- ** Workteam
    workteam_lastUpdatedDate,
    workteam_subDomain,
    workteam_notificationConfiguration,
    workteam_workforceArn,
    workteam_createDate,
    workteam_productListingIds,
    workteam_workteamName,
    workteam_memberDefinitions,
    workteam_workteamArn,
    workteam_description,
  )
where

import Amazonka.SageMaker.AddAssociation
import Amazonka.SageMaker.AddTags
import Amazonka.SageMaker.AssociateTrialComponent
import Amazonka.SageMaker.BatchDescribeModelPackage
import Amazonka.SageMaker.CreateAction
import Amazonka.SageMaker.CreateAlgorithm
import Amazonka.SageMaker.CreateApp
import Amazonka.SageMaker.CreateAppImageConfig
import Amazonka.SageMaker.CreateArtifact
import Amazonka.SageMaker.CreateAutoMLJob
import Amazonka.SageMaker.CreateCodeRepository
import Amazonka.SageMaker.CreateCompilationJob
import Amazonka.SageMaker.CreateContext
import Amazonka.SageMaker.CreateDataQualityJobDefinition
import Amazonka.SageMaker.CreateDeviceFleet
import Amazonka.SageMaker.CreateDomain
import Amazonka.SageMaker.CreateEdgeDeploymentPlan
import Amazonka.SageMaker.CreateEdgeDeploymentStage
import Amazonka.SageMaker.CreateEdgePackagingJob
import Amazonka.SageMaker.CreateEndpoint
import Amazonka.SageMaker.CreateEndpointConfig
import Amazonka.SageMaker.CreateExperiment
import Amazonka.SageMaker.CreateFeatureGroup
import Amazonka.SageMaker.CreateFlowDefinition
import Amazonka.SageMaker.CreateHumanTaskUi
import Amazonka.SageMaker.CreateHyperParameterTuningJob
import Amazonka.SageMaker.CreateImage
import Amazonka.SageMaker.CreateImageVersion
import Amazonka.SageMaker.CreateInferenceRecommendationsJob
import Amazonka.SageMaker.CreateLabelingJob
import Amazonka.SageMaker.CreateModel
import Amazonka.SageMaker.CreateModelBiasJobDefinition
import Amazonka.SageMaker.CreateModelExplainabilityJobDefinition
import Amazonka.SageMaker.CreateModelPackage
import Amazonka.SageMaker.CreateModelPackageGroup
import Amazonka.SageMaker.CreateModelQualityJobDefinition
import Amazonka.SageMaker.CreateMonitoringSchedule
import Amazonka.SageMaker.CreateNotebookInstance
import Amazonka.SageMaker.CreateNotebookInstanceLifecycleConfig
import Amazonka.SageMaker.CreatePipeline
import Amazonka.SageMaker.CreatePresignedDomainUrl
import Amazonka.SageMaker.CreatePresignedNotebookInstanceUrl
import Amazonka.SageMaker.CreateProcessingJob
import Amazonka.SageMaker.CreateProject
import Amazonka.SageMaker.CreateStudioLifecycleConfig
import Amazonka.SageMaker.CreateTrainingJob
import Amazonka.SageMaker.CreateTransformJob
import Amazonka.SageMaker.CreateTrial
import Amazonka.SageMaker.CreateTrialComponent
import Amazonka.SageMaker.CreateUserProfile
import Amazonka.SageMaker.CreateWorkforce
import Amazonka.SageMaker.CreateWorkteam
import Amazonka.SageMaker.DeleteAction
import Amazonka.SageMaker.DeleteAlgorithm
import Amazonka.SageMaker.DeleteApp
import Amazonka.SageMaker.DeleteAppImageConfig
import Amazonka.SageMaker.DeleteArtifact
import Amazonka.SageMaker.DeleteAssociation
import Amazonka.SageMaker.DeleteCodeRepository
import Amazonka.SageMaker.DeleteContext
import Amazonka.SageMaker.DeleteDataQualityJobDefinition
import Amazonka.SageMaker.DeleteDeviceFleet
import Amazonka.SageMaker.DeleteDomain
import Amazonka.SageMaker.DeleteEdgeDeploymentPlan
import Amazonka.SageMaker.DeleteEdgeDeploymentStage
import Amazonka.SageMaker.DeleteEndpoint
import Amazonka.SageMaker.DeleteEndpointConfig
import Amazonka.SageMaker.DeleteExperiment
import Amazonka.SageMaker.DeleteFeatureGroup
import Amazonka.SageMaker.DeleteFlowDefinition
import Amazonka.SageMaker.DeleteHumanTaskUi
import Amazonka.SageMaker.DeleteImage
import Amazonka.SageMaker.DeleteImageVersion
import Amazonka.SageMaker.DeleteModel
import Amazonka.SageMaker.DeleteModelBiasJobDefinition
import Amazonka.SageMaker.DeleteModelExplainabilityJobDefinition
import Amazonka.SageMaker.DeleteModelPackage
import Amazonka.SageMaker.DeleteModelPackageGroup
import Amazonka.SageMaker.DeleteModelPackageGroupPolicy
import Amazonka.SageMaker.DeleteModelQualityJobDefinition
import Amazonka.SageMaker.DeleteMonitoringSchedule
import Amazonka.SageMaker.DeleteNotebookInstance
import Amazonka.SageMaker.DeleteNotebookInstanceLifecycleConfig
import Amazonka.SageMaker.DeletePipeline
import Amazonka.SageMaker.DeleteProject
import Amazonka.SageMaker.DeleteStudioLifecycleConfig
import Amazonka.SageMaker.DeleteTags
import Amazonka.SageMaker.DeleteTrial
import Amazonka.SageMaker.DeleteTrialComponent
import Amazonka.SageMaker.DeleteUserProfile
import Amazonka.SageMaker.DeleteWorkforce
import Amazonka.SageMaker.DeleteWorkteam
import Amazonka.SageMaker.DeregisterDevices
import Amazonka.SageMaker.DescribeAction
import Amazonka.SageMaker.DescribeAlgorithm
import Amazonka.SageMaker.DescribeApp
import Amazonka.SageMaker.DescribeAppImageConfig
import Amazonka.SageMaker.DescribeArtifact
import Amazonka.SageMaker.DescribeAutoMLJob
import Amazonka.SageMaker.DescribeCodeRepository
import Amazonka.SageMaker.DescribeCompilationJob
import Amazonka.SageMaker.DescribeContext
import Amazonka.SageMaker.DescribeDataQualityJobDefinition
import Amazonka.SageMaker.DescribeDevice
import Amazonka.SageMaker.DescribeDeviceFleet
import Amazonka.SageMaker.DescribeDomain
import Amazonka.SageMaker.DescribeEdgeDeploymentPlan
import Amazonka.SageMaker.DescribeEdgePackagingJob
import Amazonka.SageMaker.DescribeEndpoint
import Amazonka.SageMaker.DescribeEndpointConfig
import Amazonka.SageMaker.DescribeExperiment
import Amazonka.SageMaker.DescribeFeatureGroup
import Amazonka.SageMaker.DescribeFeatureMetadata
import Amazonka.SageMaker.DescribeFlowDefinition
import Amazonka.SageMaker.DescribeHumanTaskUi
import Amazonka.SageMaker.DescribeHyperParameterTuningJob
import Amazonka.SageMaker.DescribeImage
import Amazonka.SageMaker.DescribeImageVersion
import Amazonka.SageMaker.DescribeInferenceRecommendationsJob
import Amazonka.SageMaker.DescribeLabelingJob
import Amazonka.SageMaker.DescribeLineageGroup
import Amazonka.SageMaker.DescribeModel
import Amazonka.SageMaker.DescribeModelBiasJobDefinition
import Amazonka.SageMaker.DescribeModelExplainabilityJobDefinition
import Amazonka.SageMaker.DescribeModelPackage
import Amazonka.SageMaker.DescribeModelPackageGroup
import Amazonka.SageMaker.DescribeModelQualityJobDefinition
import Amazonka.SageMaker.DescribeMonitoringSchedule
import Amazonka.SageMaker.DescribeNotebookInstance
import Amazonka.SageMaker.DescribeNotebookInstanceLifecycleConfig
import Amazonka.SageMaker.DescribePipeline
import Amazonka.SageMaker.DescribePipelineDefinitionForExecution
import Amazonka.SageMaker.DescribePipelineExecution
import Amazonka.SageMaker.DescribeProcessingJob
import Amazonka.SageMaker.DescribeProject
import Amazonka.SageMaker.DescribeStudioLifecycleConfig
import Amazonka.SageMaker.DescribeSubscribedWorkteam
import Amazonka.SageMaker.DescribeTrainingJob
import Amazonka.SageMaker.DescribeTransformJob
import Amazonka.SageMaker.DescribeTrial
import Amazonka.SageMaker.DescribeTrialComponent
import Amazonka.SageMaker.DescribeUserProfile
import Amazonka.SageMaker.DescribeWorkforce
import Amazonka.SageMaker.DescribeWorkteam
import Amazonka.SageMaker.DisableSagemakerServicecatalogPortfolio
import Amazonka.SageMaker.DisassociateTrialComponent
import Amazonka.SageMaker.EnableSagemakerServicecatalogPortfolio
import Amazonka.SageMaker.GetDeviceFleetReport
import Amazonka.SageMaker.GetLineageGroupPolicy
import Amazonka.SageMaker.GetModelPackageGroupPolicy
import Amazonka.SageMaker.GetSagemakerServicecatalogPortfolioStatus
import Amazonka.SageMaker.GetSearchSuggestions
import Amazonka.SageMaker.ListActions
import Amazonka.SageMaker.ListAlgorithms
import Amazonka.SageMaker.ListAppImageConfigs
import Amazonka.SageMaker.ListApps
import Amazonka.SageMaker.ListArtifacts
import Amazonka.SageMaker.ListAssociations
import Amazonka.SageMaker.ListAutoMLJobs
import Amazonka.SageMaker.ListCandidatesForAutoMLJob
import Amazonka.SageMaker.ListCodeRepositories
import Amazonka.SageMaker.ListCompilationJobs
import Amazonka.SageMaker.ListContexts
import Amazonka.SageMaker.ListDataQualityJobDefinitions
import Amazonka.SageMaker.ListDeviceFleets
import Amazonka.SageMaker.ListDevices
import Amazonka.SageMaker.ListDomains
import Amazonka.SageMaker.ListEdgeDeploymentPlans
import Amazonka.SageMaker.ListEdgePackagingJobs
import Amazonka.SageMaker.ListEndpointConfigs
import Amazonka.SageMaker.ListEndpoints
import Amazonka.SageMaker.ListExperiments
import Amazonka.SageMaker.ListFeatureGroups
import Amazonka.SageMaker.ListFlowDefinitions
import Amazonka.SageMaker.ListHumanTaskUis
import Amazonka.SageMaker.ListHyperParameterTuningJobs
import Amazonka.SageMaker.ListImageVersions
import Amazonka.SageMaker.ListImages
import Amazonka.SageMaker.ListInferenceRecommendationsJobSteps
import Amazonka.SageMaker.ListInferenceRecommendationsJobs
import Amazonka.SageMaker.ListLabelingJobs
import Amazonka.SageMaker.ListLabelingJobsForWorkteam
import Amazonka.SageMaker.ListLineageGroups
import Amazonka.SageMaker.ListModelBiasJobDefinitions
import Amazonka.SageMaker.ListModelExplainabilityJobDefinitions
import Amazonka.SageMaker.ListModelMetadata
import Amazonka.SageMaker.ListModelPackageGroups
import Amazonka.SageMaker.ListModelPackages
import Amazonka.SageMaker.ListModelQualityJobDefinitions
import Amazonka.SageMaker.ListModels
import Amazonka.SageMaker.ListMonitoringExecutions
import Amazonka.SageMaker.ListMonitoringSchedules
import Amazonka.SageMaker.ListNotebookInstanceLifecycleConfigs
import Amazonka.SageMaker.ListNotebookInstances
import Amazonka.SageMaker.ListPipelineExecutionSteps
import Amazonka.SageMaker.ListPipelineExecutions
import Amazonka.SageMaker.ListPipelineParametersForExecution
import Amazonka.SageMaker.ListPipelines
import Amazonka.SageMaker.ListProcessingJobs
import Amazonka.SageMaker.ListProjects
import Amazonka.SageMaker.ListStageDevices
import Amazonka.SageMaker.ListStudioLifecycleConfigs
import Amazonka.SageMaker.ListSubscribedWorkteams
import Amazonka.SageMaker.ListTags
import Amazonka.SageMaker.ListTrainingJobs
import Amazonka.SageMaker.ListTrainingJobsForHyperParameterTuningJob
import Amazonka.SageMaker.ListTransformJobs
import Amazonka.SageMaker.ListTrialComponents
import Amazonka.SageMaker.ListTrials
import Amazonka.SageMaker.ListUserProfiles
import Amazonka.SageMaker.ListWorkforces
import Amazonka.SageMaker.ListWorkteams
import Amazonka.SageMaker.PutModelPackageGroupPolicy
import Amazonka.SageMaker.QueryLineage
import Amazonka.SageMaker.RegisterDevices
import Amazonka.SageMaker.RenderUiTemplate
import Amazonka.SageMaker.RetryPipelineExecution
import Amazonka.SageMaker.Search
import Amazonka.SageMaker.SendPipelineExecutionStepFailure
import Amazonka.SageMaker.SendPipelineExecutionStepSuccess
import Amazonka.SageMaker.StartEdgeDeploymentStage
import Amazonka.SageMaker.StartMonitoringSchedule
import Amazonka.SageMaker.StartNotebookInstance
import Amazonka.SageMaker.StartPipelineExecution
import Amazonka.SageMaker.StopAutoMLJob
import Amazonka.SageMaker.StopCompilationJob
import Amazonka.SageMaker.StopEdgeDeploymentStage
import Amazonka.SageMaker.StopEdgePackagingJob
import Amazonka.SageMaker.StopHyperParameterTuningJob
import Amazonka.SageMaker.StopInferenceRecommendationsJob
import Amazonka.SageMaker.StopLabelingJob
import Amazonka.SageMaker.StopMonitoringSchedule
import Amazonka.SageMaker.StopNotebookInstance
import Amazonka.SageMaker.StopPipelineExecution
import Amazonka.SageMaker.StopProcessingJob
import Amazonka.SageMaker.StopTrainingJob
import Amazonka.SageMaker.StopTransformJob
import Amazonka.SageMaker.Types.ActionSource
import Amazonka.SageMaker.Types.ActionSummary
import Amazonka.SageMaker.Types.AdditionalInferenceSpecificationDefinition
import Amazonka.SageMaker.Types.AgentVersion
import Amazonka.SageMaker.Types.Alarm
import Amazonka.SageMaker.Types.AlgorithmSpecification
import Amazonka.SageMaker.Types.AlgorithmStatusDetails
import Amazonka.SageMaker.Types.AlgorithmStatusItem
import Amazonka.SageMaker.Types.AlgorithmSummary
import Amazonka.SageMaker.Types.AlgorithmValidationProfile
import Amazonka.SageMaker.Types.AlgorithmValidationSpecification
import Amazonka.SageMaker.Types.AnnotationConsolidationConfig
import Amazonka.SageMaker.Types.AppDetails
import Amazonka.SageMaker.Types.AppImageConfigDetails
import Amazonka.SageMaker.Types.AppSpecification
import Amazonka.SageMaker.Types.ArtifactSource
import Amazonka.SageMaker.Types.ArtifactSourceType
import Amazonka.SageMaker.Types.ArtifactSummary
import Amazonka.SageMaker.Types.AssociationSummary
import Amazonka.SageMaker.Types.AsyncInferenceClientConfig
import Amazonka.SageMaker.Types.AsyncInferenceConfig
import Amazonka.SageMaker.Types.AsyncInferenceNotificationConfig
import Amazonka.SageMaker.Types.AsyncInferenceOutputConfig
import Amazonka.SageMaker.Types.AthenaDatasetDefinition
import Amazonka.SageMaker.Types.AutoMLCandidate
import Amazonka.SageMaker.Types.AutoMLCandidateGenerationConfig
import Amazonka.SageMaker.Types.AutoMLCandidateStep
import Amazonka.SageMaker.Types.AutoMLChannel
import Amazonka.SageMaker.Types.AutoMLContainerDefinition
import Amazonka.SageMaker.Types.AutoMLDataSource
import Amazonka.SageMaker.Types.AutoMLDataSplitConfig
import Amazonka.SageMaker.Types.AutoMLJobArtifacts
import Amazonka.SageMaker.Types.AutoMLJobCompletionCriteria
import Amazonka.SageMaker.Types.AutoMLJobConfig
import Amazonka.SageMaker.Types.AutoMLJobObjective
import Amazonka.SageMaker.Types.AutoMLJobSummary
import Amazonka.SageMaker.Types.AutoMLOutputDataConfig
import Amazonka.SageMaker.Types.AutoMLPartialFailureReason
import Amazonka.SageMaker.Types.AutoMLS3DataSource
import Amazonka.SageMaker.Types.AutoMLSecurityConfig
import Amazonka.SageMaker.Types.AutoRollbackConfig
import Amazonka.SageMaker.Types.BatchDataCaptureConfig
import Amazonka.SageMaker.Types.BatchDescribeModelPackageError
import Amazonka.SageMaker.Types.BatchDescribeModelPackageSummary
import Amazonka.SageMaker.Types.BatchTransformInput
import Amazonka.SageMaker.Types.Bias
import Amazonka.SageMaker.Types.BlueGreenUpdatePolicy
import Amazonka.SageMaker.Types.CacheHitResult
import Amazonka.SageMaker.Types.CallbackStepMetadata
import Amazonka.SageMaker.Types.CandidateArtifactLocations
import Amazonka.SageMaker.Types.CandidateProperties
import Amazonka.SageMaker.Types.CanvasAppSettings
import Amazonka.SageMaker.Types.CapacitySize
import Amazonka.SageMaker.Types.CaptureContentTypeHeader
import Amazonka.SageMaker.Types.CaptureOption
import Amazonka.SageMaker.Types.CategoricalParameter
import Amazonka.SageMaker.Types.CategoricalParameterRange
import Amazonka.SageMaker.Types.CategoricalParameterRangeSpecification
import Amazonka.SageMaker.Types.Channel
import Amazonka.SageMaker.Types.ChannelSpecification
import Amazonka.SageMaker.Types.CheckpointConfig
import Amazonka.SageMaker.Types.ClarifyCheckStepMetadata
import Amazonka.SageMaker.Types.ClarifyExplainerConfig
import Amazonka.SageMaker.Types.ClarifyInferenceConfig
import Amazonka.SageMaker.Types.ClarifyShapBaselineConfig
import Amazonka.SageMaker.Types.ClarifyShapConfig
import Amazonka.SageMaker.Types.ClarifyTextConfig
import Amazonka.SageMaker.Types.CodeRepositorySummary
import Amazonka.SageMaker.Types.CognitoConfig
import Amazonka.SageMaker.Types.CognitoMemberDefinition
import Amazonka.SageMaker.Types.CollectionConfiguration
import Amazonka.SageMaker.Types.CompilationJobSummary
import Amazonka.SageMaker.Types.ConditionStepMetadata
import Amazonka.SageMaker.Types.ContainerDefinition
import Amazonka.SageMaker.Types.ContextSource
import Amazonka.SageMaker.Types.ContextSummary
import Amazonka.SageMaker.Types.ContinuousParameterRange
import Amazonka.SageMaker.Types.ContinuousParameterRangeSpecification
import Amazonka.SageMaker.Types.CustomImage
import Amazonka.SageMaker.Types.DataCaptureConfig
import Amazonka.SageMaker.Types.DataCaptureConfigSummary
import Amazonka.SageMaker.Types.DataCatalogConfig
import Amazonka.SageMaker.Types.DataProcessing
import Amazonka.SageMaker.Types.DataQualityAppSpecification
import Amazonka.SageMaker.Types.DataQualityBaselineConfig
import Amazonka.SageMaker.Types.DataQualityJobInput
import Amazonka.SageMaker.Types.DataSource
import Amazonka.SageMaker.Types.DatasetDefinition
import Amazonka.SageMaker.Types.DebugHookConfig
import Amazonka.SageMaker.Types.DebugRuleConfiguration
import Amazonka.SageMaker.Types.DebugRuleEvaluationStatus
import Amazonka.SageMaker.Types.DeployedImage
import Amazonka.SageMaker.Types.DeploymentConfig
import Amazonka.SageMaker.Types.DeploymentStage
import Amazonka.SageMaker.Types.DeploymentStageStatusSummary
import Amazonka.SageMaker.Types.DesiredWeightAndCapacity
import Amazonka.SageMaker.Types.Device
import Amazonka.SageMaker.Types.DeviceDeploymentSummary
import Amazonka.SageMaker.Types.DeviceFleetSummary
import Amazonka.SageMaker.Types.DeviceSelectionConfig
import Amazonka.SageMaker.Types.DeviceStats
import Amazonka.SageMaker.Types.DeviceSummary
import Amazonka.SageMaker.Types.DomainDetails
import Amazonka.SageMaker.Types.DomainSettings
import Amazonka.SageMaker.Types.DomainSettingsForUpdate
import Amazonka.SageMaker.Types.DriftCheckBaselines
import Amazonka.SageMaker.Types.DriftCheckBias
import Amazonka.SageMaker.Types.DriftCheckExplainability
import Amazonka.SageMaker.Types.DriftCheckModelDataQuality
import Amazonka.SageMaker.Types.DriftCheckModelQuality
import Amazonka.SageMaker.Types.EMRStepMetadata
import Amazonka.SageMaker.Types.Edge
import Amazonka.SageMaker.Types.EdgeDeploymentConfig
import Amazonka.SageMaker.Types.EdgeDeploymentModelConfig
import Amazonka.SageMaker.Types.EdgeDeploymentPlanSummary
import Amazonka.SageMaker.Types.EdgeDeploymentStatus
import Amazonka.SageMaker.Types.EdgeModel
import Amazonka.SageMaker.Types.EdgeModelStat
import Amazonka.SageMaker.Types.EdgeModelSummary
import Amazonka.SageMaker.Types.EdgeOutputConfig
import Amazonka.SageMaker.Types.EdgePackagingJobSummary
import Amazonka.SageMaker.Types.EdgePresetDeploymentOutput
import Amazonka.SageMaker.Types.Endpoint
import Amazonka.SageMaker.Types.EndpointConfigSummary
import Amazonka.SageMaker.Types.EndpointInfo
import Amazonka.SageMaker.Types.EndpointInput
import Amazonka.SageMaker.Types.EndpointInputConfiguration
import Amazonka.SageMaker.Types.EndpointOutputConfiguration
import Amazonka.SageMaker.Types.EndpointPerformance
import Amazonka.SageMaker.Types.EndpointSummary
import Amazonka.SageMaker.Types.EnvironmentParameter
import Amazonka.SageMaker.Types.EnvironmentParameterRanges
import Amazonka.SageMaker.Types.Experiment
import Amazonka.SageMaker.Types.ExperimentConfig
import Amazonka.SageMaker.Types.ExperimentSource
import Amazonka.SageMaker.Types.ExperimentSummary
import Amazonka.SageMaker.Types.Explainability
import Amazonka.SageMaker.Types.ExplainerConfig
import Amazonka.SageMaker.Types.FailStepMetadata
import Amazonka.SageMaker.Types.FeatureDefinition
import Amazonka.SageMaker.Types.FeatureGroup
import Amazonka.SageMaker.Types.FeatureGroupSummary
import Amazonka.SageMaker.Types.FeatureMetadata
import Amazonka.SageMaker.Types.FeatureParameter
import Amazonka.SageMaker.Types.FileSource
import Amazonka.SageMaker.Types.FileSystemConfig
import Amazonka.SageMaker.Types.FileSystemDataSource
import Amazonka.SageMaker.Types.Filter
import Amazonka.SageMaker.Types.FinalAutoMLJobObjectiveMetric
import Amazonka.SageMaker.Types.FinalHyperParameterTuningJobObjectiveMetric
import Amazonka.SageMaker.Types.FlowDefinitionOutputConfig
import Amazonka.SageMaker.Types.FlowDefinitionSummary
import Amazonka.SageMaker.Types.GitConfig
import Amazonka.SageMaker.Types.GitConfigForUpdate
import Amazonka.SageMaker.Types.HumanLoopActivationConditionsConfig
import Amazonka.SageMaker.Types.HumanLoopActivationConfig
import Amazonka.SageMaker.Types.HumanLoopConfig
import Amazonka.SageMaker.Types.HumanLoopRequestSource
import Amazonka.SageMaker.Types.HumanTaskConfig
import Amazonka.SageMaker.Types.HumanTaskUiSummary
import Amazonka.SageMaker.Types.HyperParameterAlgorithmSpecification
import Amazonka.SageMaker.Types.HyperParameterSpecification
import Amazonka.SageMaker.Types.HyperParameterTrainingJobDefinition
import Amazonka.SageMaker.Types.HyperParameterTrainingJobSummary
import Amazonka.SageMaker.Types.HyperParameterTuningInstanceConfig
import Amazonka.SageMaker.Types.HyperParameterTuningJobConfig
import Amazonka.SageMaker.Types.HyperParameterTuningJobObjective
import Amazonka.SageMaker.Types.HyperParameterTuningJobSearchEntity
import Amazonka.SageMaker.Types.HyperParameterTuningJobStrategyConfig
import Amazonka.SageMaker.Types.HyperParameterTuningJobSummary
import Amazonka.SageMaker.Types.HyperParameterTuningJobWarmStartConfig
import Amazonka.SageMaker.Types.HyperParameterTuningResourceConfig
import Amazonka.SageMaker.Types.HyperbandStrategyConfig
import Amazonka.SageMaker.Types.Image
import Amazonka.SageMaker.Types.ImageConfig
import Amazonka.SageMaker.Types.ImageVersion
import Amazonka.SageMaker.Types.InferenceExecutionConfig
import Amazonka.SageMaker.Types.InferenceMetrics
import Amazonka.SageMaker.Types.InferenceRecommendation
import Amazonka.SageMaker.Types.InferenceRecommendationsJob
import Amazonka.SageMaker.Types.InferenceRecommendationsJobStep
import Amazonka.SageMaker.Types.InferenceSpecification
import Amazonka.SageMaker.Types.InputConfig
import Amazonka.SageMaker.Types.InstanceGroup
import Amazonka.SageMaker.Types.InstanceMetadataServiceConfiguration
import Amazonka.SageMaker.Types.IntegerParameterRange
import Amazonka.SageMaker.Types.IntegerParameterRangeSpecification
import Amazonka.SageMaker.Types.JupyterServerAppSettings
import Amazonka.SageMaker.Types.KernelGatewayAppSettings
import Amazonka.SageMaker.Types.KernelGatewayImageConfig
import Amazonka.SageMaker.Types.KernelSpec
import Amazonka.SageMaker.Types.LabelCounters
import Amazonka.SageMaker.Types.LabelCountersForWorkteam
import Amazonka.SageMaker.Types.LabelingJobAlgorithmsConfig
import Amazonka.SageMaker.Types.LabelingJobDataAttributes
import Amazonka.SageMaker.Types.LabelingJobDataSource
import Amazonka.SageMaker.Types.LabelingJobForWorkteamSummary
import Amazonka.SageMaker.Types.LabelingJobInputConfig
import Amazonka.SageMaker.Types.LabelingJobOutput
import Amazonka.SageMaker.Types.LabelingJobOutputConfig
import Amazonka.SageMaker.Types.LabelingJobResourceConfig
import Amazonka.SageMaker.Types.LabelingJobS3DataSource
import Amazonka.SageMaker.Types.LabelingJobSnsDataSource
import Amazonka.SageMaker.Types.LabelingJobStoppingConditions
import Amazonka.SageMaker.Types.LabelingJobSummary
import Amazonka.SageMaker.Types.LambdaStepMetadata
import Amazonka.SageMaker.Types.LastUpdateStatus
import Amazonka.SageMaker.Types.LineageGroupSummary
import Amazonka.SageMaker.Types.MemberDefinition
import Amazonka.SageMaker.Types.MetadataProperties
import Amazonka.SageMaker.Types.MetricData
import Amazonka.SageMaker.Types.MetricDatum
import Amazonka.SageMaker.Types.MetricDefinition
import Amazonka.SageMaker.Types.MetricsSource
import Amazonka.SageMaker.Types.ModelArtifacts
import Amazonka.SageMaker.Types.ModelBiasAppSpecification
import Amazonka.SageMaker.Types.ModelBiasBaselineConfig
import Amazonka.SageMaker.Types.ModelBiasJobInput
import Amazonka.SageMaker.Types.ModelClientConfig
import Amazonka.SageMaker.Types.ModelConfiguration
import Amazonka.SageMaker.Types.ModelDataQuality
import Amazonka.SageMaker.Types.ModelDeployConfig
import Amazonka.SageMaker.Types.ModelDeployResult
import Amazonka.SageMaker.Types.ModelDigests
import Amazonka.SageMaker.Types.ModelExplainabilityAppSpecification
import Amazonka.SageMaker.Types.ModelExplainabilityBaselineConfig
import Amazonka.SageMaker.Types.ModelExplainabilityJobInput
import Amazonka.SageMaker.Types.ModelInput
import Amazonka.SageMaker.Types.ModelLatencyThreshold
import Amazonka.SageMaker.Types.ModelMetadataFilter
import Amazonka.SageMaker.Types.ModelMetadataSearchExpression
import Amazonka.SageMaker.Types.ModelMetadataSummary
import Amazonka.SageMaker.Types.ModelMetrics
import Amazonka.SageMaker.Types.ModelPackage
import Amazonka.SageMaker.Types.ModelPackageContainerDefinition
import Amazonka.SageMaker.Types.ModelPackageGroup
import Amazonka.SageMaker.Types.ModelPackageGroupSummary
import Amazonka.SageMaker.Types.ModelPackageStatusDetails
import Amazonka.SageMaker.Types.ModelPackageStatusItem
import Amazonka.SageMaker.Types.ModelPackageSummary
import Amazonka.SageMaker.Types.ModelPackageValidationProfile
import Amazonka.SageMaker.Types.ModelPackageValidationSpecification
import Amazonka.SageMaker.Types.ModelQuality
import Amazonka.SageMaker.Types.ModelQualityAppSpecification
import Amazonka.SageMaker.Types.ModelQualityBaselineConfig
import Amazonka.SageMaker.Types.ModelQualityJobInput
import Amazonka.SageMaker.Types.ModelStepMetadata
import Amazonka.SageMaker.Types.ModelSummary
import Amazonka.SageMaker.Types.MonitoringAppSpecification
import Amazonka.SageMaker.Types.MonitoringBaselineConfig
import Amazonka.SageMaker.Types.MonitoringClusterConfig
import Amazonka.SageMaker.Types.MonitoringConstraintsResource
import Amazonka.SageMaker.Types.MonitoringCsvDatasetFormat
import Amazonka.SageMaker.Types.MonitoringDatasetFormat
import Amazonka.SageMaker.Types.MonitoringExecutionSummary
import Amazonka.SageMaker.Types.MonitoringGroundTruthS3Input
import Amazonka.SageMaker.Types.MonitoringInput
import Amazonka.SageMaker.Types.MonitoringJobDefinition
import Amazonka.SageMaker.Types.MonitoringJobDefinitionSummary
import Amazonka.SageMaker.Types.MonitoringJsonDatasetFormat
import Amazonka.SageMaker.Types.MonitoringNetworkConfig
import Amazonka.SageMaker.Types.MonitoringOutput
import Amazonka.SageMaker.Types.MonitoringOutputConfig
import Amazonka.SageMaker.Types.MonitoringParquetDatasetFormat
import Amazonka.SageMaker.Types.MonitoringResources
import Amazonka.SageMaker.Types.MonitoringS3Output
import Amazonka.SageMaker.Types.MonitoringSchedule
import Amazonka.SageMaker.Types.MonitoringScheduleConfig
import Amazonka.SageMaker.Types.MonitoringScheduleSummary
import Amazonka.SageMaker.Types.MonitoringStatisticsResource
import Amazonka.SageMaker.Types.MonitoringStoppingCondition
import Amazonka.SageMaker.Types.MultiModelConfig
import Amazonka.SageMaker.Types.NeoVpcConfig
import Amazonka.SageMaker.Types.NestedFilters
import Amazonka.SageMaker.Types.NetworkConfig
import Amazonka.SageMaker.Types.NotebookInstanceLifecycleConfigSummary
import Amazonka.SageMaker.Types.NotebookInstanceLifecycleHook
import Amazonka.SageMaker.Types.NotebookInstanceSummary
import Amazonka.SageMaker.Types.NotificationConfiguration
import Amazonka.SageMaker.Types.ObjectiveStatusCounters
import Amazonka.SageMaker.Types.OfflineStoreConfig
import Amazonka.SageMaker.Types.OfflineStoreStatus
import Amazonka.SageMaker.Types.OidcConfig
import Amazonka.SageMaker.Types.OidcConfigForResponse
import Amazonka.SageMaker.Types.OidcMemberDefinition
import Amazonka.SageMaker.Types.OnlineStoreConfig
import Amazonka.SageMaker.Types.OnlineStoreSecurityConfig
import Amazonka.SageMaker.Types.OutputConfig
import Amazonka.SageMaker.Types.OutputDataConfig
import Amazonka.SageMaker.Types.OutputParameter
import Amazonka.SageMaker.Types.ParallelismConfiguration
import Amazonka.SageMaker.Types.Parameter
import Amazonka.SageMaker.Types.ParameterRange
import Amazonka.SageMaker.Types.ParameterRanges
import Amazonka.SageMaker.Types.Parent
import Amazonka.SageMaker.Types.ParentHyperParameterTuningJob
import Amazonka.SageMaker.Types.PendingDeploymentSummary
import Amazonka.SageMaker.Types.PendingProductionVariantSummary
import Amazonka.SageMaker.Types.Phase
import Amazonka.SageMaker.Types.Pipeline
import Amazonka.SageMaker.Types.PipelineDefinitionS3Location
import Amazonka.SageMaker.Types.PipelineExecution
import Amazonka.SageMaker.Types.PipelineExecutionStep
import Amazonka.SageMaker.Types.PipelineExecutionStepMetadata
import Amazonka.SageMaker.Types.PipelineExecutionSummary
import Amazonka.SageMaker.Types.PipelineExperimentConfig
import Amazonka.SageMaker.Types.PipelineSummary
import Amazonka.SageMaker.Types.ProcessingClusterConfig
import Amazonka.SageMaker.Types.ProcessingFeatureStoreOutput
import Amazonka.SageMaker.Types.ProcessingInput
import Amazonka.SageMaker.Types.ProcessingJob
import Amazonka.SageMaker.Types.ProcessingJobStepMetadata
import Amazonka.SageMaker.Types.ProcessingJobSummary
import Amazonka.SageMaker.Types.ProcessingOutput
import Amazonka.SageMaker.Types.ProcessingOutputConfig
import Amazonka.SageMaker.Types.ProcessingResources
import Amazonka.SageMaker.Types.ProcessingS3Input
import Amazonka.SageMaker.Types.ProcessingS3Output
import Amazonka.SageMaker.Types.ProcessingStoppingCondition
import Amazonka.SageMaker.Types.ProductionVariant
import Amazonka.SageMaker.Types.ProductionVariantCoreDumpConfig
import Amazonka.SageMaker.Types.ProductionVariantServerlessConfig
import Amazonka.SageMaker.Types.ProductionVariantStatus
import Amazonka.SageMaker.Types.ProductionVariantSummary
import Amazonka.SageMaker.Types.ProfilerConfig
import Amazonka.SageMaker.Types.ProfilerConfigForUpdate
import Amazonka.SageMaker.Types.ProfilerRuleConfiguration
import Amazonka.SageMaker.Types.ProfilerRuleEvaluationStatus
import Amazonka.SageMaker.Types.Project
import Amazonka.SageMaker.Types.ProjectSummary
import Amazonka.SageMaker.Types.PropertyNameQuery
import Amazonka.SageMaker.Types.PropertyNameSuggestion
import Amazonka.SageMaker.Types.ProvisioningParameter
import Amazonka.SageMaker.Types.PublicWorkforceTaskPrice
import Amazonka.SageMaker.Types.QualityCheckStepMetadata
import Amazonka.SageMaker.Types.QueryFilters
import Amazonka.SageMaker.Types.RSessionAppSettings
import Amazonka.SageMaker.Types.RStudioServerProAppSettings
import Amazonka.SageMaker.Types.RStudioServerProDomainSettings
import Amazonka.SageMaker.Types.RStudioServerProDomainSettingsForUpdate
import Amazonka.SageMaker.Types.RecommendationJobCompiledOutputConfig
import Amazonka.SageMaker.Types.RecommendationJobContainerConfig
import Amazonka.SageMaker.Types.RecommendationJobInferenceBenchmark
import Amazonka.SageMaker.Types.RecommendationJobInputConfig
import Amazonka.SageMaker.Types.RecommendationJobOutputConfig
import Amazonka.SageMaker.Types.RecommendationJobPayloadConfig
import Amazonka.SageMaker.Types.RecommendationJobResourceLimit
import Amazonka.SageMaker.Types.RecommendationJobStoppingConditions
import Amazonka.SageMaker.Types.RecommendationMetrics
import Amazonka.SageMaker.Types.RedshiftDatasetDefinition
import Amazonka.SageMaker.Types.RegisterModelStepMetadata
import Amazonka.SageMaker.Types.RenderableTask
import Amazonka.SageMaker.Types.RenderingError
import Amazonka.SageMaker.Types.RepositoryAuthConfig
import Amazonka.SageMaker.Types.ResolvedAttributes
import Amazonka.SageMaker.Types.ResourceConfig
import Amazonka.SageMaker.Types.ResourceConfigForUpdate
import Amazonka.SageMaker.Types.ResourceLimits
import Amazonka.SageMaker.Types.ResourceSpec
import Amazonka.SageMaker.Types.RetentionPolicy
import Amazonka.SageMaker.Types.RetryStrategy
import Amazonka.SageMaker.Types.S3DataSource
import Amazonka.SageMaker.Types.S3StorageConfig
import Amazonka.SageMaker.Types.ScheduleConfig
import Amazonka.SageMaker.Types.SearchExpression
import Amazonka.SageMaker.Types.SearchRecord
import Amazonka.SageMaker.Types.SecondaryStatusTransition
import Amazonka.SageMaker.Types.ServiceCatalogProvisionedProductDetails
import Amazonka.SageMaker.Types.ServiceCatalogProvisioningDetails
import Amazonka.SageMaker.Types.ServiceCatalogProvisioningUpdateDetails
import Amazonka.SageMaker.Types.SharingSettings
import Amazonka.SageMaker.Types.ShuffleConfig
import Amazonka.SageMaker.Types.SourceAlgorithm
import Amazonka.SageMaker.Types.SourceAlgorithmSpecification
import Amazonka.SageMaker.Types.SourceIpConfig
import Amazonka.SageMaker.Types.StoppingCondition
import Amazonka.SageMaker.Types.StudioLifecycleConfigDetails
import Amazonka.SageMaker.Types.SubscribedWorkteam
import Amazonka.SageMaker.Types.SuggestionQuery
import Amazonka.SageMaker.Types.Tag
import Amazonka.SageMaker.Types.TargetPlatform
import Amazonka.SageMaker.Types.TensorBoardAppSettings
import Amazonka.SageMaker.Types.TensorBoardOutputConfig
import Amazonka.SageMaker.Types.TimeSeriesForecastingSettings
import Amazonka.SageMaker.Types.TrafficPattern
import Amazonka.SageMaker.Types.TrafficRoutingConfig
import Amazonka.SageMaker.Types.TrainingJob
import Amazonka.SageMaker.Types.TrainingJobDefinition
import Amazonka.SageMaker.Types.TrainingJobStatusCounters
import Amazonka.SageMaker.Types.TrainingJobStepMetadata
import Amazonka.SageMaker.Types.TrainingJobSummary
import Amazonka.SageMaker.Types.TrainingSpecification
import Amazonka.SageMaker.Types.TransformDataSource
import Amazonka.SageMaker.Types.TransformInput
import Amazonka.SageMaker.Types.TransformJob
import Amazonka.SageMaker.Types.TransformJobDefinition
import Amazonka.SageMaker.Types.TransformJobStepMetadata
import Amazonka.SageMaker.Types.TransformJobSummary
import Amazonka.SageMaker.Types.TransformOutput
import Amazonka.SageMaker.Types.TransformResources
import Amazonka.SageMaker.Types.TransformS3DataSource
import Amazonka.SageMaker.Types.Trial
import Amazonka.SageMaker.Types.TrialComponent
import Amazonka.SageMaker.Types.TrialComponentArtifact
import Amazonka.SageMaker.Types.TrialComponentMetricSummary
import Amazonka.SageMaker.Types.TrialComponentParameterValue
import Amazonka.SageMaker.Types.TrialComponentSimpleSummary
import Amazonka.SageMaker.Types.TrialComponentSource
import Amazonka.SageMaker.Types.TrialComponentSourceDetail
import Amazonka.SageMaker.Types.TrialComponentStatus
import Amazonka.SageMaker.Types.TrialComponentSummary
import Amazonka.SageMaker.Types.TrialSource
import Amazonka.SageMaker.Types.TrialSummary
import Amazonka.SageMaker.Types.TuningJobCompletionCriteria
import Amazonka.SageMaker.Types.TuningJobStepMetaData
import Amazonka.SageMaker.Types.USD
import Amazonka.SageMaker.Types.UiConfig
import Amazonka.SageMaker.Types.UiTemplate
import Amazonka.SageMaker.Types.UiTemplateInfo
import Amazonka.SageMaker.Types.UserContext
import Amazonka.SageMaker.Types.UserProfileDetails
import Amazonka.SageMaker.Types.UserSettings
import Amazonka.SageMaker.Types.VariantProperty
import Amazonka.SageMaker.Types.Vertex
import Amazonka.SageMaker.Types.VpcConfig
import Amazonka.SageMaker.Types.WarmPoolStatus
import Amazonka.SageMaker.Types.Workforce
import Amazonka.SageMaker.Types.WorkforceVpcConfigRequest
import Amazonka.SageMaker.Types.WorkforceVpcConfigResponse
import Amazonka.SageMaker.Types.Workteam
import Amazonka.SageMaker.UpdateAction
import Amazonka.SageMaker.UpdateAppImageConfig
import Amazonka.SageMaker.UpdateArtifact
import Amazonka.SageMaker.UpdateCodeRepository
import Amazonka.SageMaker.UpdateContext
import Amazonka.SageMaker.UpdateDeviceFleet
import Amazonka.SageMaker.UpdateDevices
import Amazonka.SageMaker.UpdateDomain
import Amazonka.SageMaker.UpdateEndpoint
import Amazonka.SageMaker.UpdateEndpointWeightsAndCapacities
import Amazonka.SageMaker.UpdateExperiment
import Amazonka.SageMaker.UpdateFeatureGroup
import Amazonka.SageMaker.UpdateFeatureMetadata
import Amazonka.SageMaker.UpdateImage
import Amazonka.SageMaker.UpdateModelPackage
import Amazonka.SageMaker.UpdateMonitoringSchedule
import Amazonka.SageMaker.UpdateNotebookInstance
import Amazonka.SageMaker.UpdateNotebookInstanceLifecycleConfig
import Amazonka.SageMaker.UpdatePipeline
import Amazonka.SageMaker.UpdatePipelineExecution
import Amazonka.SageMaker.UpdateProject
import Amazonka.SageMaker.UpdateTrainingJob
import Amazonka.SageMaker.UpdateTrial
import Amazonka.SageMaker.UpdateTrialComponent
import Amazonka.SageMaker.UpdateUserProfile
import Amazonka.SageMaker.UpdateWorkforce
import Amazonka.SageMaker.UpdateWorkteam
