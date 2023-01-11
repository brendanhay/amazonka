{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-duplicate-exports #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMaker.Lens
-- Copyright   : (c) 2013-2023 Brendan Hay
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
    addAssociationResponse_destinationArn,
    addAssociationResponse_sourceArn,
    addAssociationResponse_httpStatus,

    -- ** AddTags
    addTags_resourceArn,
    addTags_tags,
    addTagsResponse_tags,
    addTagsResponse_httpStatus,

    -- ** AssociateTrialComponent
    associateTrialComponent_trialComponentName,
    associateTrialComponent_trialName,
    associateTrialComponentResponse_trialArn,
    associateTrialComponentResponse_trialComponentArn,
    associateTrialComponentResponse_httpStatus,

    -- ** BatchDescribeModelPackage
    batchDescribeModelPackage_modelPackageArnList,
    batchDescribeModelPackageResponse_batchDescribeModelPackageErrorMap,
    batchDescribeModelPackageResponse_modelPackageSummaries,
    batchDescribeModelPackageResponse_httpStatus,

    -- ** CreateAction
    createAction_description,
    createAction_metadataProperties,
    createAction_properties,
    createAction_status,
    createAction_tags,
    createAction_actionName,
    createAction_source,
    createAction_actionType,
    createActionResponse_actionArn,
    createActionResponse_httpStatus,

    -- ** CreateAlgorithm
    createAlgorithm_algorithmDescription,
    createAlgorithm_certifyForMarketplace,
    createAlgorithm_inferenceSpecification,
    createAlgorithm_tags,
    createAlgorithm_validationSpecification,
    createAlgorithm_algorithmName,
    createAlgorithm_trainingSpecification,
    createAlgorithmResponse_httpStatus,
    createAlgorithmResponse_algorithmArn,

    -- ** CreateApp
    createApp_resourceSpec,
    createApp_spaceName,
    createApp_tags,
    createApp_userProfileName,
    createApp_domainId,
    createApp_appType,
    createApp_appName,
    createAppResponse_appArn,
    createAppResponse_httpStatus,

    -- ** CreateAppImageConfig
    createAppImageConfig_kernelGatewayImageConfig,
    createAppImageConfig_tags,
    createAppImageConfig_appImageConfigName,
    createAppImageConfigResponse_appImageConfigArn,
    createAppImageConfigResponse_httpStatus,

    -- ** CreateArtifact
    createArtifact_artifactName,
    createArtifact_metadataProperties,
    createArtifact_properties,
    createArtifact_tags,
    createArtifact_source,
    createArtifact_artifactType,
    createArtifactResponse_artifactArn,
    createArtifactResponse_httpStatus,

    -- ** CreateAutoMLJob
    createAutoMLJob_autoMLJobConfig,
    createAutoMLJob_autoMLJobObjective,
    createAutoMLJob_generateCandidateDefinitionsOnly,
    createAutoMLJob_modelDeployConfig,
    createAutoMLJob_problemType,
    createAutoMLJob_tags,
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
    createCompilationJob_inputConfig,
    createCompilationJob_modelPackageVersionArn,
    createCompilationJob_tags,
    createCompilationJob_vpcConfig,
    createCompilationJob_compilationJobName,
    createCompilationJob_roleArn,
    createCompilationJob_outputConfig,
    createCompilationJob_stoppingCondition,
    createCompilationJobResponse_httpStatus,
    createCompilationJobResponse_compilationJobArn,

    -- ** CreateContext
    createContext_description,
    createContext_properties,
    createContext_tags,
    createContext_contextName,
    createContext_source,
    createContext_contextType,
    createContextResponse_contextArn,
    createContextResponse_httpStatus,

    -- ** CreateDataQualityJobDefinition
    createDataQualityJobDefinition_dataQualityBaselineConfig,
    createDataQualityJobDefinition_networkConfig,
    createDataQualityJobDefinition_stoppingCondition,
    createDataQualityJobDefinition_tags,
    createDataQualityJobDefinition_jobDefinitionName,
    createDataQualityJobDefinition_dataQualityAppSpecification,
    createDataQualityJobDefinition_dataQualityJobInput,
    createDataQualityJobDefinition_dataQualityJobOutputConfig,
    createDataQualityJobDefinition_jobResources,
    createDataQualityJobDefinition_roleArn,
    createDataQualityJobDefinitionResponse_httpStatus,
    createDataQualityJobDefinitionResponse_jobDefinitionArn,

    -- ** CreateDeviceFleet
    createDeviceFleet_description,
    createDeviceFleet_enableIotRoleAlias,
    createDeviceFleet_roleArn,
    createDeviceFleet_tags,
    createDeviceFleet_deviceFleetName,
    createDeviceFleet_outputConfig,

    -- ** CreateDomain
    createDomain_appNetworkAccessType,
    createDomain_appSecurityGroupManagement,
    createDomain_defaultSpaceSettings,
    createDomain_domainSettings,
    createDomain_homeEfsFileSystemKmsKeyId,
    createDomain_kmsKeyId,
    createDomain_tags,
    createDomain_domainName,
    createDomain_authMode,
    createDomain_defaultUserSettings,
    createDomain_subnetIds,
    createDomain_vpcId,
    createDomainResponse_domainArn,
    createDomainResponse_url,
    createDomainResponse_httpStatus,

    -- ** CreateEdgeDeploymentPlan
    createEdgeDeploymentPlan_stages,
    createEdgeDeploymentPlan_tags,
    createEdgeDeploymentPlan_edgeDeploymentPlanName,
    createEdgeDeploymentPlan_modelConfigs,
    createEdgeDeploymentPlan_deviceFleetName,
    createEdgeDeploymentPlanResponse_httpStatus,
    createEdgeDeploymentPlanResponse_edgeDeploymentPlanArn,

    -- ** CreateEdgeDeploymentStage
    createEdgeDeploymentStage_edgeDeploymentPlanName,
    createEdgeDeploymentStage_stages,

    -- ** CreateEdgePackagingJob
    createEdgePackagingJob_resourceKey,
    createEdgePackagingJob_tags,
    createEdgePackagingJob_edgePackagingJobName,
    createEdgePackagingJob_compilationJobName,
    createEdgePackagingJob_modelName,
    createEdgePackagingJob_modelVersion,
    createEdgePackagingJob_roleArn,
    createEdgePackagingJob_outputConfig,

    -- ** CreateEndpoint
    createEndpoint_deploymentConfig,
    createEndpoint_tags,
    createEndpoint_endpointName,
    createEndpoint_endpointConfigName,
    createEndpointResponse_httpStatus,
    createEndpointResponse_endpointArn,

    -- ** CreateEndpointConfig
    createEndpointConfig_asyncInferenceConfig,
    createEndpointConfig_dataCaptureConfig,
    createEndpointConfig_explainerConfig,
    createEndpointConfig_kmsKeyId,
    createEndpointConfig_shadowProductionVariants,
    createEndpointConfig_tags,
    createEndpointConfig_endpointConfigName,
    createEndpointConfig_productionVariants,
    createEndpointConfigResponse_httpStatus,
    createEndpointConfigResponse_endpointConfigArn,

    -- ** CreateExperiment
    createExperiment_description,
    createExperiment_displayName,
    createExperiment_tags,
    createExperiment_experimentName,
    createExperimentResponse_experimentArn,
    createExperimentResponse_httpStatus,

    -- ** CreateFeatureGroup
    createFeatureGroup_description,
    createFeatureGroup_offlineStoreConfig,
    createFeatureGroup_onlineStoreConfig,
    createFeatureGroup_roleArn,
    createFeatureGroup_tags,
    createFeatureGroup_featureGroupName,
    createFeatureGroup_recordIdentifierFeatureName,
    createFeatureGroup_eventTimeFeatureName,
    createFeatureGroup_featureDefinitions,
    createFeatureGroupResponse_httpStatus,
    createFeatureGroupResponse_featureGroupArn,

    -- ** CreateFlowDefinition
    createFlowDefinition_humanLoopActivationConfig,
    createFlowDefinition_humanLoopRequestSource,
    createFlowDefinition_tags,
    createFlowDefinition_flowDefinitionName,
    createFlowDefinition_humanLoopConfig,
    createFlowDefinition_outputConfig,
    createFlowDefinition_roleArn,
    createFlowDefinitionResponse_httpStatus,
    createFlowDefinitionResponse_flowDefinitionArn,

    -- ** CreateHub
    createHub_hubDisplayName,
    createHub_hubSearchKeywords,
    createHub_s3StorageConfig,
    createHub_tags,
    createHub_hubName,
    createHub_hubDescription,
    createHubResponse_httpStatus,
    createHubResponse_hubArn,

    -- ** CreateHumanTaskUi
    createHumanTaskUi_tags,
    createHumanTaskUi_humanTaskUiName,
    createHumanTaskUi_uiTemplate,
    createHumanTaskUiResponse_httpStatus,
    createHumanTaskUiResponse_humanTaskUiArn,

    -- ** CreateHyperParameterTuningJob
    createHyperParameterTuningJob_tags,
    createHyperParameterTuningJob_trainingJobDefinition,
    createHyperParameterTuningJob_trainingJobDefinitions,
    createHyperParameterTuningJob_warmStartConfig,
    createHyperParameterTuningJob_hyperParameterTuningJobName,
    createHyperParameterTuningJob_hyperParameterTuningJobConfig,
    createHyperParameterTuningJobResponse_httpStatus,
    createHyperParameterTuningJobResponse_hyperParameterTuningJobArn,

    -- ** CreateImage
    createImage_description,
    createImage_displayName,
    createImage_tags,
    createImage_imageName,
    createImage_roleArn,
    createImageResponse_imageArn,
    createImageResponse_httpStatus,

    -- ** CreateImageVersion
    createImageVersion_aliases,
    createImageVersion_horovod,
    createImageVersion_jobType,
    createImageVersion_mLFramework,
    createImageVersion_processor,
    createImageVersion_programmingLang,
    createImageVersion_releaseNotes,
    createImageVersion_vendorGuidance,
    createImageVersion_baseImage,
    createImageVersion_clientToken,
    createImageVersion_imageName,
    createImageVersionResponse_imageVersionArn,
    createImageVersionResponse_httpStatus,

    -- ** CreateInferenceExperiment
    createInferenceExperiment_dataStorageConfig,
    createInferenceExperiment_description,
    createInferenceExperiment_kmsKey,
    createInferenceExperiment_schedule,
    createInferenceExperiment_tags,
    createInferenceExperiment_name,
    createInferenceExperiment_type,
    createInferenceExperiment_roleArn,
    createInferenceExperiment_endpointName,
    createInferenceExperiment_modelVariants,
    createInferenceExperiment_shadowModeConfig,
    createInferenceExperimentResponse_httpStatus,
    createInferenceExperimentResponse_inferenceExperimentArn,

    -- ** CreateInferenceRecommendationsJob
    createInferenceRecommendationsJob_jobDescription,
    createInferenceRecommendationsJob_outputConfig,
    createInferenceRecommendationsJob_stoppingConditions,
    createInferenceRecommendationsJob_tags,
    createInferenceRecommendationsJob_jobName,
    createInferenceRecommendationsJob_jobType,
    createInferenceRecommendationsJob_roleArn,
    createInferenceRecommendationsJob_inputConfig,
    createInferenceRecommendationsJobResponse_httpStatus,
    createInferenceRecommendationsJobResponse_jobArn,

    -- ** CreateLabelingJob
    createLabelingJob_labelCategoryConfigS3Uri,
    createLabelingJob_labelingJobAlgorithmsConfig,
    createLabelingJob_stoppingConditions,
    createLabelingJob_tags,
    createLabelingJob_labelingJobName,
    createLabelingJob_labelAttributeName,
    createLabelingJob_inputConfig,
    createLabelingJob_outputConfig,
    createLabelingJob_roleArn,
    createLabelingJob_humanTaskConfig,
    createLabelingJobResponse_httpStatus,
    createLabelingJobResponse_labelingJobArn,

    -- ** CreateModel
    createModel_containers,
    createModel_enableNetworkIsolation,
    createModel_inferenceExecutionConfig,
    createModel_primaryContainer,
    createModel_tags,
    createModel_vpcConfig,
    createModel_modelName,
    createModel_executionRoleArn,
    createModelResponse_httpStatus,
    createModelResponse_modelArn,

    -- ** CreateModelBiasJobDefinition
    createModelBiasJobDefinition_modelBiasBaselineConfig,
    createModelBiasJobDefinition_networkConfig,
    createModelBiasJobDefinition_stoppingCondition,
    createModelBiasJobDefinition_tags,
    createModelBiasJobDefinition_jobDefinitionName,
    createModelBiasJobDefinition_modelBiasAppSpecification,
    createModelBiasJobDefinition_modelBiasJobInput,
    createModelBiasJobDefinition_modelBiasJobOutputConfig,
    createModelBiasJobDefinition_jobResources,
    createModelBiasJobDefinition_roleArn,
    createModelBiasJobDefinitionResponse_httpStatus,
    createModelBiasJobDefinitionResponse_jobDefinitionArn,

    -- ** CreateModelCard
    createModelCard_securityConfig,
    createModelCard_tags,
    createModelCard_modelCardName,
    createModelCard_content,
    createModelCard_modelCardStatus,
    createModelCardResponse_httpStatus,
    createModelCardResponse_modelCardArn,

    -- ** CreateModelCardExportJob
    createModelCardExportJob_modelCardVersion,
    createModelCardExportJob_modelCardName,
    createModelCardExportJob_modelCardExportJobName,
    createModelCardExportJob_outputConfig,
    createModelCardExportJobResponse_httpStatus,
    createModelCardExportJobResponse_modelCardExportJobArn,

    -- ** CreateModelExplainabilityJobDefinition
    createModelExplainabilityJobDefinition_modelExplainabilityBaselineConfig,
    createModelExplainabilityJobDefinition_networkConfig,
    createModelExplainabilityJobDefinition_stoppingCondition,
    createModelExplainabilityJobDefinition_tags,
    createModelExplainabilityJobDefinition_jobDefinitionName,
    createModelExplainabilityJobDefinition_modelExplainabilityAppSpecification,
    createModelExplainabilityJobDefinition_modelExplainabilityJobInput,
    createModelExplainabilityJobDefinition_modelExplainabilityJobOutputConfig,
    createModelExplainabilityJobDefinition_jobResources,
    createModelExplainabilityJobDefinition_roleArn,
    createModelExplainabilityJobDefinitionResponse_httpStatus,
    createModelExplainabilityJobDefinitionResponse_jobDefinitionArn,

    -- ** CreateModelPackage
    createModelPackage_additionalInferenceSpecifications,
    createModelPackage_certifyForMarketplace,
    createModelPackage_clientToken,
    createModelPackage_customerMetadataProperties,
    createModelPackage_domain,
    createModelPackage_driftCheckBaselines,
    createModelPackage_inferenceSpecification,
    createModelPackage_metadataProperties,
    createModelPackage_modelApprovalStatus,
    createModelPackage_modelMetrics,
    createModelPackage_modelPackageDescription,
    createModelPackage_modelPackageGroupName,
    createModelPackage_modelPackageName,
    createModelPackage_samplePayloadUrl,
    createModelPackage_sourceAlgorithmSpecification,
    createModelPackage_tags,
    createModelPackage_task,
    createModelPackage_validationSpecification,
    createModelPackageResponse_httpStatus,
    createModelPackageResponse_modelPackageArn,

    -- ** CreateModelPackageGroup
    createModelPackageGroup_modelPackageGroupDescription,
    createModelPackageGroup_tags,
    createModelPackageGroup_modelPackageGroupName,
    createModelPackageGroupResponse_httpStatus,
    createModelPackageGroupResponse_modelPackageGroupArn,

    -- ** CreateModelQualityJobDefinition
    createModelQualityJobDefinition_modelQualityBaselineConfig,
    createModelQualityJobDefinition_networkConfig,
    createModelQualityJobDefinition_stoppingCondition,
    createModelQualityJobDefinition_tags,
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
    createNotebookInstance_acceleratorTypes,
    createNotebookInstance_additionalCodeRepositories,
    createNotebookInstance_defaultCodeRepository,
    createNotebookInstance_directInternetAccess,
    createNotebookInstance_instanceMetadataServiceConfiguration,
    createNotebookInstance_kmsKeyId,
    createNotebookInstance_lifecycleConfigName,
    createNotebookInstance_platformIdentifier,
    createNotebookInstance_rootAccess,
    createNotebookInstance_securityGroupIds,
    createNotebookInstance_subnetId,
    createNotebookInstance_tags,
    createNotebookInstance_volumeSizeInGB,
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
    createPipeline_parallelismConfiguration,
    createPipeline_pipelineDefinition,
    createPipeline_pipelineDefinitionS3Location,
    createPipeline_pipelineDescription,
    createPipeline_pipelineDisplayName,
    createPipeline_tags,
    createPipeline_pipelineName,
    createPipeline_clientRequestToken,
    createPipeline_roleArn,
    createPipelineResponse_pipelineArn,
    createPipelineResponse_httpStatus,

    -- ** CreatePresignedDomainUrl
    createPresignedDomainUrl_expiresInSeconds,
    createPresignedDomainUrl_sessionExpirationDurationInSeconds,
    createPresignedDomainUrl_spaceName,
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
    createProcessingJob_environment,
    createProcessingJob_experimentConfig,
    createProcessingJob_networkConfig,
    createProcessingJob_processingInputs,
    createProcessingJob_processingOutputConfig,
    createProcessingJob_stoppingCondition,
    createProcessingJob_tags,
    createProcessingJob_processingJobName,
    createProcessingJob_processingResources,
    createProcessingJob_appSpecification,
    createProcessingJob_roleArn,
    createProcessingJobResponse_httpStatus,
    createProcessingJobResponse_processingJobArn,

    -- ** CreateProject
    createProject_projectDescription,
    createProject_tags,
    createProject_projectName,
    createProject_serviceCatalogProvisioningDetails,
    createProjectResponse_httpStatus,
    createProjectResponse_projectArn,
    createProjectResponse_projectId,

    -- ** CreateSpace
    createSpace_spaceSettings,
    createSpace_tags,
    createSpace_domainId,
    createSpace_spaceName,
    createSpaceResponse_spaceArn,
    createSpaceResponse_httpStatus,

    -- ** CreateStudioLifecycleConfig
    createStudioLifecycleConfig_tags,
    createStudioLifecycleConfig_studioLifecycleConfigName,
    createStudioLifecycleConfig_studioLifecycleConfigContent,
    createStudioLifecycleConfig_studioLifecycleConfigAppType,
    createStudioLifecycleConfigResponse_studioLifecycleConfigArn,
    createStudioLifecycleConfigResponse_httpStatus,

    -- ** CreateTrainingJob
    createTrainingJob_checkpointConfig,
    createTrainingJob_debugHookConfig,
    createTrainingJob_debugRuleConfigurations,
    createTrainingJob_enableInterContainerTrafficEncryption,
    createTrainingJob_enableManagedSpotTraining,
    createTrainingJob_enableNetworkIsolation,
    createTrainingJob_environment,
    createTrainingJob_experimentConfig,
    createTrainingJob_hyperParameters,
    createTrainingJob_inputDataConfig,
    createTrainingJob_profilerConfig,
    createTrainingJob_profilerRuleConfigurations,
    createTrainingJob_retryStrategy,
    createTrainingJob_tags,
    createTrainingJob_tensorBoardOutputConfig,
    createTrainingJob_vpcConfig,
    createTrainingJob_trainingJobName,
    createTrainingJob_algorithmSpecification,
    createTrainingJob_roleArn,
    createTrainingJob_outputDataConfig,
    createTrainingJob_resourceConfig,
    createTrainingJob_stoppingCondition,
    createTrainingJobResponse_httpStatus,
    createTrainingJobResponse_trainingJobArn,

    -- ** CreateTransformJob
    createTransformJob_batchStrategy,
    createTransformJob_dataCaptureConfig,
    createTransformJob_dataProcessing,
    createTransformJob_environment,
    createTransformJob_experimentConfig,
    createTransformJob_maxConcurrentTransforms,
    createTransformJob_maxPayloadInMB,
    createTransformJob_modelClientConfig,
    createTransformJob_tags,
    createTransformJob_transformJobName,
    createTransformJob_modelName,
    createTransformJob_transformInput,
    createTransformJob_transformOutput,
    createTransformJob_transformResources,
    createTransformJobResponse_httpStatus,
    createTransformJobResponse_transformJobArn,

    -- ** CreateTrial
    createTrial_displayName,
    createTrial_metadataProperties,
    createTrial_tags,
    createTrial_trialName,
    createTrial_experimentName,
    createTrialResponse_trialArn,
    createTrialResponse_httpStatus,

    -- ** CreateTrialComponent
    createTrialComponent_displayName,
    createTrialComponent_endTime,
    createTrialComponent_inputArtifacts,
    createTrialComponent_metadataProperties,
    createTrialComponent_outputArtifacts,
    createTrialComponent_parameters,
    createTrialComponent_startTime,
    createTrialComponent_status,
    createTrialComponent_tags,
    createTrialComponent_trialComponentName,
    createTrialComponentResponse_trialComponentArn,
    createTrialComponentResponse_httpStatus,

    -- ** CreateUserProfile
    createUserProfile_singleSignOnUserIdentifier,
    createUserProfile_singleSignOnUserValue,
    createUserProfile_tags,
    createUserProfile_userSettings,
    createUserProfile_domainId,
    createUserProfile_userProfileName,
    createUserProfileResponse_userProfileArn,
    createUserProfileResponse_httpStatus,

    -- ** CreateWorkforce
    createWorkforce_cognitoConfig,
    createWorkforce_oidcConfig,
    createWorkforce_sourceIpConfig,
    createWorkforce_tags,
    createWorkforce_workforceVpcConfig,
    createWorkforce_workforceName,
    createWorkforceResponse_httpStatus,
    createWorkforceResponse_workforceArn,

    -- ** CreateWorkteam
    createWorkteam_notificationConfiguration,
    createWorkteam_tags,
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
    deleteApp_spaceName,
    deleteApp_userProfileName,
    deleteApp_domainId,
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
    deleteAssociationResponse_destinationArn,
    deleteAssociationResponse_sourceArn,
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

    -- ** DeleteHub
    deleteHub_hubName,

    -- ** DeleteHubContent
    deleteHubContent_hubName,
    deleteHubContent_hubContentType,
    deleteHubContent_hubContentName,
    deleteHubContent_hubContentVersion,

    -- ** DeleteHumanTaskUi
    deleteHumanTaskUi_humanTaskUiName,
    deleteHumanTaskUiResponse_httpStatus,

    -- ** DeleteImage
    deleteImage_imageName,
    deleteImageResponse_httpStatus,

    -- ** DeleteImageVersion
    deleteImageVersion_alias,
    deleteImageVersion_version,
    deleteImageVersion_imageName,
    deleteImageVersionResponse_httpStatus,

    -- ** DeleteInferenceExperiment
    deleteInferenceExperiment_name,
    deleteInferenceExperimentResponse_httpStatus,
    deleteInferenceExperimentResponse_inferenceExperimentArn,

    -- ** DeleteModel
    deleteModel_modelName,

    -- ** DeleteModelBiasJobDefinition
    deleteModelBiasJobDefinition_jobDefinitionName,

    -- ** DeleteModelCard
    deleteModelCard_modelCardName,

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

    -- ** DeleteSpace
    deleteSpace_domainId,
    deleteSpace_spaceName,

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
    describeActionResponse_actionArn,
    describeActionResponse_actionName,
    describeActionResponse_actionType,
    describeActionResponse_createdBy,
    describeActionResponse_creationTime,
    describeActionResponse_description,
    describeActionResponse_lastModifiedBy,
    describeActionResponse_lastModifiedTime,
    describeActionResponse_lineageGroupArn,
    describeActionResponse_metadataProperties,
    describeActionResponse_properties,
    describeActionResponse_source,
    describeActionResponse_status,
    describeActionResponse_httpStatus,

    -- ** DescribeAlgorithm
    describeAlgorithm_algorithmName,
    describeAlgorithmResponse_algorithmDescription,
    describeAlgorithmResponse_certifyForMarketplace,
    describeAlgorithmResponse_inferenceSpecification,
    describeAlgorithmResponse_productId,
    describeAlgorithmResponse_validationSpecification,
    describeAlgorithmResponse_httpStatus,
    describeAlgorithmResponse_algorithmName,
    describeAlgorithmResponse_algorithmArn,
    describeAlgorithmResponse_creationTime,
    describeAlgorithmResponse_trainingSpecification,
    describeAlgorithmResponse_algorithmStatus,
    describeAlgorithmResponse_algorithmStatusDetails,

    -- ** DescribeApp
    describeApp_spaceName,
    describeApp_userProfileName,
    describeApp_domainId,
    describeApp_appType,
    describeApp_appName,
    describeAppResponse_appArn,
    describeAppResponse_appName,
    describeAppResponse_appType,
    describeAppResponse_creationTime,
    describeAppResponse_domainId,
    describeAppResponse_failureReason,
    describeAppResponse_lastHealthCheckTimestamp,
    describeAppResponse_lastUserActivityTimestamp,
    describeAppResponse_resourceSpec,
    describeAppResponse_spaceName,
    describeAppResponse_status,
    describeAppResponse_userProfileName,
    describeAppResponse_httpStatus,

    -- ** DescribeAppImageConfig
    describeAppImageConfig_appImageConfigName,
    describeAppImageConfigResponse_appImageConfigArn,
    describeAppImageConfigResponse_appImageConfigName,
    describeAppImageConfigResponse_creationTime,
    describeAppImageConfigResponse_kernelGatewayImageConfig,
    describeAppImageConfigResponse_lastModifiedTime,
    describeAppImageConfigResponse_httpStatus,

    -- ** DescribeArtifact
    describeArtifact_artifactArn,
    describeArtifactResponse_artifactArn,
    describeArtifactResponse_artifactName,
    describeArtifactResponse_artifactType,
    describeArtifactResponse_createdBy,
    describeArtifactResponse_creationTime,
    describeArtifactResponse_lastModifiedBy,
    describeArtifactResponse_lastModifiedTime,
    describeArtifactResponse_lineageGroupArn,
    describeArtifactResponse_metadataProperties,
    describeArtifactResponse_properties,
    describeArtifactResponse_source,
    describeArtifactResponse_httpStatus,

    -- ** DescribeAutoMLJob
    describeAutoMLJob_autoMLJobName,
    describeAutoMLJobResponse_autoMLJobArtifacts,
    describeAutoMLJobResponse_autoMLJobConfig,
    describeAutoMLJobResponse_autoMLJobObjective,
    describeAutoMLJobResponse_bestCandidate,
    describeAutoMLJobResponse_endTime,
    describeAutoMLJobResponse_failureReason,
    describeAutoMLJobResponse_generateCandidateDefinitionsOnly,
    describeAutoMLJobResponse_modelDeployConfig,
    describeAutoMLJobResponse_modelDeployResult,
    describeAutoMLJobResponse_partialFailureReasons,
    describeAutoMLJobResponse_problemType,
    describeAutoMLJobResponse_resolvedAttributes,
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
    describeCompilationJobResponse_compilationStartTime,
    describeCompilationJobResponse_inferenceImage,
    describeCompilationJobResponse_modelDigests,
    describeCompilationJobResponse_modelPackageVersionArn,
    describeCompilationJobResponse_vpcConfig,
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
    describeContextResponse_contextArn,
    describeContextResponse_contextName,
    describeContextResponse_contextType,
    describeContextResponse_createdBy,
    describeContextResponse_creationTime,
    describeContextResponse_description,
    describeContextResponse_lastModifiedBy,
    describeContextResponse_lastModifiedTime,
    describeContextResponse_lineageGroupArn,
    describeContextResponse_properties,
    describeContextResponse_source,
    describeContextResponse_httpStatus,

    -- ** DescribeDataQualityJobDefinition
    describeDataQualityJobDefinition_jobDefinitionName,
    describeDataQualityJobDefinitionResponse_dataQualityBaselineConfig,
    describeDataQualityJobDefinitionResponse_networkConfig,
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
    describeDeviceResponse_agentVersion,
    describeDeviceResponse_description,
    describeDeviceResponse_deviceArn,
    describeDeviceResponse_iotThingName,
    describeDeviceResponse_latestHeartbeat,
    describeDeviceResponse_maxModels,
    describeDeviceResponse_models,
    describeDeviceResponse_nextToken,
    describeDeviceResponse_httpStatus,
    describeDeviceResponse_deviceName,
    describeDeviceResponse_deviceFleetName,
    describeDeviceResponse_registrationTime,

    -- ** DescribeDeviceFleet
    describeDeviceFleet_deviceFleetName,
    describeDeviceFleetResponse_description,
    describeDeviceFleetResponse_iotRoleAlias,
    describeDeviceFleetResponse_roleArn,
    describeDeviceFleetResponse_httpStatus,
    describeDeviceFleetResponse_deviceFleetName,
    describeDeviceFleetResponse_deviceFleetArn,
    describeDeviceFleetResponse_outputConfig,
    describeDeviceFleetResponse_creationTime,
    describeDeviceFleetResponse_lastModifiedTime,

    -- ** DescribeDomain
    describeDomain_domainId,
    describeDomainResponse_appNetworkAccessType,
    describeDomainResponse_appSecurityGroupManagement,
    describeDomainResponse_authMode,
    describeDomainResponse_creationTime,
    describeDomainResponse_defaultSpaceSettings,
    describeDomainResponse_defaultUserSettings,
    describeDomainResponse_domainArn,
    describeDomainResponse_domainId,
    describeDomainResponse_domainName,
    describeDomainResponse_domainSettings,
    describeDomainResponse_failureReason,
    describeDomainResponse_homeEfsFileSystemId,
    describeDomainResponse_homeEfsFileSystemKmsKeyId,
    describeDomainResponse_kmsKeyId,
    describeDomainResponse_lastModifiedTime,
    describeDomainResponse_securityGroupIdForDomainBoundary,
    describeDomainResponse_singleSignOnManagedApplicationInstanceId,
    describeDomainResponse_status,
    describeDomainResponse_subnetIds,
    describeDomainResponse_url,
    describeDomainResponse_vpcId,
    describeDomainResponse_httpStatus,

    -- ** DescribeEdgeDeploymentPlan
    describeEdgeDeploymentPlan_maxResults,
    describeEdgeDeploymentPlan_nextToken,
    describeEdgeDeploymentPlan_edgeDeploymentPlanName,
    describeEdgeDeploymentPlanResponse_creationTime,
    describeEdgeDeploymentPlanResponse_edgeDeploymentFailed,
    describeEdgeDeploymentPlanResponse_edgeDeploymentPending,
    describeEdgeDeploymentPlanResponse_edgeDeploymentSuccess,
    describeEdgeDeploymentPlanResponse_lastModifiedTime,
    describeEdgeDeploymentPlanResponse_nextToken,
    describeEdgeDeploymentPlanResponse_httpStatus,
    describeEdgeDeploymentPlanResponse_edgeDeploymentPlanArn,
    describeEdgeDeploymentPlanResponse_edgeDeploymentPlanName,
    describeEdgeDeploymentPlanResponse_modelConfigs,
    describeEdgeDeploymentPlanResponse_deviceFleetName,
    describeEdgeDeploymentPlanResponse_stages,

    -- ** DescribeEdgePackagingJob
    describeEdgePackagingJob_edgePackagingJobName,
    describeEdgePackagingJobResponse_compilationJobName,
    describeEdgePackagingJobResponse_creationTime,
    describeEdgePackagingJobResponse_edgePackagingJobStatusMessage,
    describeEdgePackagingJobResponse_lastModifiedTime,
    describeEdgePackagingJobResponse_modelArtifact,
    describeEdgePackagingJobResponse_modelName,
    describeEdgePackagingJobResponse_modelSignature,
    describeEdgePackagingJobResponse_modelVersion,
    describeEdgePackagingJobResponse_outputConfig,
    describeEdgePackagingJobResponse_presetDeploymentOutput,
    describeEdgePackagingJobResponse_resourceKey,
    describeEdgePackagingJobResponse_roleArn,
    describeEdgePackagingJobResponse_httpStatus,
    describeEdgePackagingJobResponse_edgePackagingJobArn,
    describeEdgePackagingJobResponse_edgePackagingJobName,
    describeEdgePackagingJobResponse_edgePackagingJobStatus,

    -- ** DescribeEndpoint
    describeEndpoint_endpointName,
    describeEndpointResponse_asyncInferenceConfig,
    describeEndpointResponse_dataCaptureConfig,
    describeEndpointResponse_explainerConfig,
    describeEndpointResponse_failureReason,
    describeEndpointResponse_lastDeploymentConfig,
    describeEndpointResponse_pendingDeploymentSummary,
    describeEndpointResponse_productionVariants,
    describeEndpointResponse_shadowProductionVariants,
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
    describeEndpointConfigResponse_explainerConfig,
    describeEndpointConfigResponse_kmsKeyId,
    describeEndpointConfigResponse_shadowProductionVariants,
    describeEndpointConfigResponse_httpStatus,
    describeEndpointConfigResponse_endpointConfigName,
    describeEndpointConfigResponse_endpointConfigArn,
    describeEndpointConfigResponse_productionVariants,
    describeEndpointConfigResponse_creationTime,

    -- ** DescribeExperiment
    describeExperiment_experimentName,
    describeExperimentResponse_createdBy,
    describeExperimentResponse_creationTime,
    describeExperimentResponse_description,
    describeExperimentResponse_displayName,
    describeExperimentResponse_experimentArn,
    describeExperimentResponse_experimentName,
    describeExperimentResponse_lastModifiedBy,
    describeExperimentResponse_lastModifiedTime,
    describeExperimentResponse_source,
    describeExperimentResponse_httpStatus,

    -- ** DescribeFeatureGroup
    describeFeatureGroup_nextToken,
    describeFeatureGroup_featureGroupName,
    describeFeatureGroupResponse_description,
    describeFeatureGroupResponse_failureReason,
    describeFeatureGroupResponse_featureGroupStatus,
    describeFeatureGroupResponse_lastModifiedTime,
    describeFeatureGroupResponse_lastUpdateStatus,
    describeFeatureGroupResponse_offlineStoreConfig,
    describeFeatureGroupResponse_offlineStoreStatus,
    describeFeatureGroupResponse_onlineStoreConfig,
    describeFeatureGroupResponse_onlineStoreTotalSizeBytes,
    describeFeatureGroupResponse_roleArn,
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
    describeFlowDefinitionResponse_failureReason,
    describeFlowDefinitionResponse_humanLoopActivationConfig,
    describeFlowDefinitionResponse_humanLoopRequestSource,
    describeFlowDefinitionResponse_httpStatus,
    describeFlowDefinitionResponse_flowDefinitionArn,
    describeFlowDefinitionResponse_flowDefinitionName,
    describeFlowDefinitionResponse_flowDefinitionStatus,
    describeFlowDefinitionResponse_creationTime,
    describeFlowDefinitionResponse_humanLoopConfig,
    describeFlowDefinitionResponse_outputConfig,
    describeFlowDefinitionResponse_roleArn,

    -- ** DescribeHub
    describeHub_hubName,
    describeHubResponse_failureReason,
    describeHubResponse_hubDescription,
    describeHubResponse_hubDisplayName,
    describeHubResponse_hubSearchKeywords,
    describeHubResponse_s3StorageConfig,
    describeHubResponse_httpStatus,
    describeHubResponse_hubName,
    describeHubResponse_hubArn,
    describeHubResponse_hubStatus,
    describeHubResponse_creationTime,
    describeHubResponse_lastModifiedTime,

    -- ** DescribeHubContent
    describeHubContent_hubContentVersion,
    describeHubContent_hubName,
    describeHubContent_hubContentType,
    describeHubContent_hubContentName,
    describeHubContentResponse_failureReason,
    describeHubContentResponse_hubContentDependencies,
    describeHubContentResponse_hubContentDescription,
    describeHubContentResponse_hubContentDisplayName,
    describeHubContentResponse_hubContentMarkdown,
    describeHubContentResponse_hubContentSearchKeywords,
    describeHubContentResponse_httpStatus,
    describeHubContentResponse_hubContentName,
    describeHubContentResponse_hubContentArn,
    describeHubContentResponse_hubContentVersion,
    describeHubContentResponse_hubContentType,
    describeHubContentResponse_documentSchemaVersion,
    describeHubContentResponse_hubName,
    describeHubContentResponse_hubArn,
    describeHubContentResponse_hubContentDocument,
    describeHubContentResponse_hubContentStatus,
    describeHubContentResponse_creationTime,

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
    describeHyperParameterTuningJobResponse_bestTrainingJob,
    describeHyperParameterTuningJobResponse_failureReason,
    describeHyperParameterTuningJobResponse_hyperParameterTuningEndTime,
    describeHyperParameterTuningJobResponse_lastModifiedTime,
    describeHyperParameterTuningJobResponse_overallBestTrainingJob,
    describeHyperParameterTuningJobResponse_trainingJobDefinition,
    describeHyperParameterTuningJobResponse_trainingJobDefinitions,
    describeHyperParameterTuningJobResponse_warmStartConfig,
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
    describeImageResponse_creationTime,
    describeImageResponse_description,
    describeImageResponse_displayName,
    describeImageResponse_failureReason,
    describeImageResponse_imageArn,
    describeImageResponse_imageName,
    describeImageResponse_imageStatus,
    describeImageResponse_lastModifiedTime,
    describeImageResponse_roleArn,
    describeImageResponse_httpStatus,

    -- ** DescribeImageVersion
    describeImageVersion_alias,
    describeImageVersion_version,
    describeImageVersion_imageName,
    describeImageVersionResponse_baseImage,
    describeImageVersionResponse_containerImage,
    describeImageVersionResponse_creationTime,
    describeImageVersionResponse_failureReason,
    describeImageVersionResponse_horovod,
    describeImageVersionResponse_imageArn,
    describeImageVersionResponse_imageVersionArn,
    describeImageVersionResponse_imageVersionStatus,
    describeImageVersionResponse_jobType,
    describeImageVersionResponse_lastModifiedTime,
    describeImageVersionResponse_mLFramework,
    describeImageVersionResponse_processor,
    describeImageVersionResponse_programmingLang,
    describeImageVersionResponse_releaseNotes,
    describeImageVersionResponse_vendorGuidance,
    describeImageVersionResponse_version,
    describeImageVersionResponse_httpStatus,

    -- ** DescribeInferenceExperiment
    describeInferenceExperiment_name,
    describeInferenceExperimentResponse_completionTime,
    describeInferenceExperimentResponse_creationTime,
    describeInferenceExperimentResponse_dataStorageConfig,
    describeInferenceExperimentResponse_description,
    describeInferenceExperimentResponse_kmsKey,
    describeInferenceExperimentResponse_lastModifiedTime,
    describeInferenceExperimentResponse_roleArn,
    describeInferenceExperimentResponse_schedule,
    describeInferenceExperimentResponse_shadowModeConfig,
    describeInferenceExperimentResponse_statusReason,
    describeInferenceExperimentResponse_httpStatus,
    describeInferenceExperimentResponse_arn,
    describeInferenceExperimentResponse_name,
    describeInferenceExperimentResponse_type,
    describeInferenceExperimentResponse_status,
    describeInferenceExperimentResponse_endpointMetadata,
    describeInferenceExperimentResponse_modelVariants,

    -- ** DescribeInferenceRecommendationsJob
    describeInferenceRecommendationsJob_jobName,
    describeInferenceRecommendationsJobResponse_completionTime,
    describeInferenceRecommendationsJobResponse_endpointPerformances,
    describeInferenceRecommendationsJobResponse_failureReason,
    describeInferenceRecommendationsJobResponse_inferenceRecommendations,
    describeInferenceRecommendationsJobResponse_jobDescription,
    describeInferenceRecommendationsJobResponse_stoppingConditions,
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
    describeLabelingJobResponse_failureReason,
    describeLabelingJobResponse_labelAttributeName,
    describeLabelingJobResponse_labelCategoryConfigS3Uri,
    describeLabelingJobResponse_labelingJobAlgorithmsConfig,
    describeLabelingJobResponse_labelingJobOutput,
    describeLabelingJobResponse_stoppingConditions,
    describeLabelingJobResponse_tags,
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
    describeLineageGroupResponse_createdBy,
    describeLineageGroupResponse_creationTime,
    describeLineageGroupResponse_description,
    describeLineageGroupResponse_displayName,
    describeLineageGroupResponse_lastModifiedBy,
    describeLineageGroupResponse_lastModifiedTime,
    describeLineageGroupResponse_lineageGroupArn,
    describeLineageGroupResponse_lineageGroupName,
    describeLineageGroupResponse_httpStatus,

    -- ** DescribeModel
    describeModel_modelName,
    describeModelResponse_containers,
    describeModelResponse_enableNetworkIsolation,
    describeModelResponse_inferenceExecutionConfig,
    describeModelResponse_primaryContainer,
    describeModelResponse_vpcConfig,
    describeModelResponse_httpStatus,
    describeModelResponse_modelName,
    describeModelResponse_executionRoleArn,
    describeModelResponse_creationTime,
    describeModelResponse_modelArn,

    -- ** DescribeModelBiasJobDefinition
    describeModelBiasJobDefinition_jobDefinitionName,
    describeModelBiasJobDefinitionResponse_modelBiasBaselineConfig,
    describeModelBiasJobDefinitionResponse_networkConfig,
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

    -- ** DescribeModelCard
    describeModelCard_modelCardVersion,
    describeModelCard_modelCardName,
    describeModelCardResponse_lastModifiedBy,
    describeModelCardResponse_lastModifiedTime,
    describeModelCardResponse_modelCardProcessingStatus,
    describeModelCardResponse_securityConfig,
    describeModelCardResponse_httpStatus,
    describeModelCardResponse_modelCardArn,
    describeModelCardResponse_modelCardName,
    describeModelCardResponse_modelCardVersion,
    describeModelCardResponse_content,
    describeModelCardResponse_modelCardStatus,
    describeModelCardResponse_creationTime,
    describeModelCardResponse_createdBy,

    -- ** DescribeModelCardExportJob
    describeModelCardExportJob_modelCardExportJobArn,
    describeModelCardExportJobResponse_exportArtifacts,
    describeModelCardExportJobResponse_failureReason,
    describeModelCardExportJobResponse_httpStatus,
    describeModelCardExportJobResponse_modelCardExportJobName,
    describeModelCardExportJobResponse_modelCardExportJobArn,
    describeModelCardExportJobResponse_status,
    describeModelCardExportJobResponse_modelCardName,
    describeModelCardExportJobResponse_modelCardVersion,
    describeModelCardExportJobResponse_outputConfig,
    describeModelCardExportJobResponse_createdAt,
    describeModelCardExportJobResponse_lastModifiedAt,

    -- ** DescribeModelExplainabilityJobDefinition
    describeModelExplainabilityJobDefinition_jobDefinitionName,
    describeModelExplainabilityJobDefinitionResponse_modelExplainabilityBaselineConfig,
    describeModelExplainabilityJobDefinitionResponse_networkConfig,
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
    describeModelPackageResponse_additionalInferenceSpecifications,
    describeModelPackageResponse_approvalDescription,
    describeModelPackageResponse_certifyForMarketplace,
    describeModelPackageResponse_createdBy,
    describeModelPackageResponse_customerMetadataProperties,
    describeModelPackageResponse_domain,
    describeModelPackageResponse_driftCheckBaselines,
    describeModelPackageResponse_inferenceSpecification,
    describeModelPackageResponse_lastModifiedBy,
    describeModelPackageResponse_lastModifiedTime,
    describeModelPackageResponse_metadataProperties,
    describeModelPackageResponse_modelApprovalStatus,
    describeModelPackageResponse_modelMetrics,
    describeModelPackageResponse_modelPackageDescription,
    describeModelPackageResponse_modelPackageGroupName,
    describeModelPackageResponse_modelPackageVersion,
    describeModelPackageResponse_samplePayloadUrl,
    describeModelPackageResponse_sourceAlgorithmSpecification,
    describeModelPackageResponse_task,
    describeModelPackageResponse_validationSpecification,
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
    describeMonitoringScheduleResponse_failureReason,
    describeMonitoringScheduleResponse_lastMonitoringExecutionSummary,
    describeMonitoringScheduleResponse_monitoringType,
    describeMonitoringScheduleResponse_httpStatus,
    describeMonitoringScheduleResponse_monitoringScheduleArn,
    describeMonitoringScheduleResponse_monitoringScheduleName,
    describeMonitoringScheduleResponse_monitoringScheduleStatus,
    describeMonitoringScheduleResponse_creationTime,
    describeMonitoringScheduleResponse_lastModifiedTime,
    describeMonitoringScheduleResponse_monitoringScheduleConfig,

    -- ** DescribeNotebookInstance
    describeNotebookInstance_notebookInstanceName,
    describeNotebookInstanceResponse_acceleratorTypes,
    describeNotebookInstanceResponse_additionalCodeRepositories,
    describeNotebookInstanceResponse_creationTime,
    describeNotebookInstanceResponse_defaultCodeRepository,
    describeNotebookInstanceResponse_directInternetAccess,
    describeNotebookInstanceResponse_failureReason,
    describeNotebookInstanceResponse_instanceMetadataServiceConfiguration,
    describeNotebookInstanceResponse_instanceType,
    describeNotebookInstanceResponse_kmsKeyId,
    describeNotebookInstanceResponse_lastModifiedTime,
    describeNotebookInstanceResponse_networkInterfaceId,
    describeNotebookInstanceResponse_notebookInstanceArn,
    describeNotebookInstanceResponse_notebookInstanceLifecycleConfigName,
    describeNotebookInstanceResponse_notebookInstanceName,
    describeNotebookInstanceResponse_notebookInstanceStatus,
    describeNotebookInstanceResponse_platformIdentifier,
    describeNotebookInstanceResponse_roleArn,
    describeNotebookInstanceResponse_rootAccess,
    describeNotebookInstanceResponse_securityGroups,
    describeNotebookInstanceResponse_subnetId,
    describeNotebookInstanceResponse_url,
    describeNotebookInstanceResponse_volumeSizeInGB,
    describeNotebookInstanceResponse_httpStatus,

    -- ** DescribeNotebookInstanceLifecycleConfig
    describeNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,
    describeNotebookInstanceLifecycleConfigResponse_creationTime,
    describeNotebookInstanceLifecycleConfigResponse_lastModifiedTime,
    describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigArn,
    describeNotebookInstanceLifecycleConfigResponse_notebookInstanceLifecycleConfigName,
    describeNotebookInstanceLifecycleConfigResponse_onCreate,
    describeNotebookInstanceLifecycleConfigResponse_onStart,
    describeNotebookInstanceLifecycleConfigResponse_httpStatus,

    -- ** DescribePipeline
    describePipeline_pipelineName,
    describePipelineResponse_createdBy,
    describePipelineResponse_creationTime,
    describePipelineResponse_lastModifiedBy,
    describePipelineResponse_lastModifiedTime,
    describePipelineResponse_lastRunTime,
    describePipelineResponse_parallelismConfiguration,
    describePipelineResponse_pipelineArn,
    describePipelineResponse_pipelineDefinition,
    describePipelineResponse_pipelineDescription,
    describePipelineResponse_pipelineDisplayName,
    describePipelineResponse_pipelineName,
    describePipelineResponse_pipelineStatus,
    describePipelineResponse_roleArn,
    describePipelineResponse_httpStatus,

    -- ** DescribePipelineDefinitionForExecution
    describePipelineDefinitionForExecution_pipelineExecutionArn,
    describePipelineDefinitionForExecutionResponse_creationTime,
    describePipelineDefinitionForExecutionResponse_pipelineDefinition,
    describePipelineDefinitionForExecutionResponse_httpStatus,

    -- ** DescribePipelineExecution
    describePipelineExecution_pipelineExecutionArn,
    describePipelineExecutionResponse_createdBy,
    describePipelineExecutionResponse_creationTime,
    describePipelineExecutionResponse_failureReason,
    describePipelineExecutionResponse_lastModifiedBy,
    describePipelineExecutionResponse_lastModifiedTime,
    describePipelineExecutionResponse_parallelismConfiguration,
    describePipelineExecutionResponse_pipelineArn,
    describePipelineExecutionResponse_pipelineExecutionArn,
    describePipelineExecutionResponse_pipelineExecutionDescription,
    describePipelineExecutionResponse_pipelineExecutionDisplayName,
    describePipelineExecutionResponse_pipelineExecutionStatus,
    describePipelineExecutionResponse_pipelineExperimentConfig,
    describePipelineExecutionResponse_httpStatus,

    -- ** DescribeProcessingJob
    describeProcessingJob_processingJobName,
    describeProcessingJobResponse_autoMLJobArn,
    describeProcessingJobResponse_environment,
    describeProcessingJobResponse_exitMessage,
    describeProcessingJobResponse_experimentConfig,
    describeProcessingJobResponse_failureReason,
    describeProcessingJobResponse_lastModifiedTime,
    describeProcessingJobResponse_monitoringScheduleArn,
    describeProcessingJobResponse_networkConfig,
    describeProcessingJobResponse_processingEndTime,
    describeProcessingJobResponse_processingInputs,
    describeProcessingJobResponse_processingOutputConfig,
    describeProcessingJobResponse_processingStartTime,
    describeProcessingJobResponse_roleArn,
    describeProcessingJobResponse_stoppingCondition,
    describeProcessingJobResponse_trainingJobArn,
    describeProcessingJobResponse_httpStatus,
    describeProcessingJobResponse_processingJobName,
    describeProcessingJobResponse_processingResources,
    describeProcessingJobResponse_appSpecification,
    describeProcessingJobResponse_processingJobArn,
    describeProcessingJobResponse_processingJobStatus,
    describeProcessingJobResponse_creationTime,

    -- ** DescribeProject
    describeProject_projectName,
    describeProjectResponse_createdBy,
    describeProjectResponse_lastModifiedBy,
    describeProjectResponse_lastModifiedTime,
    describeProjectResponse_projectDescription,
    describeProjectResponse_serviceCatalogProvisionedProductDetails,
    describeProjectResponse_httpStatus,
    describeProjectResponse_projectArn,
    describeProjectResponse_projectName,
    describeProjectResponse_projectId,
    describeProjectResponse_serviceCatalogProvisioningDetails,
    describeProjectResponse_projectStatus,
    describeProjectResponse_creationTime,

    -- ** DescribeSpace
    describeSpace_domainId,
    describeSpace_spaceName,
    describeSpaceResponse_creationTime,
    describeSpaceResponse_domainId,
    describeSpaceResponse_failureReason,
    describeSpaceResponse_homeEfsFileSystemUid,
    describeSpaceResponse_lastModifiedTime,
    describeSpaceResponse_spaceArn,
    describeSpaceResponse_spaceName,
    describeSpaceResponse_spaceSettings,
    describeSpaceResponse_status,
    describeSpaceResponse_httpStatus,

    -- ** DescribeStudioLifecycleConfig
    describeStudioLifecycleConfig_studioLifecycleConfigName,
    describeStudioLifecycleConfigResponse_creationTime,
    describeStudioLifecycleConfigResponse_lastModifiedTime,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigAppType,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigArn,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigContent,
    describeStudioLifecycleConfigResponse_studioLifecycleConfigName,
    describeStudioLifecycleConfigResponse_httpStatus,

    -- ** DescribeSubscribedWorkteam
    describeSubscribedWorkteam_workteamArn,
    describeSubscribedWorkteamResponse_httpStatus,
    describeSubscribedWorkteamResponse_subscribedWorkteam,

    -- ** DescribeTrainingJob
    describeTrainingJob_trainingJobName,
    describeTrainingJobResponse_autoMLJobArn,
    describeTrainingJobResponse_billableTimeInSeconds,
    describeTrainingJobResponse_checkpointConfig,
    describeTrainingJobResponse_debugHookConfig,
    describeTrainingJobResponse_debugRuleConfigurations,
    describeTrainingJobResponse_debugRuleEvaluationStatuses,
    describeTrainingJobResponse_enableInterContainerTrafficEncryption,
    describeTrainingJobResponse_enableManagedSpotTraining,
    describeTrainingJobResponse_enableNetworkIsolation,
    describeTrainingJobResponse_environment,
    describeTrainingJobResponse_experimentConfig,
    describeTrainingJobResponse_failureReason,
    describeTrainingJobResponse_finalMetricDataList,
    describeTrainingJobResponse_hyperParameters,
    describeTrainingJobResponse_inputDataConfig,
    describeTrainingJobResponse_labelingJobArn,
    describeTrainingJobResponse_lastModifiedTime,
    describeTrainingJobResponse_outputDataConfig,
    describeTrainingJobResponse_profilerConfig,
    describeTrainingJobResponse_profilerRuleConfigurations,
    describeTrainingJobResponse_profilerRuleEvaluationStatuses,
    describeTrainingJobResponse_profilingStatus,
    describeTrainingJobResponse_retryStrategy,
    describeTrainingJobResponse_roleArn,
    describeTrainingJobResponse_secondaryStatusTransitions,
    describeTrainingJobResponse_tensorBoardOutputConfig,
    describeTrainingJobResponse_trainingEndTime,
    describeTrainingJobResponse_trainingStartTime,
    describeTrainingJobResponse_trainingTimeInSeconds,
    describeTrainingJobResponse_tuningJobArn,
    describeTrainingJobResponse_vpcConfig,
    describeTrainingJobResponse_warmPoolStatus,
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
    describeTransformJobResponse_autoMLJobArn,
    describeTransformJobResponse_batchStrategy,
    describeTransformJobResponse_dataCaptureConfig,
    describeTransformJobResponse_dataProcessing,
    describeTransformJobResponse_environment,
    describeTransformJobResponse_experimentConfig,
    describeTransformJobResponse_failureReason,
    describeTransformJobResponse_labelingJobArn,
    describeTransformJobResponse_maxConcurrentTransforms,
    describeTransformJobResponse_maxPayloadInMB,
    describeTransformJobResponse_modelClientConfig,
    describeTransformJobResponse_transformEndTime,
    describeTransformJobResponse_transformOutput,
    describeTransformJobResponse_transformStartTime,
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
    describeTrialResponse_createdBy,
    describeTrialResponse_creationTime,
    describeTrialResponse_displayName,
    describeTrialResponse_experimentName,
    describeTrialResponse_lastModifiedBy,
    describeTrialResponse_lastModifiedTime,
    describeTrialResponse_metadataProperties,
    describeTrialResponse_source,
    describeTrialResponse_trialArn,
    describeTrialResponse_trialName,
    describeTrialResponse_httpStatus,

    -- ** DescribeTrialComponent
    describeTrialComponent_trialComponentName,
    describeTrialComponentResponse_createdBy,
    describeTrialComponentResponse_creationTime,
    describeTrialComponentResponse_displayName,
    describeTrialComponentResponse_endTime,
    describeTrialComponentResponse_inputArtifacts,
    describeTrialComponentResponse_lastModifiedBy,
    describeTrialComponentResponse_lastModifiedTime,
    describeTrialComponentResponse_lineageGroupArn,
    describeTrialComponentResponse_metadataProperties,
    describeTrialComponentResponse_metrics,
    describeTrialComponentResponse_outputArtifacts,
    describeTrialComponentResponse_parameters,
    describeTrialComponentResponse_source,
    describeTrialComponentResponse_sources,
    describeTrialComponentResponse_startTime,
    describeTrialComponentResponse_status,
    describeTrialComponentResponse_trialComponentArn,
    describeTrialComponentResponse_trialComponentName,
    describeTrialComponentResponse_httpStatus,

    -- ** DescribeUserProfile
    describeUserProfile_domainId,
    describeUserProfile_userProfileName,
    describeUserProfileResponse_creationTime,
    describeUserProfileResponse_domainId,
    describeUserProfileResponse_failureReason,
    describeUserProfileResponse_homeEfsFileSystemUid,
    describeUserProfileResponse_lastModifiedTime,
    describeUserProfileResponse_singleSignOnUserIdentifier,
    describeUserProfileResponse_singleSignOnUserValue,
    describeUserProfileResponse_status,
    describeUserProfileResponse_userProfileArn,
    describeUserProfileResponse_userProfileName,
    describeUserProfileResponse_userSettings,
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
    disassociateTrialComponentResponse_trialArn,
    disassociateTrialComponentResponse_trialComponentArn,
    disassociateTrialComponentResponse_httpStatus,

    -- ** EnableSagemakerServicecatalogPortfolio
    enableSagemakerServicecatalogPortfolioResponse_httpStatus,

    -- ** GetDeviceFleetReport
    getDeviceFleetReport_deviceFleetName,
    getDeviceFleetReportResponse_agentVersions,
    getDeviceFleetReportResponse_description,
    getDeviceFleetReportResponse_deviceStats,
    getDeviceFleetReportResponse_modelStats,
    getDeviceFleetReportResponse_outputConfig,
    getDeviceFleetReportResponse_reportGenerated,
    getDeviceFleetReportResponse_httpStatus,
    getDeviceFleetReportResponse_deviceFleetArn,
    getDeviceFleetReportResponse_deviceFleetName,

    -- ** GetLineageGroupPolicy
    getLineageGroupPolicy_lineageGroupName,
    getLineageGroupPolicyResponse_lineageGroupArn,
    getLineageGroupPolicyResponse_resourcePolicy,
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

    -- ** ImportHubContent
    importHubContent_hubContentDescription,
    importHubContent_hubContentDisplayName,
    importHubContent_hubContentMarkdown,
    importHubContent_hubContentSearchKeywords,
    importHubContent_hubContentVersion,
    importHubContent_tags,
    importHubContent_hubContentName,
    importHubContent_hubContentType,
    importHubContent_documentSchemaVersion,
    importHubContent_hubName,
    importHubContent_hubContentDocument,
    importHubContentResponse_httpStatus,
    importHubContentResponse_hubArn,
    importHubContentResponse_hubContentArn,

    -- ** ListActions
    listActions_actionType,
    listActions_createdAfter,
    listActions_createdBefore,
    listActions_maxResults,
    listActions_nextToken,
    listActions_sortBy,
    listActions_sortOrder,
    listActions_sourceUri,
    listActionsResponse_actionSummaries,
    listActionsResponse_nextToken,
    listActionsResponse_httpStatus,

    -- ** ListAlgorithms
    listAlgorithms_creationTimeAfter,
    listAlgorithms_creationTimeBefore,
    listAlgorithms_maxResults,
    listAlgorithms_nameContains,
    listAlgorithms_nextToken,
    listAlgorithms_sortBy,
    listAlgorithms_sortOrder,
    listAlgorithmsResponse_nextToken,
    listAlgorithmsResponse_httpStatus,
    listAlgorithmsResponse_algorithmSummaryList,

    -- ** ListAliases
    listAliases_alias,
    listAliases_maxResults,
    listAliases_nextToken,
    listAliases_version,
    listAliases_imageName,
    listAliasesResponse_nextToken,
    listAliasesResponse_sageMakerImageVersionAliases,
    listAliasesResponse_httpStatus,

    -- ** ListAppImageConfigs
    listAppImageConfigs_creationTimeAfter,
    listAppImageConfigs_creationTimeBefore,
    listAppImageConfigs_maxResults,
    listAppImageConfigs_modifiedTimeAfter,
    listAppImageConfigs_modifiedTimeBefore,
    listAppImageConfigs_nameContains,
    listAppImageConfigs_nextToken,
    listAppImageConfigs_sortBy,
    listAppImageConfigs_sortOrder,
    listAppImageConfigsResponse_appImageConfigs,
    listAppImageConfigsResponse_nextToken,
    listAppImageConfigsResponse_httpStatus,

    -- ** ListApps
    listApps_domainIdEquals,
    listApps_maxResults,
    listApps_nextToken,
    listApps_sortBy,
    listApps_sortOrder,
    listApps_spaceNameEquals,
    listApps_userProfileNameEquals,
    listAppsResponse_apps,
    listAppsResponse_nextToken,
    listAppsResponse_httpStatus,

    -- ** ListArtifacts
    listArtifacts_artifactType,
    listArtifacts_createdAfter,
    listArtifacts_createdBefore,
    listArtifacts_maxResults,
    listArtifacts_nextToken,
    listArtifacts_sortBy,
    listArtifacts_sortOrder,
    listArtifacts_sourceUri,
    listArtifactsResponse_artifactSummaries,
    listArtifactsResponse_nextToken,
    listArtifactsResponse_httpStatus,

    -- ** ListAssociations
    listAssociations_associationType,
    listAssociations_createdAfter,
    listAssociations_createdBefore,
    listAssociations_destinationArn,
    listAssociations_destinationType,
    listAssociations_maxResults,
    listAssociations_nextToken,
    listAssociations_sortBy,
    listAssociations_sortOrder,
    listAssociations_sourceArn,
    listAssociations_sourceType,
    listAssociationsResponse_associationSummaries,
    listAssociationsResponse_nextToken,
    listAssociationsResponse_httpStatus,

    -- ** ListAutoMLJobs
    listAutoMLJobs_creationTimeAfter,
    listAutoMLJobs_creationTimeBefore,
    listAutoMLJobs_lastModifiedTimeAfter,
    listAutoMLJobs_lastModifiedTimeBefore,
    listAutoMLJobs_maxResults,
    listAutoMLJobs_nameContains,
    listAutoMLJobs_nextToken,
    listAutoMLJobs_sortBy,
    listAutoMLJobs_sortOrder,
    listAutoMLJobs_statusEquals,
    listAutoMLJobsResponse_nextToken,
    listAutoMLJobsResponse_httpStatus,
    listAutoMLJobsResponse_autoMLJobSummaries,

    -- ** ListCandidatesForAutoMLJob
    listCandidatesForAutoMLJob_candidateNameEquals,
    listCandidatesForAutoMLJob_maxResults,
    listCandidatesForAutoMLJob_nextToken,
    listCandidatesForAutoMLJob_sortBy,
    listCandidatesForAutoMLJob_sortOrder,
    listCandidatesForAutoMLJob_statusEquals,
    listCandidatesForAutoMLJob_autoMLJobName,
    listCandidatesForAutoMLJobResponse_nextToken,
    listCandidatesForAutoMLJobResponse_httpStatus,
    listCandidatesForAutoMLJobResponse_candidates,

    -- ** ListCodeRepositories
    listCodeRepositories_creationTimeAfter,
    listCodeRepositories_creationTimeBefore,
    listCodeRepositories_lastModifiedTimeAfter,
    listCodeRepositories_lastModifiedTimeBefore,
    listCodeRepositories_maxResults,
    listCodeRepositories_nameContains,
    listCodeRepositories_nextToken,
    listCodeRepositories_sortBy,
    listCodeRepositories_sortOrder,
    listCodeRepositoriesResponse_nextToken,
    listCodeRepositoriesResponse_httpStatus,
    listCodeRepositoriesResponse_codeRepositorySummaryList,

    -- ** ListCompilationJobs
    listCompilationJobs_creationTimeAfter,
    listCompilationJobs_creationTimeBefore,
    listCompilationJobs_lastModifiedTimeAfter,
    listCompilationJobs_lastModifiedTimeBefore,
    listCompilationJobs_maxResults,
    listCompilationJobs_nameContains,
    listCompilationJobs_nextToken,
    listCompilationJobs_sortBy,
    listCompilationJobs_sortOrder,
    listCompilationJobs_statusEquals,
    listCompilationJobsResponse_nextToken,
    listCompilationJobsResponse_httpStatus,
    listCompilationJobsResponse_compilationJobSummaries,

    -- ** ListContexts
    listContexts_contextType,
    listContexts_createdAfter,
    listContexts_createdBefore,
    listContexts_maxResults,
    listContexts_nextToken,
    listContexts_sortBy,
    listContexts_sortOrder,
    listContexts_sourceUri,
    listContextsResponse_contextSummaries,
    listContextsResponse_nextToken,
    listContextsResponse_httpStatus,

    -- ** ListDataQualityJobDefinitions
    listDataQualityJobDefinitions_creationTimeAfter,
    listDataQualityJobDefinitions_creationTimeBefore,
    listDataQualityJobDefinitions_endpointName,
    listDataQualityJobDefinitions_maxResults,
    listDataQualityJobDefinitions_nameContains,
    listDataQualityJobDefinitions_nextToken,
    listDataQualityJobDefinitions_sortBy,
    listDataQualityJobDefinitions_sortOrder,
    listDataQualityJobDefinitionsResponse_nextToken,
    listDataQualityJobDefinitionsResponse_httpStatus,
    listDataQualityJobDefinitionsResponse_jobDefinitionSummaries,

    -- ** ListDeviceFleets
    listDeviceFleets_creationTimeAfter,
    listDeviceFleets_creationTimeBefore,
    listDeviceFleets_lastModifiedTimeAfter,
    listDeviceFleets_lastModifiedTimeBefore,
    listDeviceFleets_maxResults,
    listDeviceFleets_nameContains,
    listDeviceFleets_nextToken,
    listDeviceFleets_sortBy,
    listDeviceFleets_sortOrder,
    listDeviceFleetsResponse_nextToken,
    listDeviceFleetsResponse_httpStatus,
    listDeviceFleetsResponse_deviceFleetSummaries,

    -- ** ListDevices
    listDevices_deviceFleetName,
    listDevices_latestHeartbeatAfter,
    listDevices_maxResults,
    listDevices_modelName,
    listDevices_nextToken,
    listDevicesResponse_nextToken,
    listDevicesResponse_httpStatus,
    listDevicesResponse_deviceSummaries,

    -- ** ListDomains
    listDomains_maxResults,
    listDomains_nextToken,
    listDomainsResponse_domains,
    listDomainsResponse_nextToken,
    listDomainsResponse_httpStatus,

    -- ** ListEdgeDeploymentPlans
    listEdgeDeploymentPlans_creationTimeAfter,
    listEdgeDeploymentPlans_creationTimeBefore,
    listEdgeDeploymentPlans_deviceFleetNameContains,
    listEdgeDeploymentPlans_lastModifiedTimeAfter,
    listEdgeDeploymentPlans_lastModifiedTimeBefore,
    listEdgeDeploymentPlans_maxResults,
    listEdgeDeploymentPlans_nameContains,
    listEdgeDeploymentPlans_nextToken,
    listEdgeDeploymentPlans_sortBy,
    listEdgeDeploymentPlans_sortOrder,
    listEdgeDeploymentPlansResponse_nextToken,
    listEdgeDeploymentPlansResponse_httpStatus,
    listEdgeDeploymentPlansResponse_edgeDeploymentPlanSummaries,

    -- ** ListEdgePackagingJobs
    listEdgePackagingJobs_creationTimeAfter,
    listEdgePackagingJobs_creationTimeBefore,
    listEdgePackagingJobs_lastModifiedTimeAfter,
    listEdgePackagingJobs_lastModifiedTimeBefore,
    listEdgePackagingJobs_maxResults,
    listEdgePackagingJobs_modelNameContains,
    listEdgePackagingJobs_nameContains,
    listEdgePackagingJobs_nextToken,
    listEdgePackagingJobs_sortBy,
    listEdgePackagingJobs_sortOrder,
    listEdgePackagingJobs_statusEquals,
    listEdgePackagingJobsResponse_nextToken,
    listEdgePackagingJobsResponse_httpStatus,
    listEdgePackagingJobsResponse_edgePackagingJobSummaries,

    -- ** ListEndpointConfigs
    listEndpointConfigs_creationTimeAfter,
    listEndpointConfigs_creationTimeBefore,
    listEndpointConfigs_maxResults,
    listEndpointConfigs_nameContains,
    listEndpointConfigs_nextToken,
    listEndpointConfigs_sortBy,
    listEndpointConfigs_sortOrder,
    listEndpointConfigsResponse_nextToken,
    listEndpointConfigsResponse_httpStatus,
    listEndpointConfigsResponse_endpointConfigs,

    -- ** ListEndpoints
    listEndpoints_creationTimeAfter,
    listEndpoints_creationTimeBefore,
    listEndpoints_lastModifiedTimeAfter,
    listEndpoints_lastModifiedTimeBefore,
    listEndpoints_maxResults,
    listEndpoints_nameContains,
    listEndpoints_nextToken,
    listEndpoints_sortBy,
    listEndpoints_sortOrder,
    listEndpoints_statusEquals,
    listEndpointsResponse_nextToken,
    listEndpointsResponse_httpStatus,
    listEndpointsResponse_endpoints,

    -- ** ListExperiments
    listExperiments_createdAfter,
    listExperiments_createdBefore,
    listExperiments_maxResults,
    listExperiments_nextToken,
    listExperiments_sortBy,
    listExperiments_sortOrder,
    listExperimentsResponse_experimentSummaries,
    listExperimentsResponse_nextToken,
    listExperimentsResponse_httpStatus,

    -- ** ListFeatureGroups
    listFeatureGroups_creationTimeAfter,
    listFeatureGroups_creationTimeBefore,
    listFeatureGroups_featureGroupStatusEquals,
    listFeatureGroups_maxResults,
    listFeatureGroups_nameContains,
    listFeatureGroups_nextToken,
    listFeatureGroups_offlineStoreStatusEquals,
    listFeatureGroups_sortBy,
    listFeatureGroups_sortOrder,
    listFeatureGroupsResponse_nextToken,
    listFeatureGroupsResponse_httpStatus,
    listFeatureGroupsResponse_featureGroupSummaries,

    -- ** ListFlowDefinitions
    listFlowDefinitions_creationTimeAfter,
    listFlowDefinitions_creationTimeBefore,
    listFlowDefinitions_maxResults,
    listFlowDefinitions_nextToken,
    listFlowDefinitions_sortOrder,
    listFlowDefinitionsResponse_nextToken,
    listFlowDefinitionsResponse_httpStatus,
    listFlowDefinitionsResponse_flowDefinitionSummaries,

    -- ** ListHubContentVersions
    listHubContentVersions_creationTimeAfter,
    listHubContentVersions_creationTimeBefore,
    listHubContentVersions_maxResults,
    listHubContentVersions_maxSchemaVersion,
    listHubContentVersions_minVersion,
    listHubContentVersions_nextToken,
    listHubContentVersions_sortBy,
    listHubContentVersions_sortOrder,
    listHubContentVersions_hubName,
    listHubContentVersions_hubContentType,
    listHubContentVersions_hubContentName,
    listHubContentVersionsResponse_nextToken,
    listHubContentVersionsResponse_httpStatus,
    listHubContentVersionsResponse_hubContentSummaries,

    -- ** ListHubContents
    listHubContents_creationTimeAfter,
    listHubContents_creationTimeBefore,
    listHubContents_maxResults,
    listHubContents_maxSchemaVersion,
    listHubContents_nameContains,
    listHubContents_nextToken,
    listHubContents_sortBy,
    listHubContents_sortOrder,
    listHubContents_hubName,
    listHubContents_hubContentType,
    listHubContentsResponse_nextToken,
    listHubContentsResponse_httpStatus,
    listHubContentsResponse_hubContentSummaries,

    -- ** ListHubs
    listHubs_creationTimeAfter,
    listHubs_creationTimeBefore,
    listHubs_lastModifiedTimeAfter,
    listHubs_lastModifiedTimeBefore,
    listHubs_maxResults,
    listHubs_nameContains,
    listHubs_nextToken,
    listHubs_sortBy,
    listHubs_sortOrder,
    listHubsResponse_nextToken,
    listHubsResponse_httpStatus,
    listHubsResponse_hubSummaries,

    -- ** ListHumanTaskUis
    listHumanTaskUis_creationTimeAfter,
    listHumanTaskUis_creationTimeBefore,
    listHumanTaskUis_maxResults,
    listHumanTaskUis_nextToken,
    listHumanTaskUis_sortOrder,
    listHumanTaskUisResponse_nextToken,
    listHumanTaskUisResponse_httpStatus,
    listHumanTaskUisResponse_humanTaskUiSummaries,

    -- ** ListHyperParameterTuningJobs
    listHyperParameterTuningJobs_creationTimeAfter,
    listHyperParameterTuningJobs_creationTimeBefore,
    listHyperParameterTuningJobs_lastModifiedTimeAfter,
    listHyperParameterTuningJobs_lastModifiedTimeBefore,
    listHyperParameterTuningJobs_maxResults,
    listHyperParameterTuningJobs_nameContains,
    listHyperParameterTuningJobs_nextToken,
    listHyperParameterTuningJobs_sortBy,
    listHyperParameterTuningJobs_sortOrder,
    listHyperParameterTuningJobs_statusEquals,
    listHyperParameterTuningJobsResponse_nextToken,
    listHyperParameterTuningJobsResponse_httpStatus,
    listHyperParameterTuningJobsResponse_hyperParameterTuningJobSummaries,

    -- ** ListImageVersions
    listImageVersions_creationTimeAfter,
    listImageVersions_creationTimeBefore,
    listImageVersions_lastModifiedTimeAfter,
    listImageVersions_lastModifiedTimeBefore,
    listImageVersions_maxResults,
    listImageVersions_nextToken,
    listImageVersions_sortBy,
    listImageVersions_sortOrder,
    listImageVersions_imageName,
    listImageVersionsResponse_imageVersions,
    listImageVersionsResponse_nextToken,
    listImageVersionsResponse_httpStatus,

    -- ** ListImages
    listImages_creationTimeAfter,
    listImages_creationTimeBefore,
    listImages_lastModifiedTimeAfter,
    listImages_lastModifiedTimeBefore,
    listImages_maxResults,
    listImages_nameContains,
    listImages_nextToken,
    listImages_sortBy,
    listImages_sortOrder,
    listImagesResponse_images,
    listImagesResponse_nextToken,
    listImagesResponse_httpStatus,

    -- ** ListInferenceExperiments
    listInferenceExperiments_creationTimeAfter,
    listInferenceExperiments_creationTimeBefore,
    listInferenceExperiments_lastModifiedTimeAfter,
    listInferenceExperiments_lastModifiedTimeBefore,
    listInferenceExperiments_maxResults,
    listInferenceExperiments_nameContains,
    listInferenceExperiments_nextToken,
    listInferenceExperiments_sortBy,
    listInferenceExperiments_sortOrder,
    listInferenceExperiments_statusEquals,
    listInferenceExperiments_type,
    listInferenceExperimentsResponse_inferenceExperiments,
    listInferenceExperimentsResponse_nextToken,
    listInferenceExperimentsResponse_httpStatus,

    -- ** ListInferenceRecommendationsJobSteps
    listInferenceRecommendationsJobSteps_maxResults,
    listInferenceRecommendationsJobSteps_nextToken,
    listInferenceRecommendationsJobSteps_status,
    listInferenceRecommendationsJobSteps_stepType,
    listInferenceRecommendationsJobSteps_jobName,
    listInferenceRecommendationsJobStepsResponse_nextToken,
    listInferenceRecommendationsJobStepsResponse_steps,
    listInferenceRecommendationsJobStepsResponse_httpStatus,

    -- ** ListInferenceRecommendationsJobs
    listInferenceRecommendationsJobs_creationTimeAfter,
    listInferenceRecommendationsJobs_creationTimeBefore,
    listInferenceRecommendationsJobs_lastModifiedTimeAfter,
    listInferenceRecommendationsJobs_lastModifiedTimeBefore,
    listInferenceRecommendationsJobs_maxResults,
    listInferenceRecommendationsJobs_nameContains,
    listInferenceRecommendationsJobs_nextToken,
    listInferenceRecommendationsJobs_sortBy,
    listInferenceRecommendationsJobs_sortOrder,
    listInferenceRecommendationsJobs_statusEquals,
    listInferenceRecommendationsJobsResponse_nextToken,
    listInferenceRecommendationsJobsResponse_httpStatus,
    listInferenceRecommendationsJobsResponse_inferenceRecommendationsJobs,

    -- ** ListLabelingJobs
    listLabelingJobs_creationTimeAfter,
    listLabelingJobs_creationTimeBefore,
    listLabelingJobs_lastModifiedTimeAfter,
    listLabelingJobs_lastModifiedTimeBefore,
    listLabelingJobs_maxResults,
    listLabelingJobs_nameContains,
    listLabelingJobs_nextToken,
    listLabelingJobs_sortBy,
    listLabelingJobs_sortOrder,
    listLabelingJobs_statusEquals,
    listLabelingJobsResponse_labelingJobSummaryList,
    listLabelingJobsResponse_nextToken,
    listLabelingJobsResponse_httpStatus,

    -- ** ListLabelingJobsForWorkteam
    listLabelingJobsForWorkteam_creationTimeAfter,
    listLabelingJobsForWorkteam_creationTimeBefore,
    listLabelingJobsForWorkteam_jobReferenceCodeContains,
    listLabelingJobsForWorkteam_maxResults,
    listLabelingJobsForWorkteam_nextToken,
    listLabelingJobsForWorkteam_sortBy,
    listLabelingJobsForWorkteam_sortOrder,
    listLabelingJobsForWorkteam_workteamArn,
    listLabelingJobsForWorkteamResponse_nextToken,
    listLabelingJobsForWorkteamResponse_httpStatus,
    listLabelingJobsForWorkteamResponse_labelingJobSummaryList,

    -- ** ListLineageGroups
    listLineageGroups_createdAfter,
    listLineageGroups_createdBefore,
    listLineageGroups_maxResults,
    listLineageGroups_nextToken,
    listLineageGroups_sortBy,
    listLineageGroups_sortOrder,
    listLineageGroupsResponse_lineageGroupSummaries,
    listLineageGroupsResponse_nextToken,
    listLineageGroupsResponse_httpStatus,

    -- ** ListModelBiasJobDefinitions
    listModelBiasJobDefinitions_creationTimeAfter,
    listModelBiasJobDefinitions_creationTimeBefore,
    listModelBiasJobDefinitions_endpointName,
    listModelBiasJobDefinitions_maxResults,
    listModelBiasJobDefinitions_nameContains,
    listModelBiasJobDefinitions_nextToken,
    listModelBiasJobDefinitions_sortBy,
    listModelBiasJobDefinitions_sortOrder,
    listModelBiasJobDefinitionsResponse_nextToken,
    listModelBiasJobDefinitionsResponse_httpStatus,
    listModelBiasJobDefinitionsResponse_jobDefinitionSummaries,

    -- ** ListModelCardExportJobs
    listModelCardExportJobs_creationTimeAfter,
    listModelCardExportJobs_creationTimeBefore,
    listModelCardExportJobs_maxResults,
    listModelCardExportJobs_modelCardExportJobNameContains,
    listModelCardExportJobs_modelCardVersion,
    listModelCardExportJobs_nextToken,
    listModelCardExportJobs_sortBy,
    listModelCardExportJobs_sortOrder,
    listModelCardExportJobs_statusEquals,
    listModelCardExportJobs_modelCardName,
    listModelCardExportJobsResponse_nextToken,
    listModelCardExportJobsResponse_httpStatus,
    listModelCardExportJobsResponse_modelCardExportJobSummaries,

    -- ** ListModelCardVersions
    listModelCardVersions_creationTimeAfter,
    listModelCardVersions_creationTimeBefore,
    listModelCardVersions_maxResults,
    listModelCardVersions_modelCardStatus,
    listModelCardVersions_nextToken,
    listModelCardVersions_sortBy,
    listModelCardVersions_sortOrder,
    listModelCardVersions_modelCardName,
    listModelCardVersionsResponse_nextToken,
    listModelCardVersionsResponse_httpStatus,
    listModelCardVersionsResponse_modelCardVersionSummaryList,

    -- ** ListModelCards
    listModelCards_creationTimeAfter,
    listModelCards_creationTimeBefore,
    listModelCards_maxResults,
    listModelCards_modelCardStatus,
    listModelCards_nameContains,
    listModelCards_nextToken,
    listModelCards_sortBy,
    listModelCards_sortOrder,
    listModelCardsResponse_nextToken,
    listModelCardsResponse_httpStatus,
    listModelCardsResponse_modelCardSummaries,

    -- ** ListModelExplainabilityJobDefinitions
    listModelExplainabilityJobDefinitions_creationTimeAfter,
    listModelExplainabilityJobDefinitions_creationTimeBefore,
    listModelExplainabilityJobDefinitions_endpointName,
    listModelExplainabilityJobDefinitions_maxResults,
    listModelExplainabilityJobDefinitions_nameContains,
    listModelExplainabilityJobDefinitions_nextToken,
    listModelExplainabilityJobDefinitions_sortBy,
    listModelExplainabilityJobDefinitions_sortOrder,
    listModelExplainabilityJobDefinitionsResponse_nextToken,
    listModelExplainabilityJobDefinitionsResponse_httpStatus,
    listModelExplainabilityJobDefinitionsResponse_jobDefinitionSummaries,

    -- ** ListModelMetadata
    listModelMetadata_maxResults,
    listModelMetadata_nextToken,
    listModelMetadata_searchExpression,
    listModelMetadataResponse_nextToken,
    listModelMetadataResponse_httpStatus,
    listModelMetadataResponse_modelMetadataSummaries,

    -- ** ListModelPackageGroups
    listModelPackageGroups_creationTimeAfter,
    listModelPackageGroups_creationTimeBefore,
    listModelPackageGroups_maxResults,
    listModelPackageGroups_nameContains,
    listModelPackageGroups_nextToken,
    listModelPackageGroups_sortBy,
    listModelPackageGroups_sortOrder,
    listModelPackageGroupsResponse_nextToken,
    listModelPackageGroupsResponse_httpStatus,
    listModelPackageGroupsResponse_modelPackageGroupSummaryList,

    -- ** ListModelPackages
    listModelPackages_creationTimeAfter,
    listModelPackages_creationTimeBefore,
    listModelPackages_maxResults,
    listModelPackages_modelApprovalStatus,
    listModelPackages_modelPackageGroupName,
    listModelPackages_modelPackageType,
    listModelPackages_nameContains,
    listModelPackages_nextToken,
    listModelPackages_sortBy,
    listModelPackages_sortOrder,
    listModelPackagesResponse_nextToken,
    listModelPackagesResponse_httpStatus,
    listModelPackagesResponse_modelPackageSummaryList,

    -- ** ListModelQualityJobDefinitions
    listModelQualityJobDefinitions_creationTimeAfter,
    listModelQualityJobDefinitions_creationTimeBefore,
    listModelQualityJobDefinitions_endpointName,
    listModelQualityJobDefinitions_maxResults,
    listModelQualityJobDefinitions_nameContains,
    listModelQualityJobDefinitions_nextToken,
    listModelQualityJobDefinitions_sortBy,
    listModelQualityJobDefinitions_sortOrder,
    listModelQualityJobDefinitionsResponse_nextToken,
    listModelQualityJobDefinitionsResponse_httpStatus,
    listModelQualityJobDefinitionsResponse_jobDefinitionSummaries,

    -- ** ListModels
    listModels_creationTimeAfter,
    listModels_creationTimeBefore,
    listModels_maxResults,
    listModels_nameContains,
    listModels_nextToken,
    listModels_sortBy,
    listModels_sortOrder,
    listModelsResponse_nextToken,
    listModelsResponse_httpStatus,
    listModelsResponse_models,

    -- ** ListMonitoringAlertHistory
    listMonitoringAlertHistory_creationTimeAfter,
    listMonitoringAlertHistory_creationTimeBefore,
    listMonitoringAlertHistory_maxResults,
    listMonitoringAlertHistory_monitoringAlertName,
    listMonitoringAlertHistory_monitoringScheduleName,
    listMonitoringAlertHistory_nextToken,
    listMonitoringAlertHistory_sortBy,
    listMonitoringAlertHistory_sortOrder,
    listMonitoringAlertHistory_statusEquals,
    listMonitoringAlertHistoryResponse_monitoringAlertHistory,
    listMonitoringAlertHistoryResponse_nextToken,
    listMonitoringAlertHistoryResponse_httpStatus,

    -- ** ListMonitoringAlerts
    listMonitoringAlerts_maxResults,
    listMonitoringAlerts_nextToken,
    listMonitoringAlerts_monitoringScheduleName,
    listMonitoringAlertsResponse_monitoringAlertSummaries,
    listMonitoringAlertsResponse_nextToken,
    listMonitoringAlertsResponse_httpStatus,

    -- ** ListMonitoringExecutions
    listMonitoringExecutions_creationTimeAfter,
    listMonitoringExecutions_creationTimeBefore,
    listMonitoringExecutions_endpointName,
    listMonitoringExecutions_lastModifiedTimeAfter,
    listMonitoringExecutions_lastModifiedTimeBefore,
    listMonitoringExecutions_maxResults,
    listMonitoringExecutions_monitoringJobDefinitionName,
    listMonitoringExecutions_monitoringScheduleName,
    listMonitoringExecutions_monitoringTypeEquals,
    listMonitoringExecutions_nextToken,
    listMonitoringExecutions_scheduledTimeAfter,
    listMonitoringExecutions_scheduledTimeBefore,
    listMonitoringExecutions_sortBy,
    listMonitoringExecutions_sortOrder,
    listMonitoringExecutions_statusEquals,
    listMonitoringExecutionsResponse_nextToken,
    listMonitoringExecutionsResponse_httpStatus,
    listMonitoringExecutionsResponse_monitoringExecutionSummaries,

    -- ** ListMonitoringSchedules
    listMonitoringSchedules_creationTimeAfter,
    listMonitoringSchedules_creationTimeBefore,
    listMonitoringSchedules_endpointName,
    listMonitoringSchedules_lastModifiedTimeAfter,
    listMonitoringSchedules_lastModifiedTimeBefore,
    listMonitoringSchedules_maxResults,
    listMonitoringSchedules_monitoringJobDefinitionName,
    listMonitoringSchedules_monitoringTypeEquals,
    listMonitoringSchedules_nameContains,
    listMonitoringSchedules_nextToken,
    listMonitoringSchedules_sortBy,
    listMonitoringSchedules_sortOrder,
    listMonitoringSchedules_statusEquals,
    listMonitoringSchedulesResponse_nextToken,
    listMonitoringSchedulesResponse_httpStatus,
    listMonitoringSchedulesResponse_monitoringScheduleSummaries,

    -- ** ListNotebookInstanceLifecycleConfigs
    listNotebookInstanceLifecycleConfigs_creationTimeAfter,
    listNotebookInstanceLifecycleConfigs_creationTimeBefore,
    listNotebookInstanceLifecycleConfigs_lastModifiedTimeAfter,
    listNotebookInstanceLifecycleConfigs_lastModifiedTimeBefore,
    listNotebookInstanceLifecycleConfigs_maxResults,
    listNotebookInstanceLifecycleConfigs_nameContains,
    listNotebookInstanceLifecycleConfigs_nextToken,
    listNotebookInstanceLifecycleConfigs_sortBy,
    listNotebookInstanceLifecycleConfigs_sortOrder,
    listNotebookInstanceLifecycleConfigsResponse_nextToken,
    listNotebookInstanceLifecycleConfigsResponse_notebookInstanceLifecycleConfigs,
    listNotebookInstanceLifecycleConfigsResponse_httpStatus,

    -- ** ListNotebookInstances
    listNotebookInstances_additionalCodeRepositoryEquals,
    listNotebookInstances_creationTimeAfter,
    listNotebookInstances_creationTimeBefore,
    listNotebookInstances_defaultCodeRepositoryContains,
    listNotebookInstances_lastModifiedTimeAfter,
    listNotebookInstances_lastModifiedTimeBefore,
    listNotebookInstances_maxResults,
    listNotebookInstances_nameContains,
    listNotebookInstances_nextToken,
    listNotebookInstances_notebookInstanceLifecycleConfigNameContains,
    listNotebookInstances_sortBy,
    listNotebookInstances_sortOrder,
    listNotebookInstances_statusEquals,
    listNotebookInstancesResponse_nextToken,
    listNotebookInstancesResponse_notebookInstances,
    listNotebookInstancesResponse_httpStatus,

    -- ** ListPipelineExecutionSteps
    listPipelineExecutionSteps_maxResults,
    listPipelineExecutionSteps_nextToken,
    listPipelineExecutionSteps_pipelineExecutionArn,
    listPipelineExecutionSteps_sortOrder,
    listPipelineExecutionStepsResponse_nextToken,
    listPipelineExecutionStepsResponse_pipelineExecutionSteps,
    listPipelineExecutionStepsResponse_httpStatus,

    -- ** ListPipelineExecutions
    listPipelineExecutions_createdAfter,
    listPipelineExecutions_createdBefore,
    listPipelineExecutions_maxResults,
    listPipelineExecutions_nextToken,
    listPipelineExecutions_sortBy,
    listPipelineExecutions_sortOrder,
    listPipelineExecutions_pipelineName,
    listPipelineExecutionsResponse_nextToken,
    listPipelineExecutionsResponse_pipelineExecutionSummaries,
    listPipelineExecutionsResponse_httpStatus,

    -- ** ListPipelineParametersForExecution
    listPipelineParametersForExecution_maxResults,
    listPipelineParametersForExecution_nextToken,
    listPipelineParametersForExecution_pipelineExecutionArn,
    listPipelineParametersForExecutionResponse_nextToken,
    listPipelineParametersForExecutionResponse_pipelineParameters,
    listPipelineParametersForExecutionResponse_httpStatus,

    -- ** ListPipelines
    listPipelines_createdAfter,
    listPipelines_createdBefore,
    listPipelines_maxResults,
    listPipelines_nextToken,
    listPipelines_pipelineNamePrefix,
    listPipelines_sortBy,
    listPipelines_sortOrder,
    listPipelinesResponse_nextToken,
    listPipelinesResponse_pipelineSummaries,
    listPipelinesResponse_httpStatus,

    -- ** ListProcessingJobs
    listProcessingJobs_creationTimeAfter,
    listProcessingJobs_creationTimeBefore,
    listProcessingJobs_lastModifiedTimeAfter,
    listProcessingJobs_lastModifiedTimeBefore,
    listProcessingJobs_maxResults,
    listProcessingJobs_nameContains,
    listProcessingJobs_nextToken,
    listProcessingJobs_sortBy,
    listProcessingJobs_sortOrder,
    listProcessingJobs_statusEquals,
    listProcessingJobsResponse_nextToken,
    listProcessingJobsResponse_httpStatus,
    listProcessingJobsResponse_processingJobSummaries,

    -- ** ListProjects
    listProjects_creationTimeAfter,
    listProjects_creationTimeBefore,
    listProjects_maxResults,
    listProjects_nameContains,
    listProjects_nextToken,
    listProjects_sortBy,
    listProjects_sortOrder,
    listProjectsResponse_nextToken,
    listProjectsResponse_httpStatus,
    listProjectsResponse_projectSummaryList,

    -- ** ListSpaces
    listSpaces_domainIdEquals,
    listSpaces_maxResults,
    listSpaces_nextToken,
    listSpaces_sortBy,
    listSpaces_sortOrder,
    listSpaces_spaceNameContains,
    listSpacesResponse_nextToken,
    listSpacesResponse_spaces,
    listSpacesResponse_httpStatus,

    -- ** ListStageDevices
    listStageDevices_excludeDevicesDeployedInOtherStage,
    listStageDevices_maxResults,
    listStageDevices_nextToken,
    listStageDevices_edgeDeploymentPlanName,
    listStageDevices_stageName,
    listStageDevicesResponse_nextToken,
    listStageDevicesResponse_httpStatus,
    listStageDevicesResponse_deviceDeploymentSummaries,

    -- ** ListStudioLifecycleConfigs
    listStudioLifecycleConfigs_appTypeEquals,
    listStudioLifecycleConfigs_creationTimeAfter,
    listStudioLifecycleConfigs_creationTimeBefore,
    listStudioLifecycleConfigs_maxResults,
    listStudioLifecycleConfigs_modifiedTimeAfter,
    listStudioLifecycleConfigs_modifiedTimeBefore,
    listStudioLifecycleConfigs_nameContains,
    listStudioLifecycleConfigs_nextToken,
    listStudioLifecycleConfigs_sortBy,
    listStudioLifecycleConfigs_sortOrder,
    listStudioLifecycleConfigsResponse_nextToken,
    listStudioLifecycleConfigsResponse_studioLifecycleConfigs,
    listStudioLifecycleConfigsResponse_httpStatus,

    -- ** ListSubscribedWorkteams
    listSubscribedWorkteams_maxResults,
    listSubscribedWorkteams_nameContains,
    listSubscribedWorkteams_nextToken,
    listSubscribedWorkteamsResponse_nextToken,
    listSubscribedWorkteamsResponse_httpStatus,
    listSubscribedWorkteamsResponse_subscribedWorkteams,

    -- ** ListTags
    listTags_maxResults,
    listTags_nextToken,
    listTags_resourceArn,
    listTagsResponse_nextToken,
    listTagsResponse_tags,
    listTagsResponse_httpStatus,

    -- ** ListTrainingJobs
    listTrainingJobs_creationTimeAfter,
    listTrainingJobs_creationTimeBefore,
    listTrainingJobs_lastModifiedTimeAfter,
    listTrainingJobs_lastModifiedTimeBefore,
    listTrainingJobs_maxResults,
    listTrainingJobs_nameContains,
    listTrainingJobs_nextToken,
    listTrainingJobs_sortBy,
    listTrainingJobs_sortOrder,
    listTrainingJobs_statusEquals,
    listTrainingJobs_warmPoolStatusEquals,
    listTrainingJobsResponse_nextToken,
    listTrainingJobsResponse_httpStatus,
    listTrainingJobsResponse_trainingJobSummaries,

    -- ** ListTrainingJobsForHyperParameterTuningJob
    listTrainingJobsForHyperParameterTuningJob_maxResults,
    listTrainingJobsForHyperParameterTuningJob_nextToken,
    listTrainingJobsForHyperParameterTuningJob_sortBy,
    listTrainingJobsForHyperParameterTuningJob_sortOrder,
    listTrainingJobsForHyperParameterTuningJob_statusEquals,
    listTrainingJobsForHyperParameterTuningJob_hyperParameterTuningJobName,
    listTrainingJobsForHyperParameterTuningJobResponse_nextToken,
    listTrainingJobsForHyperParameterTuningJobResponse_httpStatus,
    listTrainingJobsForHyperParameterTuningJobResponse_trainingJobSummaries,

    -- ** ListTransformJobs
    listTransformJobs_creationTimeAfter,
    listTransformJobs_creationTimeBefore,
    listTransformJobs_lastModifiedTimeAfter,
    listTransformJobs_lastModifiedTimeBefore,
    listTransformJobs_maxResults,
    listTransformJobs_nameContains,
    listTransformJobs_nextToken,
    listTransformJobs_sortBy,
    listTransformJobs_sortOrder,
    listTransformJobs_statusEquals,
    listTransformJobsResponse_nextToken,
    listTransformJobsResponse_httpStatus,
    listTransformJobsResponse_transformJobSummaries,

    -- ** ListTrialComponents
    listTrialComponents_createdAfter,
    listTrialComponents_createdBefore,
    listTrialComponents_experimentName,
    listTrialComponents_maxResults,
    listTrialComponents_nextToken,
    listTrialComponents_sortBy,
    listTrialComponents_sortOrder,
    listTrialComponents_sourceArn,
    listTrialComponents_trialName,
    listTrialComponentsResponse_nextToken,
    listTrialComponentsResponse_trialComponentSummaries,
    listTrialComponentsResponse_httpStatus,

    -- ** ListTrials
    listTrials_createdAfter,
    listTrials_createdBefore,
    listTrials_experimentName,
    listTrials_maxResults,
    listTrials_nextToken,
    listTrials_sortBy,
    listTrials_sortOrder,
    listTrials_trialComponentName,
    listTrialsResponse_nextToken,
    listTrialsResponse_trialSummaries,
    listTrialsResponse_httpStatus,

    -- ** ListUserProfiles
    listUserProfiles_domainIdEquals,
    listUserProfiles_maxResults,
    listUserProfiles_nextToken,
    listUserProfiles_sortBy,
    listUserProfiles_sortOrder,
    listUserProfiles_userProfileNameContains,
    listUserProfilesResponse_nextToken,
    listUserProfilesResponse_userProfiles,
    listUserProfilesResponse_httpStatus,

    -- ** ListWorkforces
    listWorkforces_maxResults,
    listWorkforces_nameContains,
    listWorkforces_nextToken,
    listWorkforces_sortBy,
    listWorkforces_sortOrder,
    listWorkforcesResponse_nextToken,
    listWorkforcesResponse_httpStatus,
    listWorkforcesResponse_workforces,

    -- ** ListWorkteams
    listWorkteams_maxResults,
    listWorkteams_nameContains,
    listWorkteams_nextToken,
    listWorkteams_sortBy,
    listWorkteams_sortOrder,
    listWorkteamsResponse_nextToken,
    listWorkteamsResponse_httpStatus,
    listWorkteamsResponse_workteams,

    -- ** PutModelPackageGroupPolicy
    putModelPackageGroupPolicy_modelPackageGroupName,
    putModelPackageGroupPolicy_resourcePolicy,
    putModelPackageGroupPolicyResponse_httpStatus,
    putModelPackageGroupPolicyResponse_modelPackageGroupArn,

    -- ** QueryLineage
    queryLineage_direction,
    queryLineage_filters,
    queryLineage_includeEdges,
    queryLineage_maxDepth,
    queryLineage_maxResults,
    queryLineage_nextToken,
    queryLineage_startArns,
    queryLineageResponse_edges,
    queryLineageResponse_nextToken,
    queryLineageResponse_vertices,
    queryLineageResponse_httpStatus,

    -- ** RegisterDevices
    registerDevices_tags,
    registerDevices_deviceFleetName,
    registerDevices_devices,

    -- ** RenderUiTemplate
    renderUiTemplate_humanTaskUiArn,
    renderUiTemplate_uiTemplate,
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
    search_maxResults,
    search_nextToken,
    search_searchExpression,
    search_sortBy,
    search_sortOrder,
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

    -- ** StartInferenceExperiment
    startInferenceExperiment_name,
    startInferenceExperimentResponse_httpStatus,
    startInferenceExperimentResponse_inferenceExperimentArn,

    -- ** StartMonitoringSchedule
    startMonitoringSchedule_monitoringScheduleName,

    -- ** StartNotebookInstance
    startNotebookInstance_notebookInstanceName,

    -- ** StartPipelineExecution
    startPipelineExecution_parallelismConfiguration,
    startPipelineExecution_pipelineExecutionDescription,
    startPipelineExecution_pipelineExecutionDisplayName,
    startPipelineExecution_pipelineParameters,
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

    -- ** StopInferenceExperiment
    stopInferenceExperiment_desiredModelVariants,
    stopInferenceExperiment_desiredState,
    stopInferenceExperiment_reason,
    stopInferenceExperiment_name,
    stopInferenceExperiment_modelVariantActions,
    stopInferenceExperimentResponse_httpStatus,
    stopInferenceExperimentResponse_inferenceExperimentArn,

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
    updateAction_description,
    updateAction_properties,
    updateAction_propertiesToRemove,
    updateAction_status,
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
    updateArtifact_properties,
    updateArtifact_propertiesToRemove,
    updateArtifact_artifactArn,
    updateArtifactResponse_artifactArn,
    updateArtifactResponse_httpStatus,

    -- ** UpdateCodeRepository
    updateCodeRepository_gitConfig,
    updateCodeRepository_codeRepositoryName,
    updateCodeRepositoryResponse_httpStatus,
    updateCodeRepositoryResponse_codeRepositoryArn,

    -- ** UpdateContext
    updateContext_description,
    updateContext_properties,
    updateContext_propertiesToRemove,
    updateContext_contextName,
    updateContextResponse_contextArn,
    updateContextResponse_httpStatus,

    -- ** UpdateDeviceFleet
    updateDeviceFleet_description,
    updateDeviceFleet_enableIotRoleAlias,
    updateDeviceFleet_roleArn,
    updateDeviceFleet_deviceFleetName,
    updateDeviceFleet_outputConfig,

    -- ** UpdateDevices
    updateDevices_deviceFleetName,
    updateDevices_devices,

    -- ** UpdateDomain
    updateDomain_appSecurityGroupManagement,
    updateDomain_defaultSpaceSettings,
    updateDomain_defaultUserSettings,
    updateDomain_domainSettingsForUpdate,
    updateDomain_domainId,
    updateDomainResponse_domainArn,
    updateDomainResponse_httpStatus,

    -- ** UpdateEndpoint
    updateEndpoint_deploymentConfig,
    updateEndpoint_excludeRetainedVariantProperties,
    updateEndpoint_retainAllVariantProperties,
    updateEndpoint_retainDeploymentConfig,
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
    updateExperiment_description,
    updateExperiment_displayName,
    updateExperiment_experimentName,
    updateExperimentResponse_experimentArn,
    updateExperimentResponse_httpStatus,

    -- ** UpdateFeatureGroup
    updateFeatureGroup_featureAdditions,
    updateFeatureGroup_featureGroupName,
    updateFeatureGroupResponse_httpStatus,
    updateFeatureGroupResponse_featureGroupArn,

    -- ** UpdateFeatureMetadata
    updateFeatureMetadata_description,
    updateFeatureMetadata_parameterAdditions,
    updateFeatureMetadata_parameterRemovals,
    updateFeatureMetadata_featureGroupName,
    updateFeatureMetadata_featureName,

    -- ** UpdateHub
    updateHub_hubDescription,
    updateHub_hubDisplayName,
    updateHub_hubSearchKeywords,
    updateHub_hubName,
    updateHubResponse_httpStatus,
    updateHubResponse_hubArn,

    -- ** UpdateImage
    updateImage_deleteProperties,
    updateImage_description,
    updateImage_displayName,
    updateImage_roleArn,
    updateImage_imageName,
    updateImageResponse_imageArn,
    updateImageResponse_httpStatus,

    -- ** UpdateImageVersion
    updateImageVersion_alias,
    updateImageVersion_aliasesToAdd,
    updateImageVersion_aliasesToDelete,
    updateImageVersion_horovod,
    updateImageVersion_jobType,
    updateImageVersion_mLFramework,
    updateImageVersion_processor,
    updateImageVersion_programmingLang,
    updateImageVersion_releaseNotes,
    updateImageVersion_vendorGuidance,
    updateImageVersion_version,
    updateImageVersion_imageName,
    updateImageVersionResponse_imageVersionArn,
    updateImageVersionResponse_httpStatus,

    -- ** UpdateInferenceExperiment
    updateInferenceExperiment_dataStorageConfig,
    updateInferenceExperiment_description,
    updateInferenceExperiment_modelVariants,
    updateInferenceExperiment_schedule,
    updateInferenceExperiment_shadowModeConfig,
    updateInferenceExperiment_name,
    updateInferenceExperimentResponse_httpStatus,
    updateInferenceExperimentResponse_inferenceExperimentArn,

    -- ** UpdateModelCard
    updateModelCard_content,
    updateModelCard_modelCardStatus,
    updateModelCard_modelCardName,
    updateModelCardResponse_httpStatus,
    updateModelCardResponse_modelCardArn,

    -- ** UpdateModelPackage
    updateModelPackage_additionalInferenceSpecificationsToAdd,
    updateModelPackage_approvalDescription,
    updateModelPackage_customerMetadataProperties,
    updateModelPackage_customerMetadataPropertiesToRemove,
    updateModelPackage_modelApprovalStatus,
    updateModelPackage_modelPackageArn,
    updateModelPackageResponse_httpStatus,
    updateModelPackageResponse_modelPackageArn,

    -- ** UpdateMonitoringAlert
    updateMonitoringAlert_monitoringScheduleName,
    updateMonitoringAlert_monitoringAlertName,
    updateMonitoringAlert_datapointsToAlert,
    updateMonitoringAlert_evaluationPeriod,
    updateMonitoringAlertResponse_monitoringAlertName,
    updateMonitoringAlertResponse_httpStatus,
    updateMonitoringAlertResponse_monitoringScheduleArn,

    -- ** UpdateMonitoringSchedule
    updateMonitoringSchedule_monitoringScheduleName,
    updateMonitoringSchedule_monitoringScheduleConfig,
    updateMonitoringScheduleResponse_httpStatus,
    updateMonitoringScheduleResponse_monitoringScheduleArn,

    -- ** UpdateNotebookInstance
    updateNotebookInstance_acceleratorTypes,
    updateNotebookInstance_additionalCodeRepositories,
    updateNotebookInstance_defaultCodeRepository,
    updateNotebookInstance_disassociateAcceleratorTypes,
    updateNotebookInstance_disassociateAdditionalCodeRepositories,
    updateNotebookInstance_disassociateDefaultCodeRepository,
    updateNotebookInstance_disassociateLifecycleConfig,
    updateNotebookInstance_instanceMetadataServiceConfiguration,
    updateNotebookInstance_instanceType,
    updateNotebookInstance_lifecycleConfigName,
    updateNotebookInstance_roleArn,
    updateNotebookInstance_rootAccess,
    updateNotebookInstance_volumeSizeInGB,
    updateNotebookInstance_notebookInstanceName,
    updateNotebookInstanceResponse_httpStatus,

    -- ** UpdateNotebookInstanceLifecycleConfig
    updateNotebookInstanceLifecycleConfig_onCreate,
    updateNotebookInstanceLifecycleConfig_onStart,
    updateNotebookInstanceLifecycleConfig_notebookInstanceLifecycleConfigName,
    updateNotebookInstanceLifecycleConfigResponse_httpStatus,

    -- ** UpdatePipeline
    updatePipeline_parallelismConfiguration,
    updatePipeline_pipelineDefinition,
    updatePipeline_pipelineDefinitionS3Location,
    updatePipeline_pipelineDescription,
    updatePipeline_pipelineDisplayName,
    updatePipeline_roleArn,
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
    updateProject_projectDescription,
    updateProject_serviceCatalogProvisioningUpdateDetails,
    updateProject_tags,
    updateProject_projectName,
    updateProjectResponse_httpStatus,
    updateProjectResponse_projectArn,

    -- ** UpdateSpace
    updateSpace_spaceSettings,
    updateSpace_domainId,
    updateSpace_spaceName,
    updateSpaceResponse_spaceArn,
    updateSpaceResponse_httpStatus,

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
    updateTrialComponent_displayName,
    updateTrialComponent_endTime,
    updateTrialComponent_inputArtifacts,
    updateTrialComponent_inputArtifactsToRemove,
    updateTrialComponent_outputArtifacts,
    updateTrialComponent_outputArtifactsToRemove,
    updateTrialComponent_parameters,
    updateTrialComponent_parametersToRemove,
    updateTrialComponent_startTime,
    updateTrialComponent_status,
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
    updateWorkforce_oidcConfig,
    updateWorkforce_sourceIpConfig,
    updateWorkforce_workforceVpcConfig,
    updateWorkforce_workforceName,
    updateWorkforceResponse_httpStatus,
    updateWorkforceResponse_workforce,

    -- ** UpdateWorkteam
    updateWorkteam_description,
    updateWorkteam_memberDefinitions,
    updateWorkteam_notificationConfiguration,
    updateWorkteam_workteamName,
    updateWorkteamResponse_httpStatus,
    updateWorkteamResponse_workteam,

    -- * Types

    -- ** ActionSource
    actionSource_sourceId,
    actionSource_sourceType,
    actionSource_sourceUri,

    -- ** ActionSummary
    actionSummary_actionArn,
    actionSummary_actionName,
    actionSummary_actionType,
    actionSummary_creationTime,
    actionSummary_lastModifiedTime,
    actionSummary_source,
    actionSummary_status,

    -- ** AdditionalInferenceSpecificationDefinition
    additionalInferenceSpecificationDefinition_description,
    additionalInferenceSpecificationDefinition_supportedContentTypes,
    additionalInferenceSpecificationDefinition_supportedRealtimeInferenceInstanceTypes,
    additionalInferenceSpecificationDefinition_supportedResponseMIMETypes,
    additionalInferenceSpecificationDefinition_supportedTransformInstanceTypes,
    additionalInferenceSpecificationDefinition_name,
    additionalInferenceSpecificationDefinition_containers,

    -- ** AgentVersion
    agentVersion_version,
    agentVersion_agentCount,

    -- ** Alarm
    alarm_alarmName,

    -- ** AlgorithmSpecification
    algorithmSpecification_algorithmName,
    algorithmSpecification_containerArguments,
    algorithmSpecification_containerEntrypoint,
    algorithmSpecification_enableSageMakerMetricsTimeSeries,
    algorithmSpecification_metricDefinitions,
    algorithmSpecification_trainingImage,
    algorithmSpecification_trainingInputMode,

    -- ** AlgorithmStatusDetails
    algorithmStatusDetails_imageScanStatuses,
    algorithmStatusDetails_validationStatuses,

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
    appDetails_creationTime,
    appDetails_domainId,
    appDetails_spaceName,
    appDetails_status,
    appDetails_userProfileName,

    -- ** AppImageConfigDetails
    appImageConfigDetails_appImageConfigArn,
    appImageConfigDetails_appImageConfigName,
    appImageConfigDetails_creationTime,
    appImageConfigDetails_kernelGatewayImageConfig,
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
    artifactSummary_artifactArn,
    artifactSummary_artifactName,
    artifactSummary_artifactType,
    artifactSummary_creationTime,
    artifactSummary_lastModifiedTime,
    artifactSummary_source,

    -- ** AssociationSummary
    associationSummary_associationType,
    associationSummary_createdBy,
    associationSummary_creationTime,
    associationSummary_destinationArn,
    associationSummary_destinationName,
    associationSummary_destinationType,
    associationSummary_sourceArn,
    associationSummary_sourceName,
    associationSummary_sourceType,

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
    athenaDatasetDefinition_kmsKeyId,
    athenaDatasetDefinition_outputCompression,
    athenaDatasetDefinition_workGroup,
    athenaDatasetDefinition_catalog,
    athenaDatasetDefinition_database,
    athenaDatasetDefinition_queryString,
    athenaDatasetDefinition_outputS3Uri,
    athenaDatasetDefinition_outputFormat,

    -- ** AutoMLCandidate
    autoMLCandidate_candidateProperties,
    autoMLCandidate_endTime,
    autoMLCandidate_failureReason,
    autoMLCandidate_finalAutoMLJobObjectiveMetric,
    autoMLCandidate_inferenceContainers,
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
    autoMLChannel_channelType,
    autoMLChannel_compressionType,
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
    autoMLJobArtifacts_candidateDefinitionNotebookLocation,
    autoMLJobArtifacts_dataExplorationNotebookLocation,

    -- ** AutoMLJobCompletionCriteria
    autoMLJobCompletionCriteria_maxAutoMLJobRuntimeInSeconds,
    autoMLJobCompletionCriteria_maxCandidates,
    autoMLJobCompletionCriteria_maxRuntimePerTrainingJobInSeconds,

    -- ** AutoMLJobConfig
    autoMLJobConfig_candidateGenerationConfig,
    autoMLJobConfig_completionCriteria,
    autoMLJobConfig_dataSplitConfig,
    autoMLJobConfig_mode,
    autoMLJobConfig_securityConfig,

    -- ** AutoMLJobObjective
    autoMLJobObjective_metricName,

    -- ** AutoMLJobStepMetadata
    autoMLJobStepMetadata_arn,

    -- ** AutoMLJobSummary
    autoMLJobSummary_endTime,
    autoMLJobSummary_failureReason,
    autoMLJobSummary_partialFailureReasons,
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
    autoMLSecurityConfig_enableInterContainerTrafficEncryption,
    autoMLSecurityConfig_volumeKmsKeyId,
    autoMLSecurityConfig_vpcConfig,

    -- ** AutoRollbackConfig
    autoRollbackConfig_alarms,

    -- ** BatchDataCaptureConfig
    batchDataCaptureConfig_generateInferenceId,
    batchDataCaptureConfig_kmsKeyId,
    batchDataCaptureConfig_destinationS3Uri,

    -- ** BatchDescribeModelPackageError
    batchDescribeModelPackageError_errorCode,
    batchDescribeModelPackageError_errorResponse,

    -- ** BatchDescribeModelPackageSummary
    batchDescribeModelPackageSummary_modelApprovalStatus,
    batchDescribeModelPackageSummary_modelPackageDescription,
    batchDescribeModelPackageSummary_modelPackageVersion,
    batchDescribeModelPackageSummary_modelPackageGroupName,
    batchDescribeModelPackageSummary_modelPackageArn,
    batchDescribeModelPackageSummary_creationTime,
    batchDescribeModelPackageSummary_inferenceSpecification,
    batchDescribeModelPackageSummary_modelPackageStatus,

    -- ** BatchTransformInput
    batchTransformInput_endTimeOffset,
    batchTransformInput_featuresAttribute,
    batchTransformInput_inferenceAttribute,
    batchTransformInput_probabilityAttribute,
    batchTransformInput_probabilityThresholdAttribute,
    batchTransformInput_s3DataDistributionType,
    batchTransformInput_s3InputMode,
    batchTransformInput_startTimeOffset,
    batchTransformInput_dataCapturedDestinationS3Uri,
    batchTransformInput_datasetFormat,
    batchTransformInput_localPath,

    -- ** Bias
    bias_postTrainingReport,
    bias_preTrainingReport,
    bias_report,

    -- ** BlueGreenUpdatePolicy
    blueGreenUpdatePolicy_maximumExecutionTimeoutInSeconds,
    blueGreenUpdatePolicy_terminationWaitInSeconds,
    blueGreenUpdatePolicy_trafficRoutingConfiguration,

    -- ** CacheHitResult
    cacheHitResult_sourcePipelineExecutionArn,

    -- ** CallbackStepMetadata
    callbackStepMetadata_callbackToken,
    callbackStepMetadata_outputParameters,
    callbackStepMetadata_sqsQueueUrl,

    -- ** CandidateArtifactLocations
    candidateArtifactLocations_modelInsights,
    candidateArtifactLocations_explainability,

    -- ** CandidateProperties
    candidateProperties_candidateArtifactLocations,
    candidateProperties_candidateMetrics,

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
    channel_compressionType,
    channel_contentType,
    channel_inputMode,
    channel_recordWrapperType,
    channel_shuffleConfig,
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

    -- ** ClarifyCheckStepMetadata
    clarifyCheckStepMetadata_baselineUsedForDriftCheckConstraints,
    clarifyCheckStepMetadata_calculatedBaselineConstraints,
    clarifyCheckStepMetadata_checkJobArn,
    clarifyCheckStepMetadata_checkType,
    clarifyCheckStepMetadata_modelPackageGroupName,
    clarifyCheckStepMetadata_registerNewBaseline,
    clarifyCheckStepMetadata_skipCheck,
    clarifyCheckStepMetadata_violationReport,

    -- ** ClarifyExplainerConfig
    clarifyExplainerConfig_enableExplanations,
    clarifyExplainerConfig_inferenceConfig,
    clarifyExplainerConfig_shapConfig,

    -- ** ClarifyInferenceConfig
    clarifyInferenceConfig_contentTemplate,
    clarifyInferenceConfig_featureHeaders,
    clarifyInferenceConfig_featureTypes,
    clarifyInferenceConfig_featuresAttribute,
    clarifyInferenceConfig_labelAttribute,
    clarifyInferenceConfig_labelHeaders,
    clarifyInferenceConfig_labelIndex,
    clarifyInferenceConfig_maxPayloadInMB,
    clarifyInferenceConfig_maxRecordCount,
    clarifyInferenceConfig_probabilityAttribute,
    clarifyInferenceConfig_probabilityIndex,

    -- ** ClarifyShapBaselineConfig
    clarifyShapBaselineConfig_mimeType,
    clarifyShapBaselineConfig_shapBaseline,
    clarifyShapBaselineConfig_shapBaselineUri,

    -- ** ClarifyShapConfig
    clarifyShapConfig_numberOfSamples,
    clarifyShapConfig_seed,
    clarifyShapConfig_textConfig,
    clarifyShapConfig_useLogit,
    clarifyShapConfig_shapBaselineConfig,

    -- ** ClarifyTextConfig
    clarifyTextConfig_language,
    clarifyTextConfig_granularity,

    -- ** CodeRepository
    codeRepository_repositoryUrl,

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
    compilationJobSummary_compilationStartTime,
    compilationJobSummary_compilationTargetDevice,
    compilationJobSummary_compilationTargetPlatformAccelerator,
    compilationJobSummary_compilationTargetPlatformArch,
    compilationJobSummary_compilationTargetPlatformOs,
    compilationJobSummary_lastModifiedTime,
    compilationJobSummary_compilationJobName,
    compilationJobSummary_compilationJobArn,
    compilationJobSummary_creationTime,
    compilationJobSummary_compilationJobStatus,

    -- ** ConditionStepMetadata
    conditionStepMetadata_outcome,

    -- ** ContainerDefinition
    containerDefinition_containerHostname,
    containerDefinition_environment,
    containerDefinition_image,
    containerDefinition_imageConfig,
    containerDefinition_inferenceSpecificationName,
    containerDefinition_mode,
    containerDefinition_modelDataUrl,
    containerDefinition_modelPackageName,
    containerDefinition_multiModelConfig,

    -- ** ContextSource
    contextSource_sourceId,
    contextSource_sourceType,
    contextSource_sourceUri,

    -- ** ContextSummary
    contextSummary_contextArn,
    contextSummary_contextName,
    contextSummary_contextType,
    contextSummary_creationTime,
    contextSummary_lastModifiedTime,
    contextSummary_source,

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
    dataCaptureConfig_enableCapture,
    dataCaptureConfig_kmsKeyId,
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
    dataProcessing_inputFilter,
    dataProcessing_joinSource,
    dataProcessing_outputFilter,

    -- ** DataQualityAppSpecification
    dataQualityAppSpecification_containerArguments,
    dataQualityAppSpecification_containerEntrypoint,
    dataQualityAppSpecification_environment,
    dataQualityAppSpecification_postAnalyticsProcessorSourceUri,
    dataQualityAppSpecification_recordPreprocessorSourceUri,
    dataQualityAppSpecification_imageUri,

    -- ** DataQualityBaselineConfig
    dataQualityBaselineConfig_baseliningJobName,
    dataQualityBaselineConfig_constraintsResource,
    dataQualityBaselineConfig_statisticsResource,

    -- ** DataQualityJobInput
    dataQualityJobInput_batchTransformInput,
    dataQualityJobInput_endpointInput,

    -- ** DataSource
    dataSource_fileSystemDataSource,
    dataSource_s3DataSource,

    -- ** DatasetDefinition
    datasetDefinition_athenaDatasetDefinition,
    datasetDefinition_dataDistributionType,
    datasetDefinition_inputMode,
    datasetDefinition_localPath,
    datasetDefinition_redshiftDatasetDefinition,

    -- ** DebugHookConfig
    debugHookConfig_collectionConfigurations,
    debugHookConfig_hookParameters,
    debugHookConfig_localPath,
    debugHookConfig_s3OutputPath,

    -- ** DebugRuleConfiguration
    debugRuleConfiguration_instanceType,
    debugRuleConfiguration_localPath,
    debugRuleConfiguration_ruleParameters,
    debugRuleConfiguration_s3OutputPath,
    debugRuleConfiguration_volumeSizeInGB,
    debugRuleConfiguration_ruleConfigurationName,
    debugRuleConfiguration_ruleEvaluatorImage,

    -- ** DebugRuleEvaluationStatus
    debugRuleEvaluationStatus_lastModifiedTime,
    debugRuleEvaluationStatus_ruleConfigurationName,
    debugRuleEvaluationStatus_ruleEvaluationJobArn,
    debugRuleEvaluationStatus_ruleEvaluationStatus,
    debugRuleEvaluationStatus_statusDetails,

    -- ** DefaultSpaceSettings
    defaultSpaceSettings_executionRole,
    defaultSpaceSettings_jupyterServerAppSettings,
    defaultSpaceSettings_kernelGatewayAppSettings,
    defaultSpaceSettings_securityGroups,

    -- ** DeployedImage
    deployedImage_resolutionTime,
    deployedImage_resolvedImage,
    deployedImage_specifiedImage,

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
    desiredWeightAndCapacity_desiredInstanceCount,
    desiredWeightAndCapacity_desiredWeight,
    desiredWeightAndCapacity_variantName,

    -- ** Device
    device_description,
    device_iotThingName,
    device_deviceName,

    -- ** DeviceDeploymentSummary
    deviceDeploymentSummary_deployedStageName,
    deviceDeploymentSummary_deploymentStartTime,
    deviceDeploymentSummary_description,
    deviceDeploymentSummary_deviceDeploymentStatus,
    deviceDeploymentSummary_deviceDeploymentStatusMessage,
    deviceDeploymentSummary_deviceFleetName,
    deviceDeploymentSummary_edgeDeploymentPlanArn,
    deviceDeploymentSummary_edgeDeploymentPlanName,
    deviceDeploymentSummary_stageName,
    deviceDeploymentSummary_deviceName,
    deviceDeploymentSummary_deviceArn,

    -- ** DeviceFleetSummary
    deviceFleetSummary_creationTime,
    deviceFleetSummary_lastModifiedTime,
    deviceFleetSummary_deviceFleetArn,
    deviceFleetSummary_deviceFleetName,

    -- ** DeviceSelectionConfig
    deviceSelectionConfig_deviceNameContains,
    deviceSelectionConfig_deviceNames,
    deviceSelectionConfig_percentage,
    deviceSelectionConfig_deviceSubsetType,

    -- ** DeviceStats
    deviceStats_connectedDeviceCount,
    deviceStats_registeredDeviceCount,

    -- ** DeviceSummary
    deviceSummary_agentVersion,
    deviceSummary_description,
    deviceSummary_deviceFleetName,
    deviceSummary_iotThingName,
    deviceSummary_latestHeartbeat,
    deviceSummary_models,
    deviceSummary_registrationTime,
    deviceSummary_deviceName,
    deviceSummary_deviceArn,

    -- ** DomainDetails
    domainDetails_creationTime,
    domainDetails_domainArn,
    domainDetails_domainId,
    domainDetails_domainName,
    domainDetails_lastModifiedTime,
    domainDetails_status,
    domainDetails_url,

    -- ** DomainSettings
    domainSettings_executionRoleIdentityConfig,
    domainSettings_rStudioServerProDomainSettings,
    domainSettings_securityGroupIds,

    -- ** DomainSettingsForUpdate
    domainSettingsForUpdate_executionRoleIdentityConfig,
    domainSettingsForUpdate_rStudioServerProDomainSettingsForUpdate,
    domainSettingsForUpdate_securityGroupIds,

    -- ** DriftCheckBaselines
    driftCheckBaselines_bias,
    driftCheckBaselines_explainability,
    driftCheckBaselines_modelDataQuality,
    driftCheckBaselines_modelQuality,

    -- ** DriftCheckBias
    driftCheckBias_configFile,
    driftCheckBias_postTrainingConstraints,
    driftCheckBias_preTrainingConstraints,

    -- ** DriftCheckExplainability
    driftCheckExplainability_configFile,
    driftCheckExplainability_constraints,

    -- ** DriftCheckModelDataQuality
    driftCheckModelDataQuality_constraints,
    driftCheckModelDataQuality_statistics,

    -- ** DriftCheckModelQuality
    driftCheckModelQuality_constraints,
    driftCheckModelQuality_statistics,

    -- ** EMRStepMetadata
    eMRStepMetadata_clusterId,
    eMRStepMetadata_logFilePath,
    eMRStepMetadata_stepId,
    eMRStepMetadata_stepName,

    -- ** Edge
    edge_associationType,
    edge_destinationArn,
    edge_sourceArn,

    -- ** EdgeDeploymentConfig
    edgeDeploymentConfig_failureHandlingPolicy,

    -- ** EdgeDeploymentModelConfig
    edgeDeploymentModelConfig_modelHandle,
    edgeDeploymentModelConfig_edgePackagingJobName,

    -- ** EdgeDeploymentPlanSummary
    edgeDeploymentPlanSummary_creationTime,
    edgeDeploymentPlanSummary_lastModifiedTime,
    edgeDeploymentPlanSummary_edgeDeploymentPlanArn,
    edgeDeploymentPlanSummary_edgeDeploymentPlanName,
    edgeDeploymentPlanSummary_deviceFleetName,
    edgeDeploymentPlanSummary_edgeDeploymentSuccess,
    edgeDeploymentPlanSummary_edgeDeploymentPending,
    edgeDeploymentPlanSummary_edgeDeploymentFailed,

    -- ** EdgeDeploymentStatus
    edgeDeploymentStatus_edgeDeploymentStageStartTime,
    edgeDeploymentStatus_edgeDeploymentStatusMessage,
    edgeDeploymentStatus_stageStatus,
    edgeDeploymentStatus_edgeDeploymentSuccessInStage,
    edgeDeploymentStatus_edgeDeploymentPendingInStage,
    edgeDeploymentStatus_edgeDeploymentFailedInStage,

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
    edgeOutputConfig_presetDeploymentConfig,
    edgeOutputConfig_presetDeploymentType,
    edgeOutputConfig_s3OutputLocation,

    -- ** EdgePackagingJobSummary
    edgePackagingJobSummary_compilationJobName,
    edgePackagingJobSummary_creationTime,
    edgePackagingJobSummary_lastModifiedTime,
    edgePackagingJobSummary_modelName,
    edgePackagingJobSummary_modelVersion,
    edgePackagingJobSummary_edgePackagingJobArn,
    edgePackagingJobSummary_edgePackagingJobName,
    edgePackagingJobSummary_edgePackagingJobStatus,

    -- ** EdgePresetDeploymentOutput
    edgePresetDeploymentOutput_artifact,
    edgePresetDeploymentOutput_status,
    edgePresetDeploymentOutput_statusMessage,
    edgePresetDeploymentOutput_type,

    -- ** Endpoint
    endpoint_dataCaptureConfig,
    endpoint_failureReason,
    endpoint_monitoringSchedules,
    endpoint_productionVariants,
    endpoint_shadowProductionVariants,
    endpoint_tags,
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
    endpointInput_endTimeOffset,
    endpointInput_featuresAttribute,
    endpointInput_inferenceAttribute,
    endpointInput_probabilityAttribute,
    endpointInput_probabilityThresholdAttribute,
    endpointInput_s3DataDistributionType,
    endpointInput_s3InputMode,
    endpointInput_startTimeOffset,
    endpointInput_endpointName,
    endpointInput_localPath,

    -- ** EndpointInputConfiguration
    endpointInputConfiguration_environmentParameterRanges,
    endpointInputConfiguration_inferenceSpecificationName,
    endpointInputConfiguration_instanceType,

    -- ** EndpointMetadata
    endpointMetadata_endpointConfigName,
    endpointMetadata_endpointStatus,
    endpointMetadata_failureReason,
    endpointMetadata_endpointName,

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
    experiment_createdBy,
    experiment_creationTime,
    experiment_description,
    experiment_displayName,
    experiment_experimentArn,
    experiment_experimentName,
    experiment_lastModifiedBy,
    experiment_lastModifiedTime,
    experiment_source,
    experiment_tags,

    -- ** ExperimentConfig
    experimentConfig_experimentName,
    experimentConfig_runName,
    experimentConfig_trialComponentDisplayName,
    experimentConfig_trialName,

    -- ** ExperimentSource
    experimentSource_sourceType,
    experimentSource_sourceArn,

    -- ** ExperimentSummary
    experimentSummary_creationTime,
    experimentSummary_displayName,
    experimentSummary_experimentArn,
    experimentSummary_experimentName,
    experimentSummary_experimentSource,
    experimentSummary_lastModifiedTime,

    -- ** Explainability
    explainability_report,

    -- ** ExplainerConfig
    explainerConfig_clarifyExplainerConfig,

    -- ** FailStepMetadata
    failStepMetadata_errorMessage,

    -- ** FeatureDefinition
    featureDefinition_featureName,
    featureDefinition_featureType,

    -- ** FeatureGroup
    featureGroup_creationTime,
    featureGroup_description,
    featureGroup_eventTimeFeatureName,
    featureGroup_failureReason,
    featureGroup_featureDefinitions,
    featureGroup_featureGroupArn,
    featureGroup_featureGroupName,
    featureGroup_featureGroupStatus,
    featureGroup_lastModifiedTime,
    featureGroup_lastUpdateStatus,
    featureGroup_offlineStoreConfig,
    featureGroup_offlineStoreStatus,
    featureGroup_onlineStoreConfig,
    featureGroup_recordIdentifierFeatureName,
    featureGroup_roleArn,
    featureGroup_tags,

    -- ** FeatureGroupSummary
    featureGroupSummary_featureGroupStatus,
    featureGroupSummary_offlineStoreStatus,
    featureGroupSummary_featureGroupName,
    featureGroupSummary_featureGroupArn,
    featureGroupSummary_creationTime,

    -- ** FeatureMetadata
    featureMetadata_creationTime,
    featureMetadata_description,
    featureMetadata_featureGroupArn,
    featureMetadata_featureGroupName,
    featureMetadata_featureName,
    featureMetadata_featureType,
    featureMetadata_lastModifiedTime,
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

    -- ** HubContentDependency
    hubContentDependency_dependencyCopyPath,
    hubContentDependency_dependencyOriginPath,

    -- ** HubContentInfo
    hubContentInfo_hubContentDescription,
    hubContentInfo_hubContentDisplayName,
    hubContentInfo_hubContentSearchKeywords,
    hubContentInfo_hubContentName,
    hubContentInfo_hubContentArn,
    hubContentInfo_hubContentVersion,
    hubContentInfo_hubContentType,
    hubContentInfo_documentSchemaVersion,
    hubContentInfo_hubContentStatus,
    hubContentInfo_creationTime,

    -- ** HubInfo
    hubInfo_hubDescription,
    hubInfo_hubDisplayName,
    hubInfo_hubSearchKeywords,
    hubInfo_hubName,
    hubInfo_hubArn,
    hubInfo_hubStatus,
    hubInfo_creationTime,
    hubInfo_lastModifiedTime,

    -- ** HubS3StorageConfig
    hubS3StorageConfig_s3OutputPath,

    -- ** HumanLoopActivationConditionsConfig
    humanLoopActivationConditionsConfig_humanLoopActivationConditions,

    -- ** HumanLoopActivationConfig
    humanLoopActivationConfig_humanLoopActivationConditionsConfig,

    -- ** HumanLoopConfig
    humanLoopConfig_publicWorkforceTaskPrice,
    humanLoopConfig_taskAvailabilityLifetimeInSeconds,
    humanLoopConfig_taskKeywords,
    humanLoopConfig_taskTimeLimitInSeconds,
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
    humanTaskConfig_taskAvailabilityLifetimeInSeconds,
    humanTaskConfig_taskKeywords,
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
    hyperParameterSpecification_isRequired,
    hyperParameterSpecification_isTunable,
    hyperParameterSpecification_range,
    hyperParameterSpecification_name,
    hyperParameterSpecification_type,

    -- ** HyperParameterTrainingJobDefinition
    hyperParameterTrainingJobDefinition_checkpointConfig,
    hyperParameterTrainingJobDefinition_definitionName,
    hyperParameterTrainingJobDefinition_enableInterContainerTrafficEncryption,
    hyperParameterTrainingJobDefinition_enableManagedSpotTraining,
    hyperParameterTrainingJobDefinition_enableNetworkIsolation,
    hyperParameterTrainingJobDefinition_hyperParameterRanges,
    hyperParameterTrainingJobDefinition_hyperParameterTuningResourceConfig,
    hyperParameterTrainingJobDefinition_inputDataConfig,
    hyperParameterTrainingJobDefinition_resourceConfig,
    hyperParameterTrainingJobDefinition_retryStrategy,
    hyperParameterTrainingJobDefinition_staticHyperParameters,
    hyperParameterTrainingJobDefinition_tuningObjective,
    hyperParameterTrainingJobDefinition_vpcConfig,
    hyperParameterTrainingJobDefinition_algorithmSpecification,
    hyperParameterTrainingJobDefinition_roleArn,
    hyperParameterTrainingJobDefinition_outputDataConfig,
    hyperParameterTrainingJobDefinition_stoppingCondition,

    -- ** HyperParameterTrainingJobSummary
    hyperParameterTrainingJobSummary_failureReason,
    hyperParameterTrainingJobSummary_finalHyperParameterTuningJobObjectiveMetric,
    hyperParameterTrainingJobSummary_objectiveStatus,
    hyperParameterTrainingJobSummary_trainingEndTime,
    hyperParameterTrainingJobSummary_trainingJobDefinitionName,
    hyperParameterTrainingJobSummary_trainingStartTime,
    hyperParameterTrainingJobSummary_tuningJobName,
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
    hyperParameterTuningJobConfig_hyperParameterTuningJobObjective,
    hyperParameterTuningJobConfig_parameterRanges,
    hyperParameterTuningJobConfig_randomSeed,
    hyperParameterTuningJobConfig_strategyConfig,
    hyperParameterTuningJobConfig_trainingJobEarlyStoppingType,
    hyperParameterTuningJobConfig_tuningJobCompletionCriteria,
    hyperParameterTuningJobConfig_strategy,
    hyperParameterTuningJobConfig_resourceLimits,

    -- ** HyperParameterTuningJobObjective
    hyperParameterTuningJobObjective_type,
    hyperParameterTuningJobObjective_metricName,

    -- ** HyperParameterTuningJobSearchEntity
    hyperParameterTuningJobSearchEntity_bestTrainingJob,
    hyperParameterTuningJobSearchEntity_creationTime,
    hyperParameterTuningJobSearchEntity_failureReason,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningEndTime,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningJobArn,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningJobConfig,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningJobName,
    hyperParameterTuningJobSearchEntity_hyperParameterTuningJobStatus,
    hyperParameterTuningJobSearchEntity_lastModifiedTime,
    hyperParameterTuningJobSearchEntity_objectiveStatusCounters,
    hyperParameterTuningJobSearchEntity_overallBestTrainingJob,
    hyperParameterTuningJobSearchEntity_tags,
    hyperParameterTuningJobSearchEntity_trainingJobDefinition,
    hyperParameterTuningJobSearchEntity_trainingJobDefinitions,
    hyperParameterTuningJobSearchEntity_trainingJobStatusCounters,
    hyperParameterTuningJobSearchEntity_warmStartConfig,

    -- ** HyperParameterTuningJobStrategyConfig
    hyperParameterTuningJobStrategyConfig_hyperbandStrategyConfig,

    -- ** HyperParameterTuningJobSummary
    hyperParameterTuningJobSummary_hyperParameterTuningEndTime,
    hyperParameterTuningJobSummary_lastModifiedTime,
    hyperParameterTuningJobSummary_resourceLimits,
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
    hyperParameterTuningResourceConfig_allocationStrategy,
    hyperParameterTuningResourceConfig_instanceConfigs,
    hyperParameterTuningResourceConfig_instanceCount,
    hyperParameterTuningResourceConfig_instanceType,
    hyperParameterTuningResourceConfig_volumeKmsKeyId,
    hyperParameterTuningResourceConfig_volumeSizeInGB,

    -- ** HyperbandStrategyConfig
    hyperbandStrategyConfig_maxResource,
    hyperbandStrategyConfig_minResource,

    -- ** Image
    image_description,
    image_displayName,
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

    -- ** InferenceExperimentDataStorageConfig
    inferenceExperimentDataStorageConfig_contentType,
    inferenceExperimentDataStorageConfig_kmsKey,
    inferenceExperimentDataStorageConfig_destination,

    -- ** InferenceExperimentSchedule
    inferenceExperimentSchedule_endTime,
    inferenceExperimentSchedule_startTime,

    -- ** InferenceExperimentSummary
    inferenceExperimentSummary_completionTime,
    inferenceExperimentSummary_description,
    inferenceExperimentSummary_roleArn,
    inferenceExperimentSummary_schedule,
    inferenceExperimentSummary_statusReason,
    inferenceExperimentSummary_name,
    inferenceExperimentSummary_type,
    inferenceExperimentSummary_status,
    inferenceExperimentSummary_creationTime,
    inferenceExperimentSummary_lastModifiedTime,

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
    jupyterServerAppSettings_codeRepositories,
    jupyterServerAppSettings_defaultResourceSpec,
    jupyterServerAppSettings_lifecycleConfigArns,

    -- ** KernelGatewayAppSettings
    kernelGatewayAppSettings_customImages,
    kernelGatewayAppSettings_defaultResourceSpec,
    kernelGatewayAppSettings_lifecycleConfigArns,

    -- ** KernelGatewayImageConfig
    kernelGatewayImageConfig_fileSystemConfig,
    kernelGatewayImageConfig_kernelSpecs,

    -- ** KernelSpec
    kernelSpec_displayName,
    kernelSpec_name,

    -- ** LabelCounters
    labelCounters_failedNonRetryableError,
    labelCounters_humanLabeled,
    labelCounters_machineLabeled,
    labelCounters_totalLabeled,
    labelCounters_unlabeled,

    -- ** LabelCountersForWorkteam
    labelCountersForWorkteam_humanLabeled,
    labelCountersForWorkteam_pendingHuman,
    labelCountersForWorkteam_total,

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
    labelingJobResourceConfig_vpcConfig,

    -- ** LabelingJobS3DataSource
    labelingJobS3DataSource_manifestS3Uri,

    -- ** LabelingJobSnsDataSource
    labelingJobSnsDataSource_snsTopicArn,

    -- ** LabelingJobStoppingConditions
    labelingJobStoppingConditions_maxHumanLabeledObjectCount,
    labelingJobStoppingConditions_maxPercentageOfInputDatasetLabeled,

    -- ** LabelingJobSummary
    labelingJobSummary_annotationConsolidationLambdaArn,
    labelingJobSummary_failureReason,
    labelingJobSummary_inputConfig,
    labelingJobSummary_labelingJobOutput,
    labelingJobSummary_labelingJobName,
    labelingJobSummary_labelingJobArn,
    labelingJobSummary_creationTime,
    labelingJobSummary_lastModifiedTime,
    labelingJobSummary_labelingJobStatus,
    labelingJobSummary_labelCounters,
    labelingJobSummary_workteamArn,
    labelingJobSummary_preHumanTaskLambdaArn,

    -- ** LambdaStepMetadata
    lambdaStepMetadata_arn,
    lambdaStepMetadata_outputParameters,

    -- ** LastUpdateStatus
    lastUpdateStatus_failureReason,
    lastUpdateStatus_status,

    -- ** LineageGroupSummary
    lineageGroupSummary_creationTime,
    lineageGroupSummary_displayName,
    lineageGroupSummary_lastModifiedTime,
    lineageGroupSummary_lineageGroupArn,
    lineageGroupSummary_lineageGroupName,

    -- ** MemberDefinition
    memberDefinition_cognitoMemberDefinition,
    memberDefinition_oidcMemberDefinition,

    -- ** MetadataProperties
    metadataProperties_commitId,
    metadataProperties_generatedBy,
    metadataProperties_projectId,
    metadataProperties_repository,

    -- ** MetricData
    metricData_metricName,
    metricData_timestamp,
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

    -- ** Model
    model_containers,
    model_creationTime,
    model_enableNetworkIsolation,
    model_executionRoleArn,
    model_inferenceExecutionConfig,
    model_modelArn,
    model_modelName,
    model_primaryContainer,
    model_tags,
    model_vpcConfig,

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
    modelBiasJobInput_batchTransformInput,
    modelBiasJobInput_endpointInput,
    modelBiasJobInput_groundTruthS3Input,

    -- ** ModelCard
    modelCard_content,
    modelCard_createdBy,
    modelCard_creationTime,
    modelCard_lastModifiedBy,
    modelCard_lastModifiedTime,
    modelCard_modelCardArn,
    modelCard_modelCardName,
    modelCard_modelCardStatus,
    modelCard_modelCardVersion,
    modelCard_modelId,
    modelCard_riskRating,
    modelCard_securityConfig,
    modelCard_tags,

    -- ** ModelCardExportArtifacts
    modelCardExportArtifacts_s3ExportArtifacts,

    -- ** ModelCardExportJobSummary
    modelCardExportJobSummary_modelCardExportJobName,
    modelCardExportJobSummary_modelCardExportJobArn,
    modelCardExportJobSummary_status,
    modelCardExportJobSummary_modelCardName,
    modelCardExportJobSummary_modelCardVersion,
    modelCardExportJobSummary_createdAt,
    modelCardExportJobSummary_lastModifiedAt,

    -- ** ModelCardExportOutputConfig
    modelCardExportOutputConfig_s3OutputPath,

    -- ** ModelCardSecurityConfig
    modelCardSecurityConfig_kmsKeyId,

    -- ** ModelCardSummary
    modelCardSummary_lastModifiedTime,
    modelCardSummary_modelCardName,
    modelCardSummary_modelCardArn,
    modelCardSummary_modelCardStatus,
    modelCardSummary_creationTime,

    -- ** ModelCardVersionSummary
    modelCardVersionSummary_lastModifiedTime,
    modelCardVersionSummary_modelCardName,
    modelCardVersionSummary_modelCardArn,
    modelCardVersionSummary_modelCardStatus,
    modelCardVersionSummary_modelCardVersion,
    modelCardVersionSummary_creationTime,

    -- ** ModelClientConfig
    modelClientConfig_invocationsMaxRetries,
    modelClientConfig_invocationsTimeoutInSeconds,

    -- ** ModelConfiguration
    modelConfiguration_environmentParameters,
    modelConfiguration_inferenceSpecificationName,

    -- ** ModelDashboardEndpoint
    modelDashboardEndpoint_endpointName,
    modelDashboardEndpoint_endpointArn,
    modelDashboardEndpoint_creationTime,
    modelDashboardEndpoint_lastModifiedTime,
    modelDashboardEndpoint_endpointStatus,

    -- ** ModelDashboardIndicatorAction
    modelDashboardIndicatorAction_enabled,

    -- ** ModelDashboardModel
    modelDashboardModel_endpoints,
    modelDashboardModel_lastBatchTransformJob,
    modelDashboardModel_model,
    modelDashboardModel_modelCard,
    modelDashboardModel_monitoringSchedules,

    -- ** ModelDashboardModelCard
    modelDashboardModelCard_createdBy,
    modelDashboardModelCard_creationTime,
    modelDashboardModelCard_lastModifiedBy,
    modelDashboardModelCard_lastModifiedTime,
    modelDashboardModelCard_modelCardArn,
    modelDashboardModelCard_modelCardName,
    modelDashboardModelCard_modelCardStatus,
    modelDashboardModelCard_modelCardVersion,
    modelDashboardModelCard_modelId,
    modelDashboardModelCard_riskRating,
    modelDashboardModelCard_securityConfig,
    modelDashboardModelCard_tags,

    -- ** ModelDashboardMonitoringSchedule
    modelDashboardMonitoringSchedule_creationTime,
    modelDashboardMonitoringSchedule_endpointName,
    modelDashboardMonitoringSchedule_failureReason,
    modelDashboardMonitoringSchedule_lastModifiedTime,
    modelDashboardMonitoringSchedule_lastMonitoringExecutionSummary,
    modelDashboardMonitoringSchedule_monitoringAlertSummaries,
    modelDashboardMonitoringSchedule_monitoringScheduleArn,
    modelDashboardMonitoringSchedule_monitoringScheduleConfig,
    modelDashboardMonitoringSchedule_monitoringScheduleName,
    modelDashboardMonitoringSchedule_monitoringScheduleStatus,
    modelDashboardMonitoringSchedule_monitoringType,

    -- ** ModelDataQuality
    modelDataQuality_constraints,
    modelDataQuality_statistics,

    -- ** ModelDeployConfig
    modelDeployConfig_autoGenerateEndpointName,
    modelDeployConfig_endpointName,

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
    modelExplainabilityJobInput_batchTransformInput,
    modelExplainabilityJobInput_endpointInput,

    -- ** ModelInfrastructureConfig
    modelInfrastructureConfig_infrastructureType,
    modelInfrastructureConfig_realTimeInferenceConfig,

    -- ** ModelInput
    modelInput_dataInputConfig,

    -- ** ModelLatencyThreshold
    modelLatencyThreshold_percentile,
    modelLatencyThreshold_valueInMilliseconds,

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
    modelMetrics_bias,
    modelMetrics_explainability,
    modelMetrics_modelDataQuality,
    modelMetrics_modelQuality,

    -- ** ModelPackage
    modelPackage_additionalInferenceSpecifications,
    modelPackage_approvalDescription,
    modelPackage_certifyForMarketplace,
    modelPackage_createdBy,
    modelPackage_creationTime,
    modelPackage_customerMetadataProperties,
    modelPackage_domain,
    modelPackage_driftCheckBaselines,
    modelPackage_inferenceSpecification,
    modelPackage_lastModifiedBy,
    modelPackage_lastModifiedTime,
    modelPackage_metadataProperties,
    modelPackage_modelApprovalStatus,
    modelPackage_modelMetrics,
    modelPackage_modelPackageArn,
    modelPackage_modelPackageDescription,
    modelPackage_modelPackageGroupName,
    modelPackage_modelPackageName,
    modelPackage_modelPackageStatus,
    modelPackage_modelPackageStatusDetails,
    modelPackage_modelPackageVersion,
    modelPackage_samplePayloadUrl,
    modelPackage_sourceAlgorithmSpecification,
    modelPackage_tags,
    modelPackage_task,
    modelPackage_validationSpecification,

    -- ** ModelPackageContainerDefinition
    modelPackageContainerDefinition_containerHostname,
    modelPackageContainerDefinition_environment,
    modelPackageContainerDefinition_framework,
    modelPackageContainerDefinition_frameworkVersion,
    modelPackageContainerDefinition_imageDigest,
    modelPackageContainerDefinition_modelDataUrl,
    modelPackageContainerDefinition_modelInput,
    modelPackageContainerDefinition_nearestModelName,
    modelPackageContainerDefinition_productId,
    modelPackageContainerDefinition_image,

    -- ** ModelPackageGroup
    modelPackageGroup_createdBy,
    modelPackageGroup_creationTime,
    modelPackageGroup_modelPackageGroupArn,
    modelPackageGroup_modelPackageGroupDescription,
    modelPackageGroup_modelPackageGroupName,
    modelPackageGroup_modelPackageGroupStatus,
    modelPackageGroup_tags,

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
    modelPackageSummary_modelApprovalStatus,
    modelPackageSummary_modelPackageDescription,
    modelPackageSummary_modelPackageGroupName,
    modelPackageSummary_modelPackageVersion,
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
    modelQualityAppSpecification_environment,
    modelQualityAppSpecification_postAnalyticsProcessorSourceUri,
    modelQualityAppSpecification_problemType,
    modelQualityAppSpecification_recordPreprocessorSourceUri,
    modelQualityAppSpecification_imageUri,

    -- ** ModelQualityBaselineConfig
    modelQualityBaselineConfig_baseliningJobName,
    modelQualityBaselineConfig_constraintsResource,

    -- ** ModelQualityJobInput
    modelQualityJobInput_batchTransformInput,
    modelQualityJobInput_endpointInput,
    modelQualityJobInput_groundTruthS3Input,

    -- ** ModelStepMetadata
    modelStepMetadata_arn,

    -- ** ModelSummary
    modelSummary_modelName,
    modelSummary_modelArn,
    modelSummary_creationTime,

    -- ** ModelVariantConfig
    modelVariantConfig_modelName,
    modelVariantConfig_variantName,
    modelVariantConfig_infrastructureConfig,

    -- ** ModelVariantConfigSummary
    modelVariantConfigSummary_modelName,
    modelVariantConfigSummary_variantName,
    modelVariantConfigSummary_infrastructureConfig,
    modelVariantConfigSummary_status,

    -- ** MonitoringAlertActions
    monitoringAlertActions_modelDashboardIndicator,

    -- ** MonitoringAlertHistorySummary
    monitoringAlertHistorySummary_monitoringScheduleName,
    monitoringAlertHistorySummary_monitoringAlertName,
    monitoringAlertHistorySummary_creationTime,
    monitoringAlertHistorySummary_alertStatus,

    -- ** MonitoringAlertSummary
    monitoringAlertSummary_monitoringAlertName,
    monitoringAlertSummary_creationTime,
    monitoringAlertSummary_lastModifiedTime,
    monitoringAlertSummary_alertStatus,
    monitoringAlertSummary_datapointsToAlert,
    monitoringAlertSummary_evaluationPeriod,
    monitoringAlertSummary_actions,

    -- ** MonitoringAppSpecification
    monitoringAppSpecification_containerArguments,
    monitoringAppSpecification_containerEntrypoint,
    monitoringAppSpecification_postAnalyticsProcessorSourceUri,
    monitoringAppSpecification_recordPreprocessorSourceUri,
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
    monitoringDatasetFormat_csv,
    monitoringDatasetFormat_json,
    monitoringDatasetFormat_parquet,

    -- ** MonitoringExecutionSummary
    monitoringExecutionSummary_endpointName,
    monitoringExecutionSummary_failureReason,
    monitoringExecutionSummary_monitoringJobDefinitionName,
    monitoringExecutionSummary_monitoringType,
    monitoringExecutionSummary_processingJobArn,
    monitoringExecutionSummary_monitoringScheduleName,
    monitoringExecutionSummary_scheduledTime,
    monitoringExecutionSummary_creationTime,
    monitoringExecutionSummary_lastModifiedTime,
    monitoringExecutionSummary_monitoringExecutionStatus,

    -- ** MonitoringGroundTruthS3Input
    monitoringGroundTruthS3Input_s3Uri,

    -- ** MonitoringInput
    monitoringInput_batchTransformInput,
    monitoringInput_endpointInput,

    -- ** MonitoringJobDefinition
    monitoringJobDefinition_baselineConfig,
    monitoringJobDefinition_environment,
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
    monitoringNetworkConfig_enableInterContainerTrafficEncryption,
    monitoringNetworkConfig_enableNetworkIsolation,
    monitoringNetworkConfig_vpcConfig,

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
    monitoringSchedule_creationTime,
    monitoringSchedule_endpointName,
    monitoringSchedule_failureReason,
    monitoringSchedule_lastModifiedTime,
    monitoringSchedule_lastMonitoringExecutionSummary,
    monitoringSchedule_monitoringScheduleArn,
    monitoringSchedule_monitoringScheduleConfig,
    monitoringSchedule_monitoringScheduleName,
    monitoringSchedule_monitoringScheduleStatus,
    monitoringSchedule_monitoringType,
    monitoringSchedule_tags,

    -- ** MonitoringScheduleConfig
    monitoringScheduleConfig_monitoringJobDefinition,
    monitoringScheduleConfig_monitoringJobDefinitionName,
    monitoringScheduleConfig_monitoringType,
    monitoringScheduleConfig_scheduleConfig,

    -- ** MonitoringScheduleSummary
    monitoringScheduleSummary_endpointName,
    monitoringScheduleSummary_monitoringJobDefinitionName,
    monitoringScheduleSummary_monitoringType,
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
    networkConfig_enableInterContainerTrafficEncryption,
    networkConfig_enableNetworkIsolation,
    networkConfig_vpcConfig,

    -- ** NotebookInstanceLifecycleConfigSummary
    notebookInstanceLifecycleConfigSummary_creationTime,
    notebookInstanceLifecycleConfigSummary_lastModifiedTime,
    notebookInstanceLifecycleConfigSummary_notebookInstanceLifecycleConfigName,
    notebookInstanceLifecycleConfigSummary_notebookInstanceLifecycleConfigArn,

    -- ** NotebookInstanceLifecycleHook
    notebookInstanceLifecycleHook_content,

    -- ** NotebookInstanceSummary
    notebookInstanceSummary_additionalCodeRepositories,
    notebookInstanceSummary_creationTime,
    notebookInstanceSummary_defaultCodeRepository,
    notebookInstanceSummary_instanceType,
    notebookInstanceSummary_lastModifiedTime,
    notebookInstanceSummary_notebookInstanceLifecycleConfigName,
    notebookInstanceSummary_notebookInstanceStatus,
    notebookInstanceSummary_url,
    notebookInstanceSummary_notebookInstanceName,
    notebookInstanceSummary_notebookInstanceArn,

    -- ** NotificationConfiguration
    notificationConfiguration_notificationTopicArn,

    -- ** ObjectiveStatusCounters
    objectiveStatusCounters_failed,
    objectiveStatusCounters_pending,
    objectiveStatusCounters_succeeded,

    -- ** OfflineStoreConfig
    offlineStoreConfig_dataCatalogConfig,
    offlineStoreConfig_disableGlueTableCreation,
    offlineStoreConfig_tableFormat,
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
    oidcConfigForResponse_authorizationEndpoint,
    oidcConfigForResponse_clientId,
    oidcConfigForResponse_issuer,
    oidcConfigForResponse_jwksUri,
    oidcConfigForResponse_logoutEndpoint,
    oidcConfigForResponse_tokenEndpoint,
    oidcConfigForResponse_userInfoEndpoint,

    -- ** OidcMemberDefinition
    oidcMemberDefinition_groups,

    -- ** OnlineStoreConfig
    onlineStoreConfig_enableOnlineStore,
    onlineStoreConfig_securityConfig,

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

    -- ** OutputParameter
    outputParameter_name,
    outputParameter_value,

    -- ** ParallelismConfiguration
    parallelismConfiguration_maxParallelExecutionSteps,

    -- ** Parameter
    parameter_name,
    parameter_value,

    -- ** ParameterRange
    parameterRange_categoricalParameterRangeSpecification,
    parameterRange_continuousParameterRangeSpecification,
    parameterRange_integerParameterRangeSpecification,

    -- ** ParameterRanges
    parameterRanges_categoricalParameterRanges,
    parameterRanges_continuousParameterRanges,
    parameterRanges_integerParameterRanges,

    -- ** Parent
    parent_experimentName,
    parent_trialName,

    -- ** ParentHyperParameterTuningJob
    parentHyperParameterTuningJob_hyperParameterTuningJobName,

    -- ** PendingDeploymentSummary
    pendingDeploymentSummary_productionVariants,
    pendingDeploymentSummary_shadowProductionVariants,
    pendingDeploymentSummary_startTime,
    pendingDeploymentSummary_endpointConfigName,

    -- ** PendingProductionVariantSummary
    pendingProductionVariantSummary_acceleratorType,
    pendingProductionVariantSummary_currentInstanceCount,
    pendingProductionVariantSummary_currentServerlessConfig,
    pendingProductionVariantSummary_currentWeight,
    pendingProductionVariantSummary_deployedImages,
    pendingProductionVariantSummary_desiredInstanceCount,
    pendingProductionVariantSummary_desiredServerlessConfig,
    pendingProductionVariantSummary_desiredWeight,
    pendingProductionVariantSummary_instanceType,
    pendingProductionVariantSummary_variantStatus,
    pendingProductionVariantSummary_variantName,

    -- ** Phase
    phase_durationInSeconds,
    phase_initialNumberOfUsers,
    phase_spawnRate,

    -- ** Pipeline
    pipeline_createdBy,
    pipeline_creationTime,
    pipeline_lastModifiedBy,
    pipeline_lastModifiedTime,
    pipeline_lastRunTime,
    pipeline_parallelismConfiguration,
    pipeline_pipelineArn,
    pipeline_pipelineDescription,
    pipeline_pipelineDisplayName,
    pipeline_pipelineName,
    pipeline_pipelineStatus,
    pipeline_roleArn,
    pipeline_tags,

    -- ** PipelineDefinitionS3Location
    pipelineDefinitionS3Location_versionId,
    pipelineDefinitionS3Location_bucket,
    pipelineDefinitionS3Location_objectKey,

    -- ** PipelineExecution
    pipelineExecution_createdBy,
    pipelineExecution_creationTime,
    pipelineExecution_failureReason,
    pipelineExecution_lastModifiedBy,
    pipelineExecution_lastModifiedTime,
    pipelineExecution_parallelismConfiguration,
    pipelineExecution_pipelineArn,
    pipelineExecution_pipelineExecutionArn,
    pipelineExecution_pipelineExecutionDescription,
    pipelineExecution_pipelineExecutionDisplayName,
    pipelineExecution_pipelineExecutionStatus,
    pipelineExecution_pipelineExperimentConfig,
    pipelineExecution_pipelineParameters,

    -- ** PipelineExecutionStep
    pipelineExecutionStep_attemptCount,
    pipelineExecutionStep_cacheHitResult,
    pipelineExecutionStep_endTime,
    pipelineExecutionStep_failureReason,
    pipelineExecutionStep_metadata,
    pipelineExecutionStep_startTime,
    pipelineExecutionStep_stepDescription,
    pipelineExecutionStep_stepDisplayName,
    pipelineExecutionStep_stepName,
    pipelineExecutionStep_stepStatus,

    -- ** PipelineExecutionStepMetadata
    pipelineExecutionStepMetadata_autoMLJob,
    pipelineExecutionStepMetadata_callback,
    pipelineExecutionStepMetadata_clarifyCheck,
    pipelineExecutionStepMetadata_condition,
    pipelineExecutionStepMetadata_emr,
    pipelineExecutionStepMetadata_fail,
    pipelineExecutionStepMetadata_lambda,
    pipelineExecutionStepMetadata_model,
    pipelineExecutionStepMetadata_processingJob,
    pipelineExecutionStepMetadata_qualityCheck,
    pipelineExecutionStepMetadata_registerModel,
    pipelineExecutionStepMetadata_trainingJob,
    pipelineExecutionStepMetadata_transformJob,
    pipelineExecutionStepMetadata_tuningJob,

    -- ** PipelineExecutionSummary
    pipelineExecutionSummary_pipelineExecutionArn,
    pipelineExecutionSummary_pipelineExecutionDescription,
    pipelineExecutionSummary_pipelineExecutionDisplayName,
    pipelineExecutionSummary_pipelineExecutionFailureReason,
    pipelineExecutionSummary_pipelineExecutionStatus,
    pipelineExecutionSummary_startTime,

    -- ** PipelineExperimentConfig
    pipelineExperimentConfig_experimentName,
    pipelineExperimentConfig_trialName,

    -- ** PipelineSummary
    pipelineSummary_creationTime,
    pipelineSummary_lastExecutionTime,
    pipelineSummary_lastModifiedTime,
    pipelineSummary_pipelineArn,
    pipelineSummary_pipelineDescription,
    pipelineSummary_pipelineDisplayName,
    pipelineSummary_pipelineName,
    pipelineSummary_roleArn,

    -- ** ProcessingClusterConfig
    processingClusterConfig_volumeKmsKeyId,
    processingClusterConfig_instanceCount,
    processingClusterConfig_instanceType,
    processingClusterConfig_volumeSizeInGB,

    -- ** ProcessingFeatureStoreOutput
    processingFeatureStoreOutput_featureGroupName,

    -- ** ProcessingInput
    processingInput_appManaged,
    processingInput_datasetDefinition,
    processingInput_s3Input,
    processingInput_inputName,

    -- ** ProcessingJob
    processingJob_appSpecification,
    processingJob_autoMLJobArn,
    processingJob_creationTime,
    processingJob_environment,
    processingJob_exitMessage,
    processingJob_experimentConfig,
    processingJob_failureReason,
    processingJob_lastModifiedTime,
    processingJob_monitoringScheduleArn,
    processingJob_networkConfig,
    processingJob_processingEndTime,
    processingJob_processingInputs,
    processingJob_processingJobArn,
    processingJob_processingJobName,
    processingJob_processingJobStatus,
    processingJob_processingOutputConfig,
    processingJob_processingResources,
    processingJob_processingStartTime,
    processingJob_roleArn,
    processingJob_stoppingCondition,
    processingJob_tags,
    processingJob_trainingJobArn,

    -- ** ProcessingJobStepMetadata
    processingJobStepMetadata_arn,

    -- ** ProcessingJobSummary
    processingJobSummary_exitMessage,
    processingJobSummary_failureReason,
    processingJobSummary_lastModifiedTime,
    processingJobSummary_processingEndTime,
    processingJobSummary_processingJobName,
    processingJobSummary_processingJobArn,
    processingJobSummary_creationTime,
    processingJobSummary_processingJobStatus,

    -- ** ProcessingOutput
    processingOutput_appManaged,
    processingOutput_featureStoreOutput,
    processingOutput_s3Output,
    processingOutput_outputName,

    -- ** ProcessingOutputConfig
    processingOutputConfig_kmsKeyId,
    processingOutputConfig_outputs,

    -- ** ProcessingResources
    processingResources_clusterConfig,

    -- ** ProcessingS3Input
    processingS3Input_localPath,
    processingS3Input_s3CompressionType,
    processingS3Input_s3DataDistributionType,
    processingS3Input_s3InputMode,
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
    productionVariant_coreDumpConfig,
    productionVariant_initialInstanceCount,
    productionVariant_initialVariantWeight,
    productionVariant_instanceType,
    productionVariant_modelDataDownloadTimeoutInSeconds,
    productionVariant_serverlessConfig,
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
    productionVariantStatus_startTime,
    productionVariantStatus_statusMessage,
    productionVariantStatus_status,

    -- ** ProductionVariantSummary
    productionVariantSummary_currentInstanceCount,
    productionVariantSummary_currentServerlessConfig,
    productionVariantSummary_currentWeight,
    productionVariantSummary_deployedImages,
    productionVariantSummary_desiredInstanceCount,
    productionVariantSummary_desiredServerlessConfig,
    productionVariantSummary_desiredWeight,
    productionVariantSummary_variantStatus,
    productionVariantSummary_variantName,

    -- ** ProfilerConfig
    profilerConfig_disableProfiler,
    profilerConfig_profilingIntervalInMilliseconds,
    profilerConfig_profilingParameters,
    profilerConfig_s3OutputPath,

    -- ** ProfilerConfigForUpdate
    profilerConfigForUpdate_disableProfiler,
    profilerConfigForUpdate_profilingIntervalInMilliseconds,
    profilerConfigForUpdate_profilingParameters,
    profilerConfigForUpdate_s3OutputPath,

    -- ** ProfilerRuleConfiguration
    profilerRuleConfiguration_instanceType,
    profilerRuleConfiguration_localPath,
    profilerRuleConfiguration_ruleParameters,
    profilerRuleConfiguration_s3OutputPath,
    profilerRuleConfiguration_volumeSizeInGB,
    profilerRuleConfiguration_ruleConfigurationName,
    profilerRuleConfiguration_ruleEvaluatorImage,

    -- ** ProfilerRuleEvaluationStatus
    profilerRuleEvaluationStatus_lastModifiedTime,
    profilerRuleEvaluationStatus_ruleConfigurationName,
    profilerRuleEvaluationStatus_ruleEvaluationJobArn,
    profilerRuleEvaluationStatus_ruleEvaluationStatus,
    profilerRuleEvaluationStatus_statusDetails,

    -- ** Project
    project_createdBy,
    project_creationTime,
    project_lastModifiedBy,
    project_lastModifiedTime,
    project_projectArn,
    project_projectDescription,
    project_projectId,
    project_projectName,
    project_projectStatus,
    project_serviceCatalogProvisionedProductDetails,
    project_serviceCatalogProvisioningDetails,
    project_tags,

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
    qualityCheckStepMetadata_baselineUsedForDriftCheckConstraints,
    qualityCheckStepMetadata_baselineUsedForDriftCheckStatistics,
    qualityCheckStepMetadata_calculatedBaselineConstraints,
    qualityCheckStepMetadata_calculatedBaselineStatistics,
    qualityCheckStepMetadata_checkJobArn,
    qualityCheckStepMetadata_checkType,
    qualityCheckStepMetadata_modelPackageGroupName,
    qualityCheckStepMetadata_registerNewBaseline,
    qualityCheckStepMetadata_skipCheck,
    qualityCheckStepMetadata_violationReport,

    -- ** QueryFilters
    queryFilters_createdAfter,
    queryFilters_createdBefore,
    queryFilters_lineageTypes,
    queryFilters_modifiedAfter,
    queryFilters_modifiedBefore,
    queryFilters_properties,
    queryFilters_types,

    -- ** RSessionAppSettings
    rSessionAppSettings_customImages,
    rSessionAppSettings_defaultResourceSpec,

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
    rStudioServerProDomainSettingsForUpdate_rStudioConnectUrl,
    rStudioServerProDomainSettingsForUpdate_rStudioPackageManagerUrl,
    rStudioServerProDomainSettingsForUpdate_domainExecutionRoleArn,

    -- ** RealTimeInferenceConfig
    realTimeInferenceConfig_instanceType,
    realTimeInferenceConfig_instanceCount,

    -- ** RecommendationJobCompiledOutputConfig
    recommendationJobCompiledOutputConfig_s3OutputUri,

    -- ** RecommendationJobContainerConfig
    recommendationJobContainerConfig_domain,
    recommendationJobContainerConfig_framework,
    recommendationJobContainerConfig_frameworkVersion,
    recommendationJobContainerConfig_nearestModelName,
    recommendationJobContainerConfig_payloadConfig,
    recommendationJobContainerConfig_supportedInstanceTypes,
    recommendationJobContainerConfig_task,

    -- ** RecommendationJobInferenceBenchmark
    recommendationJobInferenceBenchmark_endpointConfiguration,
    recommendationJobInferenceBenchmark_failureReason,
    recommendationJobInferenceBenchmark_metrics,
    recommendationJobInferenceBenchmark_modelConfiguration,

    -- ** RecommendationJobInputConfig
    recommendationJobInputConfig_containerConfig,
    recommendationJobInputConfig_endpointConfigurations,
    recommendationJobInputConfig_endpoints,
    recommendationJobInputConfig_jobDurationInSeconds,
    recommendationJobInputConfig_resourceLimit,
    recommendationJobInputConfig_trafficPattern,
    recommendationJobInputConfig_volumeKmsKeyId,
    recommendationJobInputConfig_vpcConfig,
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

    -- ** RecommendationJobVpcConfig
    recommendationJobVpcConfig_securityGroupIds,
    recommendationJobVpcConfig_subnets,

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
    resolvedAttributes_autoMLJobObjective,
    resolvedAttributes_completionCriteria,
    resolvedAttributes_problemType,

    -- ** ResourceConfig
    resourceConfig_instanceCount,
    resourceConfig_instanceGroups,
    resourceConfig_instanceType,
    resourceConfig_keepAlivePeriodInSeconds,
    resourceConfig_volumeKmsKeyId,
    resourceConfig_volumeSizeInGB,

    -- ** ResourceConfigForUpdate
    resourceConfigForUpdate_keepAlivePeriodInSeconds,

    -- ** ResourceLimits
    resourceLimits_maxNumberOfTrainingJobs,
    resourceLimits_maxParallelTrainingJobs,

    -- ** ResourceSpec
    resourceSpec_instanceType,
    resourceSpec_lifecycleConfigArn,
    resourceSpec_sageMakerImageArn,
    resourceSpec_sageMakerImageVersionArn,

    -- ** RetentionPolicy
    retentionPolicy_homeEfsFileSystem,

    -- ** RetryStrategy
    retryStrategy_maximumRetryAttempts,

    -- ** S3DataSource
    s3DataSource_attributeNames,
    s3DataSource_instanceGroupNames,
    s3DataSource_s3DataDistributionType,
    s3DataSource_s3DataType,
    s3DataSource_s3Uri,

    -- ** S3StorageConfig
    s3StorageConfig_kmsKeyId,
    s3StorageConfig_resolvedOutputS3Uri,
    s3StorageConfig_s3Uri,

    -- ** ScheduleConfig
    scheduleConfig_scheduleExpression,

    -- ** SearchExpression
    searchExpression_filters,
    searchExpression_nestedFilters,
    searchExpression_operator,
    searchExpression_subExpressions,

    -- ** SearchRecord
    searchRecord_endpoint,
    searchRecord_experiment,
    searchRecord_featureGroup,
    searchRecord_featureMetadata,
    searchRecord_hyperParameterTuningJob,
    searchRecord_model,
    searchRecord_modelCard,
    searchRecord_modelPackage,
    searchRecord_modelPackageGroup,
    searchRecord_pipeline,
    searchRecord_pipelineExecution,
    searchRecord_project,
    searchRecord_trainingJob,
    searchRecord_trial,
    searchRecord_trialComponent,

    -- ** SecondaryStatusTransition
    secondaryStatusTransition_endTime,
    secondaryStatusTransition_statusMessage,
    secondaryStatusTransition_status,
    secondaryStatusTransition_startTime,

    -- ** ServiceCatalogProvisionedProductDetails
    serviceCatalogProvisionedProductDetails_provisionedProductId,
    serviceCatalogProvisionedProductDetails_provisionedProductStatusMessage,

    -- ** ServiceCatalogProvisioningDetails
    serviceCatalogProvisioningDetails_pathId,
    serviceCatalogProvisioningDetails_provisioningArtifactId,
    serviceCatalogProvisioningDetails_provisioningParameters,
    serviceCatalogProvisioningDetails_productId,

    -- ** ServiceCatalogProvisioningUpdateDetails
    serviceCatalogProvisioningUpdateDetails_provisioningArtifactId,
    serviceCatalogProvisioningUpdateDetails_provisioningParameters,

    -- ** ShadowModeConfig
    shadowModeConfig_sourceModelVariantName,
    shadowModeConfig_shadowModelVariants,

    -- ** ShadowModelVariantConfig
    shadowModelVariantConfig_shadowModelVariantName,
    shadowModelVariantConfig_samplingPercentage,

    -- ** SharingSettings
    sharingSettings_notebookOutputOption,
    sharingSettings_s3KmsKeyId,
    sharingSettings_s3OutputPath,

    -- ** ShuffleConfig
    shuffleConfig_seed,

    -- ** SourceAlgorithm
    sourceAlgorithm_modelDataUrl,
    sourceAlgorithm_algorithmName,

    -- ** SourceAlgorithmSpecification
    sourceAlgorithmSpecification_sourceAlgorithms,

    -- ** SourceIpConfig
    sourceIpConfig_cidrs,

    -- ** SpaceDetails
    spaceDetails_creationTime,
    spaceDetails_domainId,
    spaceDetails_lastModifiedTime,
    spaceDetails_spaceName,
    spaceDetails_status,

    -- ** SpaceSettings
    spaceSettings_jupyterServerAppSettings,
    spaceSettings_kernelGatewayAppSettings,

    -- ** StoppingCondition
    stoppingCondition_maxRuntimeInSeconds,
    stoppingCondition_maxWaitTimeInSeconds,

    -- ** StudioLifecycleConfigDetails
    studioLifecycleConfigDetails_creationTime,
    studioLifecycleConfigDetails_lastModifiedTime,
    studioLifecycleConfigDetails_studioLifecycleConfigAppType,
    studioLifecycleConfigDetails_studioLifecycleConfigArn,
    studioLifecycleConfigDetails_studioLifecycleConfigName,

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
    timeSeriesForecastingSettings_amazonForecastRoleArn,
    timeSeriesForecastingSettings_status,

    -- ** TrafficPattern
    trafficPattern_phases,
    trafficPattern_trafficType,

    -- ** TrafficRoutingConfig
    trafficRoutingConfig_canarySize,
    trafficRoutingConfig_linearStepSize,
    trafficRoutingConfig_type,
    trafficRoutingConfig_waitIntervalInSeconds,

    -- ** TrainingJob
    trainingJob_algorithmSpecification,
    trainingJob_autoMLJobArn,
    trainingJob_billableTimeInSeconds,
    trainingJob_checkpointConfig,
    trainingJob_creationTime,
    trainingJob_debugHookConfig,
    trainingJob_debugRuleConfigurations,
    trainingJob_debugRuleEvaluationStatuses,
    trainingJob_enableInterContainerTrafficEncryption,
    trainingJob_enableManagedSpotTraining,
    trainingJob_enableNetworkIsolation,
    trainingJob_environment,
    trainingJob_experimentConfig,
    trainingJob_failureReason,
    trainingJob_finalMetricDataList,
    trainingJob_hyperParameters,
    trainingJob_inputDataConfig,
    trainingJob_labelingJobArn,
    trainingJob_lastModifiedTime,
    trainingJob_modelArtifacts,
    trainingJob_outputDataConfig,
    trainingJob_resourceConfig,
    trainingJob_retryStrategy,
    trainingJob_roleArn,
    trainingJob_secondaryStatus,
    trainingJob_secondaryStatusTransitions,
    trainingJob_stoppingCondition,
    trainingJob_tags,
    trainingJob_tensorBoardOutputConfig,
    trainingJob_trainingEndTime,
    trainingJob_trainingJobArn,
    trainingJob_trainingJobName,
    trainingJob_trainingJobStatus,
    trainingJob_trainingStartTime,
    trainingJob_trainingTimeInSeconds,
    trainingJob_tuningJobArn,
    trainingJob_vpcConfig,

    -- ** TrainingJobDefinition
    trainingJobDefinition_hyperParameters,
    trainingJobDefinition_trainingInputMode,
    trainingJobDefinition_inputDataConfig,
    trainingJobDefinition_outputDataConfig,
    trainingJobDefinition_resourceConfig,
    trainingJobDefinition_stoppingCondition,

    -- ** TrainingJobStatusCounters
    trainingJobStatusCounters_completed,
    trainingJobStatusCounters_inProgress,
    trainingJobStatusCounters_nonRetryableError,
    trainingJobStatusCounters_retryableError,
    trainingJobStatusCounters_stopped,

    -- ** TrainingJobStepMetadata
    trainingJobStepMetadata_arn,

    -- ** TrainingJobSummary
    trainingJobSummary_lastModifiedTime,
    trainingJobSummary_trainingEndTime,
    trainingJobSummary_warmPoolStatus,
    trainingJobSummary_trainingJobName,
    trainingJobSummary_trainingJobArn,
    trainingJobSummary_creationTime,
    trainingJobSummary_trainingJobStatus,

    -- ** TrainingSpecification
    trainingSpecification_metricDefinitions,
    trainingSpecification_supportedHyperParameters,
    trainingSpecification_supportedTuningJobObjectiveMetrics,
    trainingSpecification_supportsDistributedTraining,
    trainingSpecification_trainingImageDigest,
    trainingSpecification_trainingImage,
    trainingSpecification_supportedTrainingInstanceTypes,
    trainingSpecification_trainingChannels,

    -- ** TransformDataSource
    transformDataSource_s3DataSource,

    -- ** TransformInput
    transformInput_compressionType,
    transformInput_contentType,
    transformInput_splitType,
    transformInput_dataSource,

    -- ** TransformJob
    transformJob_autoMLJobArn,
    transformJob_batchStrategy,
    transformJob_creationTime,
    transformJob_dataProcessing,
    transformJob_environment,
    transformJob_experimentConfig,
    transformJob_failureReason,
    transformJob_labelingJobArn,
    transformJob_maxConcurrentTransforms,
    transformJob_maxPayloadInMB,
    transformJob_modelClientConfig,
    transformJob_modelName,
    transformJob_tags,
    transformJob_transformEndTime,
    transformJob_transformInput,
    transformJob_transformJobArn,
    transformJob_transformJobName,
    transformJob_transformJobStatus,
    transformJob_transformOutput,
    transformJob_transformResources,
    transformJob_transformStartTime,

    -- ** TransformJobDefinition
    transformJobDefinition_batchStrategy,
    transformJobDefinition_environment,
    transformJobDefinition_maxConcurrentTransforms,
    transformJobDefinition_maxPayloadInMB,
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
    trial_createdBy,
    trial_creationTime,
    trial_displayName,
    trial_experimentName,
    trial_lastModifiedBy,
    trial_lastModifiedTime,
    trial_metadataProperties,
    trial_source,
    trial_tags,
    trial_trialArn,
    trial_trialComponentSummaries,
    trial_trialName,

    -- ** TrialComponent
    trialComponent_createdBy,
    trialComponent_creationTime,
    trialComponent_displayName,
    trialComponent_endTime,
    trialComponent_inputArtifacts,
    trialComponent_lastModifiedBy,
    trialComponent_lastModifiedTime,
    trialComponent_lineageGroupArn,
    trialComponent_metadataProperties,
    trialComponent_metrics,
    trialComponent_outputArtifacts,
    trialComponent_parameters,
    trialComponent_parents,
    trialComponent_runName,
    trialComponent_source,
    trialComponent_sourceDetail,
    trialComponent_startTime,
    trialComponent_status,
    trialComponent_tags,
    trialComponent_trialComponentArn,
    trialComponent_trialComponentName,

    -- ** TrialComponentArtifact
    trialComponentArtifact_mediaType,
    trialComponentArtifact_value,

    -- ** TrialComponentMetricSummary
    trialComponentMetricSummary_avg,
    trialComponentMetricSummary_count,
    trialComponentMetricSummary_last,
    trialComponentMetricSummary_max,
    trialComponentMetricSummary_metricName,
    trialComponentMetricSummary_min,
    trialComponentMetricSummary_sourceArn,
    trialComponentMetricSummary_stdDev,
    trialComponentMetricSummary_timeStamp,

    -- ** TrialComponentParameterValue
    trialComponentParameterValue_numberValue,
    trialComponentParameterValue_stringValue,

    -- ** TrialComponentSimpleSummary
    trialComponentSimpleSummary_createdBy,
    trialComponentSimpleSummary_creationTime,
    trialComponentSimpleSummary_trialComponentArn,
    trialComponentSimpleSummary_trialComponentName,
    trialComponentSimpleSummary_trialComponentSource,

    -- ** TrialComponentSource
    trialComponentSource_sourceType,
    trialComponentSource_sourceArn,

    -- ** TrialComponentSourceDetail
    trialComponentSourceDetail_processingJob,
    trialComponentSourceDetail_sourceArn,
    trialComponentSourceDetail_trainingJob,
    trialComponentSourceDetail_transformJob,

    -- ** TrialComponentStatus
    trialComponentStatus_message,
    trialComponentStatus_primaryStatus,

    -- ** TrialComponentSummary
    trialComponentSummary_createdBy,
    trialComponentSummary_creationTime,
    trialComponentSummary_displayName,
    trialComponentSummary_endTime,
    trialComponentSummary_lastModifiedBy,
    trialComponentSummary_lastModifiedTime,
    trialComponentSummary_startTime,
    trialComponentSummary_status,
    trialComponentSummary_trialComponentArn,
    trialComponentSummary_trialComponentName,
    trialComponentSummary_trialComponentSource,

    -- ** TrialSource
    trialSource_sourceType,
    trialSource_sourceArn,

    -- ** TrialSummary
    trialSummary_creationTime,
    trialSummary_displayName,
    trialSummary_lastModifiedTime,
    trialSummary_trialArn,
    trialSummary_trialName,
    trialSummary_trialSource,

    -- ** TuningJobCompletionCriteria
    tuningJobCompletionCriteria_targetObjectiveMetricValue,

    -- ** TuningJobStepMetaData
    tuningJobStepMetaData_arn,

    -- ** USD
    usd_cents,
    usd_dollars,
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
    userContext_domainId,
    userContext_userProfileArn,
    userContext_userProfileName,

    -- ** UserProfileDetails
    userProfileDetails_creationTime,
    userProfileDetails_domainId,
    userProfileDetails_lastModifiedTime,
    userProfileDetails_status,
    userProfileDetails_userProfileName,

    -- ** UserSettings
    userSettings_canvasAppSettings,
    userSettings_executionRole,
    userSettings_jupyterServerAppSettings,
    userSettings_kernelGatewayAppSettings,
    userSettings_rSessionAppSettings,
    userSettings_rStudioServerProAppSettings,
    userSettings_securityGroups,
    userSettings_sharingSettings,
    userSettings_tensorBoardAppSettings,

    -- ** VariantProperty
    variantProperty_variantPropertyType,

    -- ** Vertex
    vertex_arn,
    vertex_lineageType,
    vertex_type,

    -- ** VpcConfig
    vpcConfig_securityGroupIds,
    vpcConfig_subnets,

    -- ** WarmPoolStatus
    warmPoolStatus_resourceRetainedBillableTimeInSeconds,
    warmPoolStatus_reusedByJob,
    warmPoolStatus_status,

    -- ** Workforce
    workforce_cognitoConfig,
    workforce_createDate,
    workforce_failureReason,
    workforce_lastUpdatedDate,
    workforce_oidcConfig,
    workforce_sourceIpConfig,
    workforce_status,
    workforce_subDomain,
    workforce_workforceVpcConfig,
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
    workteam_createDate,
    workteam_lastUpdatedDate,
    workteam_notificationConfiguration,
    workteam_productListingIds,
    workteam_subDomain,
    workteam_workforceArn,
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
import Amazonka.SageMaker.CreateHub
import Amazonka.SageMaker.CreateHumanTaskUi
import Amazonka.SageMaker.CreateHyperParameterTuningJob
import Amazonka.SageMaker.CreateImage
import Amazonka.SageMaker.CreateImageVersion
import Amazonka.SageMaker.CreateInferenceExperiment
import Amazonka.SageMaker.CreateInferenceRecommendationsJob
import Amazonka.SageMaker.CreateLabelingJob
import Amazonka.SageMaker.CreateModel
import Amazonka.SageMaker.CreateModelBiasJobDefinition
import Amazonka.SageMaker.CreateModelCard
import Amazonka.SageMaker.CreateModelCardExportJob
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
import Amazonka.SageMaker.CreateSpace
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
import Amazonka.SageMaker.DeleteHub
import Amazonka.SageMaker.DeleteHubContent
import Amazonka.SageMaker.DeleteHumanTaskUi
import Amazonka.SageMaker.DeleteImage
import Amazonka.SageMaker.DeleteImageVersion
import Amazonka.SageMaker.DeleteInferenceExperiment
import Amazonka.SageMaker.DeleteModel
import Amazonka.SageMaker.DeleteModelBiasJobDefinition
import Amazonka.SageMaker.DeleteModelCard
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
import Amazonka.SageMaker.DeleteSpace
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
import Amazonka.SageMaker.DescribeHub
import Amazonka.SageMaker.DescribeHubContent
import Amazonka.SageMaker.DescribeHumanTaskUi
import Amazonka.SageMaker.DescribeHyperParameterTuningJob
import Amazonka.SageMaker.DescribeImage
import Amazonka.SageMaker.DescribeImageVersion
import Amazonka.SageMaker.DescribeInferenceExperiment
import Amazonka.SageMaker.DescribeInferenceRecommendationsJob
import Amazonka.SageMaker.DescribeLabelingJob
import Amazonka.SageMaker.DescribeLineageGroup
import Amazonka.SageMaker.DescribeModel
import Amazonka.SageMaker.DescribeModelBiasJobDefinition
import Amazonka.SageMaker.DescribeModelCard
import Amazonka.SageMaker.DescribeModelCardExportJob
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
import Amazonka.SageMaker.DescribeSpace
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
import Amazonka.SageMaker.ImportHubContent
import Amazonka.SageMaker.ListActions
import Amazonka.SageMaker.ListAlgorithms
import Amazonka.SageMaker.ListAliases
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
import Amazonka.SageMaker.ListHubContentVersions
import Amazonka.SageMaker.ListHubContents
import Amazonka.SageMaker.ListHubs
import Amazonka.SageMaker.ListHumanTaskUis
import Amazonka.SageMaker.ListHyperParameterTuningJobs
import Amazonka.SageMaker.ListImageVersions
import Amazonka.SageMaker.ListImages
import Amazonka.SageMaker.ListInferenceExperiments
import Amazonka.SageMaker.ListInferenceRecommendationsJobSteps
import Amazonka.SageMaker.ListInferenceRecommendationsJobs
import Amazonka.SageMaker.ListLabelingJobs
import Amazonka.SageMaker.ListLabelingJobsForWorkteam
import Amazonka.SageMaker.ListLineageGroups
import Amazonka.SageMaker.ListModelBiasJobDefinitions
import Amazonka.SageMaker.ListModelCardExportJobs
import Amazonka.SageMaker.ListModelCardVersions
import Amazonka.SageMaker.ListModelCards
import Amazonka.SageMaker.ListModelExplainabilityJobDefinitions
import Amazonka.SageMaker.ListModelMetadata
import Amazonka.SageMaker.ListModelPackageGroups
import Amazonka.SageMaker.ListModelPackages
import Amazonka.SageMaker.ListModelQualityJobDefinitions
import Amazonka.SageMaker.ListModels
import Amazonka.SageMaker.ListMonitoringAlertHistory
import Amazonka.SageMaker.ListMonitoringAlerts
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
import Amazonka.SageMaker.ListSpaces
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
import Amazonka.SageMaker.StartInferenceExperiment
import Amazonka.SageMaker.StartMonitoringSchedule
import Amazonka.SageMaker.StartNotebookInstance
import Amazonka.SageMaker.StartPipelineExecution
import Amazonka.SageMaker.StopAutoMLJob
import Amazonka.SageMaker.StopCompilationJob
import Amazonka.SageMaker.StopEdgeDeploymentStage
import Amazonka.SageMaker.StopEdgePackagingJob
import Amazonka.SageMaker.StopHyperParameterTuningJob
import Amazonka.SageMaker.StopInferenceExperiment
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
import Amazonka.SageMaker.Types.AutoMLJobStepMetadata
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
import Amazonka.SageMaker.Types.CodeRepository
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
import Amazonka.SageMaker.Types.DefaultSpaceSettings
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
import Amazonka.SageMaker.Types.EndpointMetadata
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
import Amazonka.SageMaker.Types.HubContentDependency
import Amazonka.SageMaker.Types.HubContentInfo
import Amazonka.SageMaker.Types.HubInfo
import Amazonka.SageMaker.Types.HubS3StorageConfig
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
import Amazonka.SageMaker.Types.InferenceExperimentDataStorageConfig
import Amazonka.SageMaker.Types.InferenceExperimentSchedule
import Amazonka.SageMaker.Types.InferenceExperimentSummary
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
import Amazonka.SageMaker.Types.Model
import Amazonka.SageMaker.Types.ModelArtifacts
import Amazonka.SageMaker.Types.ModelBiasAppSpecification
import Amazonka.SageMaker.Types.ModelBiasBaselineConfig
import Amazonka.SageMaker.Types.ModelBiasJobInput
import Amazonka.SageMaker.Types.ModelCard
import Amazonka.SageMaker.Types.ModelCardExportArtifacts
import Amazonka.SageMaker.Types.ModelCardExportJobSummary
import Amazonka.SageMaker.Types.ModelCardExportOutputConfig
import Amazonka.SageMaker.Types.ModelCardSecurityConfig
import Amazonka.SageMaker.Types.ModelCardSummary
import Amazonka.SageMaker.Types.ModelCardVersionSummary
import Amazonka.SageMaker.Types.ModelClientConfig
import Amazonka.SageMaker.Types.ModelConfiguration
import Amazonka.SageMaker.Types.ModelDashboardEndpoint
import Amazonka.SageMaker.Types.ModelDashboardIndicatorAction
import Amazonka.SageMaker.Types.ModelDashboardModel
import Amazonka.SageMaker.Types.ModelDashboardModelCard
import Amazonka.SageMaker.Types.ModelDashboardMonitoringSchedule
import Amazonka.SageMaker.Types.ModelDataQuality
import Amazonka.SageMaker.Types.ModelDeployConfig
import Amazonka.SageMaker.Types.ModelDeployResult
import Amazonka.SageMaker.Types.ModelDigests
import Amazonka.SageMaker.Types.ModelExplainabilityAppSpecification
import Amazonka.SageMaker.Types.ModelExplainabilityBaselineConfig
import Amazonka.SageMaker.Types.ModelExplainabilityJobInput
import Amazonka.SageMaker.Types.ModelInfrastructureConfig
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
import Amazonka.SageMaker.Types.ModelVariantConfig
import Amazonka.SageMaker.Types.ModelVariantConfigSummary
import Amazonka.SageMaker.Types.MonitoringAlertActions
import Amazonka.SageMaker.Types.MonitoringAlertHistorySummary
import Amazonka.SageMaker.Types.MonitoringAlertSummary
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
import Amazonka.SageMaker.Types.RealTimeInferenceConfig
import Amazonka.SageMaker.Types.RecommendationJobCompiledOutputConfig
import Amazonka.SageMaker.Types.RecommendationJobContainerConfig
import Amazonka.SageMaker.Types.RecommendationJobInferenceBenchmark
import Amazonka.SageMaker.Types.RecommendationJobInputConfig
import Amazonka.SageMaker.Types.RecommendationJobOutputConfig
import Amazonka.SageMaker.Types.RecommendationJobPayloadConfig
import Amazonka.SageMaker.Types.RecommendationJobResourceLimit
import Amazonka.SageMaker.Types.RecommendationJobStoppingConditions
import Amazonka.SageMaker.Types.RecommendationJobVpcConfig
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
import Amazonka.SageMaker.Types.ShadowModeConfig
import Amazonka.SageMaker.Types.ShadowModelVariantConfig
import Amazonka.SageMaker.Types.SharingSettings
import Amazonka.SageMaker.Types.ShuffleConfig
import Amazonka.SageMaker.Types.SourceAlgorithm
import Amazonka.SageMaker.Types.SourceAlgorithmSpecification
import Amazonka.SageMaker.Types.SourceIpConfig
import Amazonka.SageMaker.Types.SpaceDetails
import Amazonka.SageMaker.Types.SpaceSettings
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
import Amazonka.SageMaker.UpdateHub
import Amazonka.SageMaker.UpdateImage
import Amazonka.SageMaker.UpdateImageVersion
import Amazonka.SageMaker.UpdateInferenceExperiment
import Amazonka.SageMaker.UpdateModelCard
import Amazonka.SageMaker.UpdateModelPackage
import Amazonka.SageMaker.UpdateMonitoringAlert
import Amazonka.SageMaker.UpdateMonitoringSchedule
import Amazonka.SageMaker.UpdateNotebookInstance
import Amazonka.SageMaker.UpdateNotebookInstanceLifecycleConfig
import Amazonka.SageMaker.UpdatePipeline
import Amazonka.SageMaker.UpdatePipelineExecution
import Amazonka.SageMaker.UpdateProject
import Amazonka.SageMaker.UpdateSpace
import Amazonka.SageMaker.UpdateTrainingJob
import Amazonka.SageMaker.UpdateTrial
import Amazonka.SageMaker.UpdateTrialComponent
import Amazonka.SageMaker.UpdateUserProfile
import Amazonka.SageMaker.UpdateWorkforce
import Amazonka.SageMaker.UpdateWorkteam
