{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.CreateCloudFormationChangeSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS CloudFormation change set for the given application.
module Network.AWS.ServerlessApplicationRepository.CreateCloudFormationChangeSet
  ( -- * Creating a Request
    CreateCloudFormationChangeSet (..),
    newCreateCloudFormationChangeSet,

    -- * Request Lenses
    createCloudFormationChangeSet_resourceTypes,
    createCloudFormationChangeSet_capabilities,
    createCloudFormationChangeSet_parameterOverrides,
    createCloudFormationChangeSet_notificationArns,
    createCloudFormationChangeSet_tags,
    createCloudFormationChangeSet_rollbackConfiguration,
    createCloudFormationChangeSet_description,
    createCloudFormationChangeSet_changeSetName,
    createCloudFormationChangeSet_semanticVersion,
    createCloudFormationChangeSet_templateId,
    createCloudFormationChangeSet_clientToken,
    createCloudFormationChangeSet_applicationId,
    createCloudFormationChangeSet_stackName,

    -- * Destructuring the Response
    CreateCloudFormationChangeSetResponse (..),
    newCreateCloudFormationChangeSetResponse,

    -- * Response Lenses
    createCloudFormationChangeSetResponse_applicationId,
    createCloudFormationChangeSetResponse_stackId,
    createCloudFormationChangeSetResponse_changeSetId,
    createCloudFormationChangeSetResponse_semanticVersion,
    createCloudFormationChangeSetResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'newCreateCloudFormationChangeSet' smart constructor.
data CreateCloudFormationChangeSet = CreateCloudFormationChangeSet'
  { -- | This property corresponds to the parameter of the same name for the /AWS
    -- CloudFormation
    -- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
    -- API.
    resourceTypes :: Core.Maybe [Core.Text],
    -- | A list of values that you must specify before you can deploy certain
    -- applications. Some applications might include resources that can affect
    -- permissions in your AWS account, for example, by creating new AWS
    -- Identity and Access Management (IAM) users. For those applications, you
    -- must explicitly acknowledge their capabilities by specifying this
    -- parameter.
    --
    -- The only valid values are CAPABILITY_IAM, CAPABILITY_NAMED_IAM,
    -- CAPABILITY_RESOURCE_POLICY, and CAPABILITY_AUTO_EXPAND.
    --
    -- The following resources require you to specify CAPABILITY_IAM or
    -- CAPABILITY_NAMED_IAM:
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group>,
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile>,
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM::Policy>,
    -- and
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role>.
    -- If the application contains IAM resources, you can specify either
    -- CAPABILITY_IAM or CAPABILITY_NAMED_IAM. If the application contains IAM
    -- resources with custom names, you must specify CAPABILITY_NAMED_IAM.
    --
    -- The following resources require you to specify
    -- CAPABILITY_RESOURCE_POLICY:
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-lambda-permission.html AWS::Lambda::Permission>,
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM:Policy>,
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-applicationautoscaling-scalingpolicy.html AWS::ApplicationAutoScaling::ScalingPolicy>,
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-policy.html AWS::S3::BucketPolicy>,
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sqs-policy.html AWS::SQS::QueuePolicy>,
    -- and
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sns-policy.html AWS::SNS:TopicPolicy>.
    --
    -- Applications that contain one or more nested applications require you to
    -- specify CAPABILITY_AUTO_EXPAND.
    --
    -- If your application template contains any of the above resources, we
    -- recommend that you review all permissions associated with the
    -- application before deploying. If you don\'t specify this parameter for
    -- an application that requires capabilities, the call will fail.
    capabilities :: Core.Maybe [Core.Text],
    -- | A list of parameter values for the parameters of the application.
    parameterOverrides :: Core.Maybe [ParameterValue],
    -- | This property corresponds to the parameter of the same name for the /AWS
    -- CloudFormation
    -- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
    -- API.
    notificationArns :: Core.Maybe [Core.Text],
    -- | This property corresponds to the parameter of the same name for the /AWS
    -- CloudFormation
    -- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
    -- API.
    tags :: Core.Maybe [Tag],
    -- | This property corresponds to the parameter of the same name for the /AWS
    -- CloudFormation
    -- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
    -- API.
    rollbackConfiguration :: Core.Maybe RollbackConfiguration,
    -- | This property corresponds to the parameter of the same name for the /AWS
    -- CloudFormation
    -- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
    -- API.
    description :: Core.Maybe Core.Text,
    -- | This property corresponds to the parameter of the same name for the /AWS
    -- CloudFormation
    -- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
    -- API.
    changeSetName :: Core.Maybe Core.Text,
    -- | The semantic version of the application:
    --
    -- <https://semver.org/>
    semanticVersion :: Core.Maybe Core.Text,
    -- | The UUID returned by CreateCloudFormationTemplate.
    --
    -- Pattern:
    -- [0-9a-fA-F]{8}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{12}
    templateId :: Core.Maybe Core.Text,
    -- | This property corresponds to the parameter of the same name for the /AWS
    -- CloudFormation
    -- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
    -- API.
    clientToken :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Core.Text,
    -- | This property corresponds to the parameter of the same name for the /AWS
    -- CloudFormation
    -- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
    -- API.
    stackName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCloudFormationChangeSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceTypes', 'createCloudFormationChangeSet_resourceTypes' - This property corresponds to the parameter of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
-- API.
--
-- 'capabilities', 'createCloudFormationChangeSet_capabilities' - A list of values that you must specify before you can deploy certain
-- applications. Some applications might include resources that can affect
-- permissions in your AWS account, for example, by creating new AWS
-- Identity and Access Management (IAM) users. For those applications, you
-- must explicitly acknowledge their capabilities by specifying this
-- parameter.
--
-- The only valid values are CAPABILITY_IAM, CAPABILITY_NAMED_IAM,
-- CAPABILITY_RESOURCE_POLICY, and CAPABILITY_AUTO_EXPAND.
--
-- The following resources require you to specify CAPABILITY_IAM or
-- CAPABILITY_NAMED_IAM:
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM::Policy>,
-- and
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role>.
-- If the application contains IAM resources, you can specify either
-- CAPABILITY_IAM or CAPABILITY_NAMED_IAM. If the application contains IAM
-- resources with custom names, you must specify CAPABILITY_NAMED_IAM.
--
-- The following resources require you to specify
-- CAPABILITY_RESOURCE_POLICY:
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-lambda-permission.html AWS::Lambda::Permission>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM:Policy>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-applicationautoscaling-scalingpolicy.html AWS::ApplicationAutoScaling::ScalingPolicy>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-policy.html AWS::S3::BucketPolicy>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sqs-policy.html AWS::SQS::QueuePolicy>,
-- and
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sns-policy.html AWS::SNS:TopicPolicy>.
--
-- Applications that contain one or more nested applications require you to
-- specify CAPABILITY_AUTO_EXPAND.
--
-- If your application template contains any of the above resources, we
-- recommend that you review all permissions associated with the
-- application before deploying. If you don\'t specify this parameter for
-- an application that requires capabilities, the call will fail.
--
-- 'parameterOverrides', 'createCloudFormationChangeSet_parameterOverrides' - A list of parameter values for the parameters of the application.
--
-- 'notificationArns', 'createCloudFormationChangeSet_notificationArns' - This property corresponds to the parameter of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
-- API.
--
-- 'tags', 'createCloudFormationChangeSet_tags' - This property corresponds to the parameter of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
-- API.
--
-- 'rollbackConfiguration', 'createCloudFormationChangeSet_rollbackConfiguration' - This property corresponds to the parameter of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
-- API.
--
-- 'description', 'createCloudFormationChangeSet_description' - This property corresponds to the parameter of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
-- API.
--
-- 'changeSetName', 'createCloudFormationChangeSet_changeSetName' - This property corresponds to the parameter of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
-- API.
--
-- 'semanticVersion', 'createCloudFormationChangeSet_semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/>
--
-- 'templateId', 'createCloudFormationChangeSet_templateId' - The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern:
-- [0-9a-fA-F]{8}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{12}
--
-- 'clientToken', 'createCloudFormationChangeSet_clientToken' - This property corresponds to the parameter of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
-- API.
--
-- 'applicationId', 'createCloudFormationChangeSet_applicationId' - The Amazon Resource Name (ARN) of the application.
--
-- 'stackName', 'createCloudFormationChangeSet_stackName' - This property corresponds to the parameter of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
-- API.
newCreateCloudFormationChangeSet ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'stackName'
  Core.Text ->
  CreateCloudFormationChangeSet
newCreateCloudFormationChangeSet
  pApplicationId_
  pStackName_ =
    CreateCloudFormationChangeSet'
      { resourceTypes =
          Core.Nothing,
        capabilities = Core.Nothing,
        parameterOverrides = Core.Nothing,
        notificationArns = Core.Nothing,
        tags = Core.Nothing,
        rollbackConfiguration = Core.Nothing,
        description = Core.Nothing,
        changeSetName = Core.Nothing,
        semanticVersion = Core.Nothing,
        templateId = Core.Nothing,
        clientToken = Core.Nothing,
        applicationId = pApplicationId_,
        stackName = pStackName_
      }

-- | This property corresponds to the parameter of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
-- API.
createCloudFormationChangeSet_resourceTypes :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe [Core.Text])
createCloudFormationChangeSet_resourceTypes = Lens.lens (\CreateCloudFormationChangeSet' {resourceTypes} -> resourceTypes) (\s@CreateCloudFormationChangeSet' {} a -> s {resourceTypes = a} :: CreateCloudFormationChangeSet) Core.. Lens.mapping Lens._Coerce

-- | A list of values that you must specify before you can deploy certain
-- applications. Some applications might include resources that can affect
-- permissions in your AWS account, for example, by creating new AWS
-- Identity and Access Management (IAM) users. For those applications, you
-- must explicitly acknowledge their capabilities by specifying this
-- parameter.
--
-- The only valid values are CAPABILITY_IAM, CAPABILITY_NAMED_IAM,
-- CAPABILITY_RESOURCE_POLICY, and CAPABILITY_AUTO_EXPAND.
--
-- The following resources require you to specify CAPABILITY_IAM or
-- CAPABILITY_NAMED_IAM:
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM::Policy>,
-- and
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role>.
-- If the application contains IAM resources, you can specify either
-- CAPABILITY_IAM or CAPABILITY_NAMED_IAM. If the application contains IAM
-- resources with custom names, you must specify CAPABILITY_NAMED_IAM.
--
-- The following resources require you to specify
-- CAPABILITY_RESOURCE_POLICY:
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-lambda-permission.html AWS::Lambda::Permission>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM:Policy>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-applicationautoscaling-scalingpolicy.html AWS::ApplicationAutoScaling::ScalingPolicy>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-policy.html AWS::S3::BucketPolicy>,
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sqs-policy.html AWS::SQS::QueuePolicy>,
-- and
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sns-policy.html AWS::SNS:TopicPolicy>.
--
-- Applications that contain one or more nested applications require you to
-- specify CAPABILITY_AUTO_EXPAND.
--
-- If your application template contains any of the above resources, we
-- recommend that you review all permissions associated with the
-- application before deploying. If you don\'t specify this parameter for
-- an application that requires capabilities, the call will fail.
createCloudFormationChangeSet_capabilities :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe [Core.Text])
createCloudFormationChangeSet_capabilities = Lens.lens (\CreateCloudFormationChangeSet' {capabilities} -> capabilities) (\s@CreateCloudFormationChangeSet' {} a -> s {capabilities = a} :: CreateCloudFormationChangeSet) Core.. Lens.mapping Lens._Coerce

-- | A list of parameter values for the parameters of the application.
createCloudFormationChangeSet_parameterOverrides :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe [ParameterValue])
createCloudFormationChangeSet_parameterOverrides = Lens.lens (\CreateCloudFormationChangeSet' {parameterOverrides} -> parameterOverrides) (\s@CreateCloudFormationChangeSet' {} a -> s {parameterOverrides = a} :: CreateCloudFormationChangeSet) Core.. Lens.mapping Lens._Coerce

-- | This property corresponds to the parameter of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
-- API.
createCloudFormationChangeSet_notificationArns :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe [Core.Text])
createCloudFormationChangeSet_notificationArns = Lens.lens (\CreateCloudFormationChangeSet' {notificationArns} -> notificationArns) (\s@CreateCloudFormationChangeSet' {} a -> s {notificationArns = a} :: CreateCloudFormationChangeSet) Core.. Lens.mapping Lens._Coerce

-- | This property corresponds to the parameter of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
-- API.
createCloudFormationChangeSet_tags :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe [Tag])
createCloudFormationChangeSet_tags = Lens.lens (\CreateCloudFormationChangeSet' {tags} -> tags) (\s@CreateCloudFormationChangeSet' {} a -> s {tags = a} :: CreateCloudFormationChangeSet) Core.. Lens.mapping Lens._Coerce

-- | This property corresponds to the parameter of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
-- API.
createCloudFormationChangeSet_rollbackConfiguration :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe RollbackConfiguration)
createCloudFormationChangeSet_rollbackConfiguration = Lens.lens (\CreateCloudFormationChangeSet' {rollbackConfiguration} -> rollbackConfiguration) (\s@CreateCloudFormationChangeSet' {} a -> s {rollbackConfiguration = a} :: CreateCloudFormationChangeSet)

-- | This property corresponds to the parameter of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
-- API.
createCloudFormationChangeSet_description :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe Core.Text)
createCloudFormationChangeSet_description = Lens.lens (\CreateCloudFormationChangeSet' {description} -> description) (\s@CreateCloudFormationChangeSet' {} a -> s {description = a} :: CreateCloudFormationChangeSet)

-- | This property corresponds to the parameter of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
-- API.
createCloudFormationChangeSet_changeSetName :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe Core.Text)
createCloudFormationChangeSet_changeSetName = Lens.lens (\CreateCloudFormationChangeSet' {changeSetName} -> changeSetName) (\s@CreateCloudFormationChangeSet' {} a -> s {changeSetName = a} :: CreateCloudFormationChangeSet)

-- | The semantic version of the application:
--
-- <https://semver.org/>
createCloudFormationChangeSet_semanticVersion :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe Core.Text)
createCloudFormationChangeSet_semanticVersion = Lens.lens (\CreateCloudFormationChangeSet' {semanticVersion} -> semanticVersion) (\s@CreateCloudFormationChangeSet' {} a -> s {semanticVersion = a} :: CreateCloudFormationChangeSet)

-- | The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern:
-- [0-9a-fA-F]{8}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{4}\\-[0-9a-fA-F]{12}
createCloudFormationChangeSet_templateId :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe Core.Text)
createCloudFormationChangeSet_templateId = Lens.lens (\CreateCloudFormationChangeSet' {templateId} -> templateId) (\s@CreateCloudFormationChangeSet' {} a -> s {templateId = a} :: CreateCloudFormationChangeSet)

-- | This property corresponds to the parameter of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
-- API.
createCloudFormationChangeSet_clientToken :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe Core.Text)
createCloudFormationChangeSet_clientToken = Lens.lens (\CreateCloudFormationChangeSet' {clientToken} -> clientToken) (\s@CreateCloudFormationChangeSet' {} a -> s {clientToken = a} :: CreateCloudFormationChangeSet)

-- | The Amazon Resource Name (ARN) of the application.
createCloudFormationChangeSet_applicationId :: Lens.Lens' CreateCloudFormationChangeSet Core.Text
createCloudFormationChangeSet_applicationId = Lens.lens (\CreateCloudFormationChangeSet' {applicationId} -> applicationId) (\s@CreateCloudFormationChangeSet' {} a -> s {applicationId = a} :: CreateCloudFormationChangeSet)

-- | This property corresponds to the parameter of the same name for the /AWS
-- CloudFormation
-- <https://docs.aws.amazon.com/goto/WebAPI/cloudformation-2010-05-15/CreateChangeSet CreateChangeSet>/
-- API.
createCloudFormationChangeSet_stackName :: Lens.Lens' CreateCloudFormationChangeSet Core.Text
createCloudFormationChangeSet_stackName = Lens.lens (\CreateCloudFormationChangeSet' {stackName} -> stackName) (\s@CreateCloudFormationChangeSet' {} a -> s {stackName = a} :: CreateCloudFormationChangeSet)

instance
  Core.AWSRequest
    CreateCloudFormationChangeSet
  where
  type
    AWSResponse CreateCloudFormationChangeSet =
      CreateCloudFormationChangeSetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCloudFormationChangeSetResponse'
            Core.<$> (x Core..?> "applicationId")
            Core.<*> (x Core..?> "stackId")
            Core.<*> (x Core..?> "changeSetId")
            Core.<*> (x Core..?> "semanticVersion")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateCloudFormationChangeSet

instance Core.NFData CreateCloudFormationChangeSet

instance Core.ToHeaders CreateCloudFormationChangeSet where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateCloudFormationChangeSet where
  toJSON CreateCloudFormationChangeSet' {..} =
    Core.object
      ( Core.catMaybes
          [ ("resourceTypes" Core..=) Core.<$> resourceTypes,
            ("capabilities" Core..=) Core.<$> capabilities,
            ("parameterOverrides" Core..=)
              Core.<$> parameterOverrides,
            ("notificationArns" Core..=)
              Core.<$> notificationArns,
            ("tags" Core..=) Core.<$> tags,
            ("rollbackConfiguration" Core..=)
              Core.<$> rollbackConfiguration,
            ("description" Core..=) Core.<$> description,
            ("changeSetName" Core..=) Core.<$> changeSetName,
            ("semanticVersion" Core..=) Core.<$> semanticVersion,
            ("templateId" Core..=) Core.<$> templateId,
            ("clientToken" Core..=) Core.<$> clientToken,
            Core.Just ("stackName" Core..= stackName)
          ]
      )

instance Core.ToPath CreateCloudFormationChangeSet where
  toPath CreateCloudFormationChangeSet' {..} =
    Core.mconcat
      [ "/applications/",
        Core.toBS applicationId,
        "/changesets"
      ]

instance Core.ToQuery CreateCloudFormationChangeSet where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateCloudFormationChangeSetResponse' smart constructor.
data CreateCloudFormationChangeSetResponse = CreateCloudFormationChangeSetResponse'
  { -- | The application Amazon Resource Name (ARN).
    applicationId :: Core.Maybe Core.Text,
    -- | The unique ID of the stack.
    stackId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the change set.
    --
    -- Length constraints: Minimum length of 1.
    --
    -- Pattern: ARN:[-a-zA-Z0-9:\/]*
    changeSetId :: Core.Maybe Core.Text,
    -- | The semantic version of the application:
    --
    -- <https://semver.org/>
    semanticVersion :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateCloudFormationChangeSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'createCloudFormationChangeSetResponse_applicationId' - The application Amazon Resource Name (ARN).
--
-- 'stackId', 'createCloudFormationChangeSetResponse_stackId' - The unique ID of the stack.
--
-- 'changeSetId', 'createCloudFormationChangeSetResponse_changeSetId' - The Amazon Resource Name (ARN) of the change set.
--
-- Length constraints: Minimum length of 1.
--
-- Pattern: ARN:[-a-zA-Z0-9:\/]*
--
-- 'semanticVersion', 'createCloudFormationChangeSetResponse_semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/>
--
-- 'httpStatus', 'createCloudFormationChangeSetResponse_httpStatus' - The response's http status code.
newCreateCloudFormationChangeSetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateCloudFormationChangeSetResponse
newCreateCloudFormationChangeSetResponse pHttpStatus_ =
  CreateCloudFormationChangeSetResponse'
    { applicationId =
        Core.Nothing,
      stackId = Core.Nothing,
      changeSetId = Core.Nothing,
      semanticVersion = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The application Amazon Resource Name (ARN).
createCloudFormationChangeSetResponse_applicationId :: Lens.Lens' CreateCloudFormationChangeSetResponse (Core.Maybe Core.Text)
createCloudFormationChangeSetResponse_applicationId = Lens.lens (\CreateCloudFormationChangeSetResponse' {applicationId} -> applicationId) (\s@CreateCloudFormationChangeSetResponse' {} a -> s {applicationId = a} :: CreateCloudFormationChangeSetResponse)

-- | The unique ID of the stack.
createCloudFormationChangeSetResponse_stackId :: Lens.Lens' CreateCloudFormationChangeSetResponse (Core.Maybe Core.Text)
createCloudFormationChangeSetResponse_stackId = Lens.lens (\CreateCloudFormationChangeSetResponse' {stackId} -> stackId) (\s@CreateCloudFormationChangeSetResponse' {} a -> s {stackId = a} :: CreateCloudFormationChangeSetResponse)

-- | The Amazon Resource Name (ARN) of the change set.
--
-- Length constraints: Minimum length of 1.
--
-- Pattern: ARN:[-a-zA-Z0-9:\/]*
createCloudFormationChangeSetResponse_changeSetId :: Lens.Lens' CreateCloudFormationChangeSetResponse (Core.Maybe Core.Text)
createCloudFormationChangeSetResponse_changeSetId = Lens.lens (\CreateCloudFormationChangeSetResponse' {changeSetId} -> changeSetId) (\s@CreateCloudFormationChangeSetResponse' {} a -> s {changeSetId = a} :: CreateCloudFormationChangeSetResponse)

-- | The semantic version of the application:
--
-- <https://semver.org/>
createCloudFormationChangeSetResponse_semanticVersion :: Lens.Lens' CreateCloudFormationChangeSetResponse (Core.Maybe Core.Text)
createCloudFormationChangeSetResponse_semanticVersion = Lens.lens (\CreateCloudFormationChangeSetResponse' {semanticVersion} -> semanticVersion) (\s@CreateCloudFormationChangeSetResponse' {} a -> s {semanticVersion = a} :: CreateCloudFormationChangeSetResponse)

-- | The response's http status code.
createCloudFormationChangeSetResponse_httpStatus :: Lens.Lens' CreateCloudFormationChangeSetResponse Core.Int
createCloudFormationChangeSetResponse_httpStatus = Lens.lens (\CreateCloudFormationChangeSetResponse' {httpStatus} -> httpStatus) (\s@CreateCloudFormationChangeSetResponse' {} a -> s {httpStatus = a} :: CreateCloudFormationChangeSetResponse)

instance
  Core.NFData
    CreateCloudFormationChangeSetResponse
