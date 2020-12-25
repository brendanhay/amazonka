{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.CreateCloudFormationChangeSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AWS CloudFormation change set for the given application.
module Network.AWS.ServerlessApplicationRepository.CreateCloudFormationChangeSet
  ( -- * Creating a request
    CreateCloudFormationChangeSet (..),
    mkCreateCloudFormationChangeSet,

    -- ** Request lenses
    ccfcsApplicationId,
    ccfcsStackName,
    ccfcsCapabilities,
    ccfcsChangeSetName,
    ccfcsClientToken,
    ccfcsDescription,
    ccfcsNotificationArns,
    ccfcsParameterOverrides,
    ccfcsResourceTypes,
    ccfcsRollbackConfiguration,
    ccfcsSemanticVersion,
    ccfcsTags,
    ccfcsTemplateId,

    -- * Destructuring the response
    CreateCloudFormationChangeSetResponse (..),
    mkCreateCloudFormationChangeSetResponse,

    -- ** Response lenses
    ccfcsrrsApplicationId,
    ccfcsrrsChangeSetId,
    ccfcsrrsSemanticVersion,
    ccfcsrrsStackId,
    ccfcsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServerlessApplicationRepository.Types as Types

-- | /See:/ 'mkCreateCloudFormationChangeSet' smart constructor.
data CreateCloudFormationChangeSet = CreateCloudFormationChangeSet'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Core.Text,
    -- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
    stackName :: Core.Text,
    -- | A list of values that you must specify before you can deploy certain applications.
    --
    --  Some applications might include resources that can affect permissions in your AWS
    --  account, for example, by creating new AWS Identity and Access Management (IAM) users.
    --  For those applications, you must explicitly acknowledge their capabilities by
    --  specifying this parameter.
    -- The only valid values are CAPABILITY_IAM, CAPABILITY_NAMED_IAM,
    --  CAPABILITY_RESOURCE_POLICY, and CAPABILITY_AUTO_EXPAND.
    -- The following resources require you to specify CAPABILITY_IAM or
    --  CAPABILITY_NAMED_IAM:
    --  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group> ,
    --  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile> ,
    --  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM::Policy> , and
    --  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role> .
    --  If the application contains IAM resources, you can specify either CAPABILITY_IAM
    --  or CAPABILITY_NAMED_IAM. If the application contains IAM resources
    --  with custom names, you must specify CAPABILITY_NAMED_IAM.
    -- The following resources require you to specify CAPABILITY_RESOURCE_POLICY:
    --  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-lambda-permission.html AWS::Lambda::Permission> ,
    --  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM:Policy> ,
    --  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-applicationautoscaling-scalingpolicy.html AWS::ApplicationAutoScaling::ScalingPolicy> ,
    --  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-policy.html AWS::S3::BucketPolicy> ,
    --  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sqs-policy.html AWS::SQS::QueuePolicy> , and
    --  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sns-policy.html AWS::SNS:TopicPolicy> .
    -- Applications that contain one or more nested applications require you to specify
    --  CAPABILITY_AUTO_EXPAND.
    -- If your application template contains any of the above resources, we recommend that you review
    --  all permissions associated with the application before deploying. If you don't specify
    --  this parameter for an application that requires capabilities, the call will fail.
    capabilities :: Core.Maybe [Core.Text],
    -- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
    changeSetName :: Core.Maybe Core.Text,
    -- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
    clientToken :: Core.Maybe Core.Text,
    -- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
    description :: Core.Maybe Core.Text,
    -- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
    notificationArns :: Core.Maybe [Core.Text],
    -- | A list of parameter values for the parameters of the application.
    parameterOverrides :: Core.Maybe [Types.ParameterValue],
    -- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
    resourceTypes :: Core.Maybe [Core.Text],
    -- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
    rollbackConfiguration :: Core.Maybe Types.RollbackConfiguration,
    -- | The semantic version of the application:
    --
    -- <https://semver.org/ https://semver.org/>
    semanticVersion :: Core.Maybe Core.Text,
    -- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
    tags :: Core.Maybe [Types.Tag],
    -- | The UUID returned by CreateCloudFormationTemplate.
    --
    -- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
    templateId :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCloudFormationChangeSet' value with any optional fields omitted.
mkCreateCloudFormationChangeSet ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'stackName'
  Core.Text ->
  CreateCloudFormationChangeSet
mkCreateCloudFormationChangeSet applicationId stackName =
  CreateCloudFormationChangeSet'
    { applicationId,
      stackName,
      capabilities = Core.Nothing,
      changeSetName = Core.Nothing,
      clientToken = Core.Nothing,
      description = Core.Nothing,
      notificationArns = Core.Nothing,
      parameterOverrides = Core.Nothing,
      resourceTypes = Core.Nothing,
      rollbackConfiguration = Core.Nothing,
      semanticVersion = Core.Nothing,
      tags = Core.Nothing,
      templateId = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsApplicationId :: Lens.Lens' CreateCloudFormationChangeSet Core.Text
ccfcsApplicationId = Lens.field @"applicationId"
{-# DEPRECATED ccfcsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsStackName :: Lens.Lens' CreateCloudFormationChangeSet Core.Text
ccfcsStackName = Lens.field @"stackName"
{-# DEPRECATED ccfcsStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

-- | A list of values that you must specify before you can deploy certain applications.
--
--  Some applications might include resources that can affect permissions in your AWS
--  account, for example, by creating new AWS Identity and Access Management (IAM) users.
--  For those applications, you must explicitly acknowledge their capabilities by
--  specifying this parameter.
-- The only valid values are CAPABILITY_IAM, CAPABILITY_NAMED_IAM,
--  CAPABILITY_RESOURCE_POLICY, and CAPABILITY_AUTO_EXPAND.
-- The following resources require you to specify CAPABILITY_IAM or
--  CAPABILITY_NAMED_IAM:
--  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-iam-group.html AWS::IAM::Group> ,
--  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-instanceprofile.html AWS::IAM::InstanceProfile> ,
--  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM::Policy> , and
--  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-role.html AWS::IAM::Role> .
--  If the application contains IAM resources, you can specify either CAPABILITY_IAM
--  or CAPABILITY_NAMED_IAM. If the application contains IAM resources
--  with custom names, you must specify CAPABILITY_NAMED_IAM.
-- The following resources require you to specify CAPABILITY_RESOURCE_POLICY:
--  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-lambda-permission.html AWS::Lambda::Permission> ,
--  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-iam-policy.html AWS::IAM:Policy> ,
--  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-resource-applicationautoscaling-scalingpolicy.html AWS::ApplicationAutoScaling::ScalingPolicy> ,
--  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-s3-policy.html AWS::S3::BucketPolicy> ,
--  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sqs-policy.html AWS::SQS::QueuePolicy> , and
--  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sns-policy.html AWS::SNS:TopicPolicy> .
-- Applications that contain one or more nested applications require you to specify
--  CAPABILITY_AUTO_EXPAND.
-- If your application template contains any of the above resources, we recommend that you review
--  all permissions associated with the application before deploying. If you don't specify
--  this parameter for an application that requires capabilities, the call will fail.
--
-- /Note:/ Consider using 'capabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsCapabilities :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe [Core.Text])
ccfcsCapabilities = Lens.field @"capabilities"
{-# DEPRECATED ccfcsCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsChangeSetName :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe Core.Text)
ccfcsChangeSetName = Lens.field @"changeSetName"
{-# DEPRECATED ccfcsChangeSetName "Use generic-lens or generic-optics with 'changeSetName' instead." #-}

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsClientToken :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe Core.Text)
ccfcsClientToken = Lens.field @"clientToken"
{-# DEPRECATED ccfcsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsDescription :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe Core.Text)
ccfcsDescription = Lens.field @"description"
{-# DEPRECATED ccfcsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- /Note:/ Consider using 'notificationArns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsNotificationArns :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe [Core.Text])
ccfcsNotificationArns = Lens.field @"notificationArns"
{-# DEPRECATED ccfcsNotificationArns "Use generic-lens or generic-optics with 'notificationArns' instead." #-}

-- | A list of parameter values for the parameters of the application.
--
-- /Note:/ Consider using 'parameterOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsParameterOverrides :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe [Types.ParameterValue])
ccfcsParameterOverrides = Lens.field @"parameterOverrides"
{-# DEPRECATED ccfcsParameterOverrides "Use generic-lens or generic-optics with 'parameterOverrides' instead." #-}

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- /Note:/ Consider using 'resourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsResourceTypes :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe [Core.Text])
ccfcsResourceTypes = Lens.field @"resourceTypes"
{-# DEPRECATED ccfcsResourceTypes "Use generic-lens or generic-optics with 'resourceTypes' instead." #-}

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- /Note:/ Consider using 'rollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsRollbackConfiguration :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe Types.RollbackConfiguration)
ccfcsRollbackConfiguration = Lens.field @"rollbackConfiguration"
{-# DEPRECATED ccfcsRollbackConfiguration "Use generic-lens or generic-optics with 'rollbackConfiguration' instead." #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsSemanticVersion :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe Core.Text)
ccfcsSemanticVersion = Lens.field @"semanticVersion"
{-# DEPRECATED ccfcsSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsTags :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe [Types.Tag])
ccfcsTags = Lens.field @"tags"
{-# DEPRECATED ccfcsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
--
-- /Note:/ Consider using 'templateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsTemplateId :: Lens.Lens' CreateCloudFormationChangeSet (Core.Maybe Core.Text)
ccfcsTemplateId = Lens.field @"templateId"
{-# DEPRECATED ccfcsTemplateId "Use generic-lens or generic-optics with 'templateId' instead." #-}

instance Core.FromJSON CreateCloudFormationChangeSet where
  toJSON CreateCloudFormationChangeSet {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("stackName" Core..= stackName),
            ("capabilities" Core..=) Core.<$> capabilities,
            ("changeSetName" Core..=) Core.<$> changeSetName,
            ("clientToken" Core..=) Core.<$> clientToken,
            ("description" Core..=) Core.<$> description,
            ("notificationArns" Core..=) Core.<$> notificationArns,
            ("parameterOverrides" Core..=) Core.<$> parameterOverrides,
            ("resourceTypes" Core..=) Core.<$> resourceTypes,
            ("rollbackConfiguration" Core..=) Core.<$> rollbackConfiguration,
            ("semanticVersion" Core..=) Core.<$> semanticVersion,
            ("tags" Core..=) Core.<$> tags,
            ("templateId" Core..=) Core.<$> templateId
          ]
      )

instance Core.AWSRequest CreateCloudFormationChangeSet where
  type
    Rs CreateCloudFormationChangeSet =
      CreateCloudFormationChangeSetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath =
          Core.rawPath
            ( "/applications/" Core.<> (Core.toText applicationId)
                Core.<> ("/changesets")
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCloudFormationChangeSetResponse'
            Core.<$> (x Core..:? "applicationId")
            Core.<*> (x Core..:? "changeSetId")
            Core.<*> (x Core..:? "semanticVersion")
            Core.<*> (x Core..:? "stackId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateCloudFormationChangeSetResponse' smart constructor.
data CreateCloudFormationChangeSetResponse = CreateCloudFormationChangeSetResponse'
  { -- | The application Amazon Resource Name (ARN).
    applicationId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the change set.
    --
    -- Length constraints: Minimum length of 1.
    -- Pattern: ARN:[-a-zA-Z0-9:/]*
    changeSetId :: Core.Maybe Core.Text,
    -- | The semantic version of the application:
    --
    -- <https://semver.org/ https://semver.org/>
    semanticVersion :: Core.Maybe Core.Text,
    -- | The unique ID of the stack.
    stackId :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateCloudFormationChangeSetResponse' value with any optional fields omitted.
mkCreateCloudFormationChangeSetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateCloudFormationChangeSetResponse
mkCreateCloudFormationChangeSetResponse responseStatus =
  CreateCloudFormationChangeSetResponse'
    { applicationId =
        Core.Nothing,
      changeSetId = Core.Nothing,
      semanticVersion = Core.Nothing,
      stackId = Core.Nothing,
      responseStatus
    }

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsrrsApplicationId :: Lens.Lens' CreateCloudFormationChangeSetResponse (Core.Maybe Core.Text)
ccfcsrrsApplicationId = Lens.field @"applicationId"
{-# DEPRECATED ccfcsrrsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The Amazon Resource Name (ARN) of the change set.
--
-- Length constraints: Minimum length of 1.
-- Pattern: ARN:[-a-zA-Z0-9:/]*
--
-- /Note:/ Consider using 'changeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsrrsChangeSetId :: Lens.Lens' CreateCloudFormationChangeSetResponse (Core.Maybe Core.Text)
ccfcsrrsChangeSetId = Lens.field @"changeSetId"
{-# DEPRECATED ccfcsrrsChangeSetId "Use generic-lens or generic-optics with 'changeSetId' instead." #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsrrsSemanticVersion :: Lens.Lens' CreateCloudFormationChangeSetResponse (Core.Maybe Core.Text)
ccfcsrrsSemanticVersion = Lens.field @"semanticVersion"
{-# DEPRECATED ccfcsrrsSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

-- | The unique ID of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsrrsStackId :: Lens.Lens' CreateCloudFormationChangeSetResponse (Core.Maybe Core.Text)
ccfcsrrsStackId = Lens.field @"stackId"
{-# DEPRECATED ccfcsrrsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsrrsResponseStatus :: Lens.Lens' CreateCloudFormationChangeSetResponse Core.Int
ccfcsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ccfcsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
