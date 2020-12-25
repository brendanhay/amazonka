{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.CreateApplicationVersion
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application version.
module Network.AWS.ServerlessApplicationRepository.CreateApplicationVersion
  ( -- * Creating a request
    CreateApplicationVersion (..),
    mkCreateApplicationVersion,

    -- ** Request lenses
    cavApplicationId,
    cavSemanticVersion,
    cavSourceCodeArchiveUrl,
    cavSourceCodeUrl,
    cavTemplateBody,
    cavTemplateUrl,

    -- * Destructuring the response
    CreateApplicationVersionResponse (..),
    mkCreateApplicationVersionResponse,

    -- ** Response lenses
    cavrrsApplicationId,
    cavrrsCreationTime,
    cavrrsParameterDefinitions,
    cavrrsRequiredCapabilities,
    cavrrsResourcesSupported,
    cavrrsSemanticVersion,
    cavrrsSourceCodeArchiveUrl,
    cavrrsSourceCodeUrl,
    cavrrsTemplateUrl,
    cavrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServerlessApplicationRepository.Types as Types

-- | /See:/ 'mkCreateApplicationVersion' smart constructor.
data CreateApplicationVersion = CreateApplicationVersion'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Core.Text,
    -- | The semantic version of the new version.
    semanticVersion :: Core.Text,
    -- | A link to the S3 object that contains the ZIP archive of the source code for this version of your application.
    --
    -- Maximum size 50 MB
    sourceCodeArchiveUrl :: Core.Maybe Core.Text,
    -- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
    sourceCodeUrl :: Core.Maybe Core.Text,
    -- | The raw packaged AWS SAM template of your application.
    templateBody :: Core.Maybe Core.Text,
    -- | A link to the packaged AWS SAM template of your application.
    templateUrl :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApplicationVersion' value with any optional fields omitted.
mkCreateApplicationVersion ::
  -- | 'applicationId'
  Core.Text ->
  -- | 'semanticVersion'
  Core.Text ->
  CreateApplicationVersion
mkCreateApplicationVersion applicationId semanticVersion =
  CreateApplicationVersion'
    { applicationId,
      semanticVersion,
      sourceCodeArchiveUrl = Core.Nothing,
      sourceCodeUrl = Core.Nothing,
      templateBody = Core.Nothing,
      templateUrl = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavApplicationId :: Lens.Lens' CreateApplicationVersion Core.Text
cavApplicationId = Lens.field @"applicationId"
{-# DEPRECATED cavApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The semantic version of the new version.
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavSemanticVersion :: Lens.Lens' CreateApplicationVersion Core.Text
cavSemanticVersion = Lens.field @"semanticVersion"
{-# DEPRECATED cavSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

-- | A link to the S3 object that contains the ZIP archive of the source code for this version of your application.
--
-- Maximum size 50 MB
--
-- /Note:/ Consider using 'sourceCodeArchiveUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavSourceCodeArchiveUrl :: Lens.Lens' CreateApplicationVersion (Core.Maybe Core.Text)
cavSourceCodeArchiveUrl = Lens.field @"sourceCodeArchiveUrl"
{-# DEPRECATED cavSourceCodeArchiveUrl "Use generic-lens or generic-optics with 'sourceCodeArchiveUrl' instead." #-}

-- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
--
-- /Note:/ Consider using 'sourceCodeUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavSourceCodeUrl :: Lens.Lens' CreateApplicationVersion (Core.Maybe Core.Text)
cavSourceCodeUrl = Lens.field @"sourceCodeUrl"
{-# DEPRECATED cavSourceCodeUrl "Use generic-lens or generic-optics with 'sourceCodeUrl' instead." #-}

-- | The raw packaged AWS SAM template of your application.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavTemplateBody :: Lens.Lens' CreateApplicationVersion (Core.Maybe Core.Text)
cavTemplateBody = Lens.field @"templateBody"
{-# DEPRECATED cavTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | A link to the packaged AWS SAM template of your application.
--
-- /Note:/ Consider using 'templateUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavTemplateUrl :: Lens.Lens' CreateApplicationVersion (Core.Maybe Core.Text)
cavTemplateUrl = Lens.field @"templateUrl"
{-# DEPRECATED cavTemplateUrl "Use generic-lens or generic-optics with 'templateUrl' instead." #-}

instance Core.FromJSON CreateApplicationVersion where
  toJSON CreateApplicationVersion {..} =
    Core.object
      ( Core.catMaybes
          [ ("sourceCodeArchiveUrl" Core..=) Core.<$> sourceCodeArchiveUrl,
            ("sourceCodeUrl" Core..=) Core.<$> sourceCodeUrl,
            ("templateBody" Core..=) Core.<$> templateBody,
            ("templateUrl" Core..=) Core.<$> templateUrl
          ]
      )

instance Core.AWSRequest CreateApplicationVersion where
  type Rs CreateApplicationVersion = CreateApplicationVersionResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PUT,
        Core._rqPath =
          Core.rawPath
            ( "/applications/" Core.<> (Core.toText applicationId)
                Core.<> ("/versions/")
                Core.<> (Core.toText semanticVersion)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApplicationVersionResponse'
            Core.<$> (x Core..:? "applicationId")
            Core.<*> (x Core..:? "creationTime")
            Core.<*> (x Core..:? "parameterDefinitions")
            Core.<*> (x Core..:? "requiredCapabilities")
            Core.<*> (x Core..:? "resourcesSupported")
            Core.<*> (x Core..:? "semanticVersion")
            Core.<*> (x Core..:? "sourceCodeArchiveUrl")
            Core.<*> (x Core..:? "sourceCodeUrl")
            Core.<*> (x Core..:? "templateUrl")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateApplicationVersionResponse' smart constructor.
data CreateApplicationVersionResponse = CreateApplicationVersionResponse'
  { -- | The application Amazon Resource Name (ARN).
    applicationId :: Core.Maybe Core.Text,
    -- | The date and time this resource was created.
    creationTime :: Core.Maybe Core.Text,
    -- | An array of parameter types supported by the application.
    parameterDefinitions :: Core.Maybe [Types.ParameterDefinition],
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
    --  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sns-policy.html AWS::SNS::TopicPolicy> .
    -- Applications that contain one or more nested applications require you to specify
    --  CAPABILITY_AUTO_EXPAND.
    -- If your application template contains any of the above resources, we recommend that you review
    --  all permissions associated with the application before deploying. If you don't specify
    --  this parameter for an application that requires capabilities, the call will fail.
    requiredCapabilities :: Core.Maybe [Types.Capability],
    -- | Whether all of the AWS resources contained in this application are supported in the region
    --
    --  in which it is being retrieved.
    resourcesSupported :: Core.Maybe Core.Bool,
    -- | The semantic version of the application:
    --
    -- <https://semver.org/ https://semver.org/>
    semanticVersion :: Core.Maybe Core.Text,
    -- | A link to the S3 object that contains the ZIP archive of the source code for this version of your application.
    --
    -- Maximum size 50 MB
    sourceCodeArchiveUrl :: Core.Maybe Core.Text,
    -- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
    sourceCodeUrl :: Core.Maybe Core.Text,
    -- | A link to the packaged AWS SAM template of your application.
    templateUrl :: Core.Maybe Core.Text,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApplicationVersionResponse' value with any optional fields omitted.
mkCreateApplicationVersionResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateApplicationVersionResponse
mkCreateApplicationVersionResponse responseStatus =
  CreateApplicationVersionResponse'
    { applicationId = Core.Nothing,
      creationTime = Core.Nothing,
      parameterDefinitions = Core.Nothing,
      requiredCapabilities = Core.Nothing,
      resourcesSupported = Core.Nothing,
      semanticVersion = Core.Nothing,
      sourceCodeArchiveUrl = Core.Nothing,
      sourceCodeUrl = Core.Nothing,
      templateUrl = Core.Nothing,
      responseStatus
    }

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrrsApplicationId :: Lens.Lens' CreateApplicationVersionResponse (Core.Maybe Core.Text)
cavrrsApplicationId = Lens.field @"applicationId"
{-# DEPRECATED cavrrsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The date and time this resource was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrrsCreationTime :: Lens.Lens' CreateApplicationVersionResponse (Core.Maybe Core.Text)
cavrrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED cavrrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | An array of parameter types supported by the application.
--
-- /Note:/ Consider using 'parameterDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrrsParameterDefinitions :: Lens.Lens' CreateApplicationVersionResponse (Core.Maybe [Types.ParameterDefinition])
cavrrsParameterDefinitions = Lens.field @"parameterDefinitions"
{-# DEPRECATED cavrrsParameterDefinitions "Use generic-lens or generic-optics with 'parameterDefinitions' instead." #-}

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
--  <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/aws-properties-sns-policy.html AWS::SNS::TopicPolicy> .
-- Applications that contain one or more nested applications require you to specify
--  CAPABILITY_AUTO_EXPAND.
-- If your application template contains any of the above resources, we recommend that you review
--  all permissions associated with the application before deploying. If you don't specify
--  this parameter for an application that requires capabilities, the call will fail.
--
-- /Note:/ Consider using 'requiredCapabilities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrrsRequiredCapabilities :: Lens.Lens' CreateApplicationVersionResponse (Core.Maybe [Types.Capability])
cavrrsRequiredCapabilities = Lens.field @"requiredCapabilities"
{-# DEPRECATED cavrrsRequiredCapabilities "Use generic-lens or generic-optics with 'requiredCapabilities' instead." #-}

-- | Whether all of the AWS resources contained in this application are supported in the region
--
--  in which it is being retrieved.
--
-- /Note:/ Consider using 'resourcesSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrrsResourcesSupported :: Lens.Lens' CreateApplicationVersionResponse (Core.Maybe Core.Bool)
cavrrsResourcesSupported = Lens.field @"resourcesSupported"
{-# DEPRECATED cavrrsResourcesSupported "Use generic-lens or generic-optics with 'resourcesSupported' instead." #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrrsSemanticVersion :: Lens.Lens' CreateApplicationVersionResponse (Core.Maybe Core.Text)
cavrrsSemanticVersion = Lens.field @"semanticVersion"
{-# DEPRECATED cavrrsSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

-- | A link to the S3 object that contains the ZIP archive of the source code for this version of your application.
--
-- Maximum size 50 MB
--
-- /Note:/ Consider using 'sourceCodeArchiveUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrrsSourceCodeArchiveUrl :: Lens.Lens' CreateApplicationVersionResponse (Core.Maybe Core.Text)
cavrrsSourceCodeArchiveUrl = Lens.field @"sourceCodeArchiveUrl"
{-# DEPRECATED cavrrsSourceCodeArchiveUrl "Use generic-lens or generic-optics with 'sourceCodeArchiveUrl' instead." #-}

-- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
--
-- /Note:/ Consider using 'sourceCodeUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrrsSourceCodeUrl :: Lens.Lens' CreateApplicationVersionResponse (Core.Maybe Core.Text)
cavrrsSourceCodeUrl = Lens.field @"sourceCodeUrl"
{-# DEPRECATED cavrrsSourceCodeUrl "Use generic-lens or generic-optics with 'sourceCodeUrl' instead." #-}

-- | A link to the packaged AWS SAM template of your application.
--
-- /Note:/ Consider using 'templateUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrrsTemplateUrl :: Lens.Lens' CreateApplicationVersionResponse (Core.Maybe Core.Text)
cavrrsTemplateUrl = Lens.field @"templateUrl"
{-# DEPRECATED cavrrsTemplateUrl "Use generic-lens or generic-optics with 'templateUrl' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrrsResponseStatus :: Lens.Lens' CreateApplicationVersionResponse Core.Int
cavrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cavrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
