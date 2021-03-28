{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.Version
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServerlessApplicationRepository.Types.Version
  ( Version (..)
  -- * Smart constructor
  , mkVersion
  -- * Lenses
  , vTemplateUrl
  , vParameterDefinitions
  , vResourcesSupported
  , vCreationTime
  , vRequiredCapabilities
  , vApplicationId
  , vSemanticVersion
  , vSourceCodeArchiveUrl
  , vSourceCodeUrl
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.ServerlessApplicationRepository.Types.Capability as Types
import qualified Network.AWS.ServerlessApplicationRepository.Types.ParameterDefinition as Types

-- | Application version details.
--
-- /See:/ 'mkVersion' smart constructor.
data Version = Version'
  { templateUrl :: Core.Text
    -- ^ A link to the packaged AWS SAM template of your application.
  , parameterDefinitions :: [Types.ParameterDefinition]
    -- ^ An array of parameter types supported by the application.
  , resourcesSupported :: Core.Bool
    -- ^ Whether all of the AWS resources contained in this application are supported in the region
--
--  in which it is being retrieved.
  , creationTime :: Core.Text
    -- ^ The date and time this resource was created.
  , requiredCapabilities :: [Types.Capability]
    -- ^ A list of values that you must specify before you can deploy certain applications.
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
  , applicationId :: Core.Text
    -- ^ The application Amazon Resource Name (ARN).
  , semanticVersion :: Core.Text
    -- ^ The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/> 
  , sourceCodeArchiveUrl :: Core.Maybe Core.Text
    -- ^ A link to the S3 object that contains the ZIP archive of the source code for this version of your application.
--
-- Maximum size 50 MB
  , sourceCodeUrl :: Core.Maybe Core.Text
    -- ^ A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Version' value with any optional fields omitted.
mkVersion
    :: Core.Text -- ^ 'templateUrl'
    -> Core.Bool -- ^ 'resourcesSupported'
    -> Core.Text -- ^ 'creationTime'
    -> Core.Text -- ^ 'applicationId'
    -> Core.Text -- ^ 'semanticVersion'
    -> Version
mkVersion templateUrl resourcesSupported creationTime applicationId
  semanticVersion
  = Version'{templateUrl, parameterDefinitions = Core.mempty,
             resourcesSupported, creationTime,
             requiredCapabilities = Core.mempty, applicationId, semanticVersion,
             sourceCodeArchiveUrl = Core.Nothing, sourceCodeUrl = Core.Nothing}

-- | A link to the packaged AWS SAM template of your application.
--
-- /Note:/ Consider using 'templateUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vTemplateUrl :: Lens.Lens' Version Core.Text
vTemplateUrl = Lens.field @"templateUrl"
{-# INLINEABLE vTemplateUrl #-}
{-# DEPRECATED templateUrl "Use generic-lens or generic-optics with 'templateUrl' instead"  #-}

-- | An array of parameter types supported by the application.
--
-- /Note:/ Consider using 'parameterDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vParameterDefinitions :: Lens.Lens' Version [Types.ParameterDefinition]
vParameterDefinitions = Lens.field @"parameterDefinitions"
{-# INLINEABLE vParameterDefinitions #-}
{-# DEPRECATED parameterDefinitions "Use generic-lens or generic-optics with 'parameterDefinitions' instead"  #-}

-- | Whether all of the AWS resources contained in this application are supported in the region
--
--  in which it is being retrieved.
--
-- /Note:/ Consider using 'resourcesSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vResourcesSupported :: Lens.Lens' Version Core.Bool
vResourcesSupported = Lens.field @"resourcesSupported"
{-# INLINEABLE vResourcesSupported #-}
{-# DEPRECATED resourcesSupported "Use generic-lens or generic-optics with 'resourcesSupported' instead"  #-}

-- | The date and time this resource was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vCreationTime :: Lens.Lens' Version Core.Text
vCreationTime = Lens.field @"creationTime"
{-# INLINEABLE vCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

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
vRequiredCapabilities :: Lens.Lens' Version [Types.Capability]
vRequiredCapabilities = Lens.field @"requiredCapabilities"
{-# INLINEABLE vRequiredCapabilities #-}
{-# DEPRECATED requiredCapabilities "Use generic-lens or generic-optics with 'requiredCapabilities' instead"  #-}

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vApplicationId :: Lens.Lens' Version Core.Text
vApplicationId = Lens.field @"applicationId"
{-# INLINEABLE vApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/> 
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vSemanticVersion :: Lens.Lens' Version Core.Text
vSemanticVersion = Lens.field @"semanticVersion"
{-# INLINEABLE vSemanticVersion #-}
{-# DEPRECATED semanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead"  #-}

-- | A link to the S3 object that contains the ZIP archive of the source code for this version of your application.
--
-- Maximum size 50 MB
--
-- /Note:/ Consider using 'sourceCodeArchiveUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vSourceCodeArchiveUrl :: Lens.Lens' Version (Core.Maybe Core.Text)
vSourceCodeArchiveUrl = Lens.field @"sourceCodeArchiveUrl"
{-# INLINEABLE vSourceCodeArchiveUrl #-}
{-# DEPRECATED sourceCodeArchiveUrl "Use generic-lens or generic-optics with 'sourceCodeArchiveUrl' instead"  #-}

-- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
--
-- /Note:/ Consider using 'sourceCodeUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vSourceCodeUrl :: Lens.Lens' Version (Core.Maybe Core.Text)
vSourceCodeUrl = Lens.field @"sourceCodeUrl"
{-# INLINEABLE vSourceCodeUrl #-}
{-# DEPRECATED sourceCodeUrl "Use generic-lens or generic-optics with 'sourceCodeUrl' instead"  #-}

instance Core.FromJSON Version where
        parseJSON
          = Core.withObject "Version" Core.$
              \ x ->
                Version' Core.<$>
                  (x Core..: "templateUrl") Core.<*>
                    x Core..:? "parameterDefinitions" Core..!= Core.mempty
                    Core.<*> x Core..: "resourcesSupported"
                    Core.<*> x Core..: "creationTime"
                    Core.<*> x Core..:? "requiredCapabilities" Core..!= Core.mempty
                    Core.<*> x Core..: "applicationId"
                    Core.<*> x Core..: "semanticVersion"
                    Core.<*> x Core..:? "sourceCodeArchiveUrl"
                    Core.<*> x Core..:? "sourceCodeUrl"
