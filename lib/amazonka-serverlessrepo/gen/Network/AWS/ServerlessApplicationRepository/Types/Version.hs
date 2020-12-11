-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.Version
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServerlessApplicationRepository.Types.Version
  ( Version (..),

    -- * Smart constructor
    mkVersion,

    -- * Lenses
    vSourceCodeURL,
    vSourceCodeArchiveURL,
    vTemplateURL,
    vParameterDefinitions,
    vResourcesSupported,
    vCreationTime,
    vRequiredCapabilities,
    vApplicationId,
    vSemanticVersion,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.ServerlessApplicationRepository.Types.Capability
import Network.AWS.ServerlessApplicationRepository.Types.ParameterDefinition

-- | Application version details.
--
-- /See:/ 'mkVersion' smart constructor.
data Version = Version'
  { sourceCodeURL :: Lude.Maybe Lude.Text,
    sourceCodeArchiveURL :: Lude.Maybe Lude.Text,
    templateURL :: Lude.Text,
    parameterDefinitions :: [ParameterDefinition],
    resourcesSupported :: Lude.Bool,
    creationTime :: Lude.Text,
    requiredCapabilities :: [Capability],
    applicationId :: Lude.Text,
    semanticVersion :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Version' with the minimum fields required to make a request.
--
-- * 'applicationId' - The application Amazon Resource Name (ARN).
-- * 'creationTime' - The date and time this resource was created.
-- * 'parameterDefinitions' - An array of parameter types supported by the application.
-- * 'requiredCapabilities' - A list of values that you must specify before you can deploy certain applications.
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
-- * 'resourcesSupported' - Whether all of the AWS resources contained in this application are supported in the region
--
--  in which it is being retrieved.
-- * 'semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
-- * 'sourceCodeArchiveURL' - A link to the S3 object that contains the ZIP archive of the source code for this version of your application.
--
-- Maximum size 50 MB
-- * 'sourceCodeURL' - A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
-- * 'templateURL' - A link to the packaged AWS SAM template of your application.
mkVersion ::
  -- | 'templateURL'
  Lude.Text ->
  -- | 'resourcesSupported'
  Lude.Bool ->
  -- | 'creationTime'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  -- | 'semanticVersion'
  Lude.Text ->
  Version
mkVersion
  pTemplateURL_
  pResourcesSupported_
  pCreationTime_
  pApplicationId_
  pSemanticVersion_ =
    Version'
      { sourceCodeURL = Lude.Nothing,
        sourceCodeArchiveURL = Lude.Nothing,
        templateURL = pTemplateURL_,
        parameterDefinitions = Lude.mempty,
        resourcesSupported = pResourcesSupported_,
        creationTime = pCreationTime_,
        requiredCapabilities = Lude.mempty,
        applicationId = pApplicationId_,
        semanticVersion = pSemanticVersion_
      }

-- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
--
-- /Note:/ Consider using 'sourceCodeURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vSourceCodeURL :: Lens.Lens' Version (Lude.Maybe Lude.Text)
vSourceCodeURL = Lens.lens (sourceCodeURL :: Version -> Lude.Maybe Lude.Text) (\s a -> s {sourceCodeURL = a} :: Version)
{-# DEPRECATED vSourceCodeURL "Use generic-lens or generic-optics with 'sourceCodeURL' instead." #-}

-- | A link to the S3 object that contains the ZIP archive of the source code for this version of your application.
--
-- Maximum size 50 MB
--
-- /Note:/ Consider using 'sourceCodeArchiveURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vSourceCodeArchiveURL :: Lens.Lens' Version (Lude.Maybe Lude.Text)
vSourceCodeArchiveURL = Lens.lens (sourceCodeArchiveURL :: Version -> Lude.Maybe Lude.Text) (\s a -> s {sourceCodeArchiveURL = a} :: Version)
{-# DEPRECATED vSourceCodeArchiveURL "Use generic-lens or generic-optics with 'sourceCodeArchiveURL' instead." #-}

-- | A link to the packaged AWS SAM template of your application.
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vTemplateURL :: Lens.Lens' Version Lude.Text
vTemplateURL = Lens.lens (templateURL :: Version -> Lude.Text) (\s a -> s {templateURL = a} :: Version)
{-# DEPRECATED vTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

-- | An array of parameter types supported by the application.
--
-- /Note:/ Consider using 'parameterDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vParameterDefinitions :: Lens.Lens' Version [ParameterDefinition]
vParameterDefinitions = Lens.lens (parameterDefinitions :: Version -> [ParameterDefinition]) (\s a -> s {parameterDefinitions = a} :: Version)
{-# DEPRECATED vParameterDefinitions "Use generic-lens or generic-optics with 'parameterDefinitions' instead." #-}

-- | Whether all of the AWS resources contained in this application are supported in the region
--
--  in which it is being retrieved.
--
-- /Note:/ Consider using 'resourcesSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vResourcesSupported :: Lens.Lens' Version Lude.Bool
vResourcesSupported = Lens.lens (resourcesSupported :: Version -> Lude.Bool) (\s a -> s {resourcesSupported = a} :: Version)
{-# DEPRECATED vResourcesSupported "Use generic-lens or generic-optics with 'resourcesSupported' instead." #-}

-- | The date and time this resource was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vCreationTime :: Lens.Lens' Version Lude.Text
vCreationTime = Lens.lens (creationTime :: Version -> Lude.Text) (\s a -> s {creationTime = a} :: Version)
{-# DEPRECATED vCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

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
vRequiredCapabilities :: Lens.Lens' Version [Capability]
vRequiredCapabilities = Lens.lens (requiredCapabilities :: Version -> [Capability]) (\s a -> s {requiredCapabilities = a} :: Version)
{-# DEPRECATED vRequiredCapabilities "Use generic-lens or generic-optics with 'requiredCapabilities' instead." #-}

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vApplicationId :: Lens.Lens' Version Lude.Text
vApplicationId = Lens.lens (applicationId :: Version -> Lude.Text) (\s a -> s {applicationId = a} :: Version)
{-# DEPRECATED vApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vSemanticVersion :: Lens.Lens' Version Lude.Text
vSemanticVersion = Lens.lens (semanticVersion :: Version -> Lude.Text) (\s a -> s {semanticVersion = a} :: Version)
{-# DEPRECATED vSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

instance Lude.FromJSON Version where
  parseJSON =
    Lude.withObject
      "Version"
      ( \x ->
          Version'
            Lude.<$> (x Lude..:? "sourceCodeUrl")
            Lude.<*> (x Lude..:? "sourceCodeArchiveUrl")
            Lude.<*> (x Lude..: "templateUrl")
            Lude.<*> (x Lude..:? "parameterDefinitions" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "resourcesSupported")
            Lude.<*> (x Lude..: "creationTime")
            Lude.<*> (x Lude..:? "requiredCapabilities" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "applicationId")
            Lude.<*> (x Lude..: "semanticVersion")
      )
