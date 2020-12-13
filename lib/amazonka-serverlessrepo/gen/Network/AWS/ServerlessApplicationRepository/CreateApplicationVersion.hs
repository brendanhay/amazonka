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
    cavSemanticVersion,
    cavSourceCodeURL,
    cavApplicationId,
    cavTemplateBody,
    cavTemplateURL,
    cavSourceCodeArchiveURL,

    -- * Destructuring the response
    CreateApplicationVersionResponse (..),
    mkCreateApplicationVersionResponse,

    -- ** Response lenses
    cavrsCreationTime,
    cavrsResourcesSupported,
    cavrsRequiredCapabilities,
    cavrsParameterDefinitions,
    cavrsSemanticVersion,
    cavrsSourceCodeURL,
    cavrsApplicationId,
    cavrsTemplateURL,
    cavrsSourceCodeArchiveURL,
    cavrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'mkCreateApplicationVersion' smart constructor.
data CreateApplicationVersion = CreateApplicationVersion'
  { -- | The semantic version of the new version.
    semanticVersion :: Lude.Text,
    -- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
    sourceCodeURL :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Lude.Text,
    -- | The raw packaged AWS SAM template of your application.
    templateBody :: Lude.Maybe Lude.Text,
    -- | A link to the packaged AWS SAM template of your application.
    templateURL :: Lude.Maybe Lude.Text,
    -- | A link to the S3 object that contains the ZIP archive of the source code for this version of your application.
    --
    -- Maximum size 50 MB
    sourceCodeArchiveURL :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateApplicationVersion' with the minimum fields required to make a request.
--
-- * 'semanticVersion' - The semantic version of the new version.
-- * 'sourceCodeURL' - A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
-- * 'applicationId' - The Amazon Resource Name (ARN) of the application.
-- * 'templateBody' - The raw packaged AWS SAM template of your application.
-- * 'templateURL' - A link to the packaged AWS SAM template of your application.
-- * 'sourceCodeArchiveURL' - A link to the S3 object that contains the ZIP archive of the source code for this version of your application.
--
-- Maximum size 50 MB
mkCreateApplicationVersion ::
  -- | 'semanticVersion'
  Lude.Text ->
  -- | 'applicationId'
  Lude.Text ->
  CreateApplicationVersion
mkCreateApplicationVersion pSemanticVersion_ pApplicationId_ =
  CreateApplicationVersion'
    { semanticVersion = pSemanticVersion_,
      sourceCodeURL = Lude.Nothing,
      applicationId = pApplicationId_,
      templateBody = Lude.Nothing,
      templateURL = Lude.Nothing,
      sourceCodeArchiveURL = Lude.Nothing
    }

-- | The semantic version of the new version.
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavSemanticVersion :: Lens.Lens' CreateApplicationVersion Lude.Text
cavSemanticVersion = Lens.lens (semanticVersion :: CreateApplicationVersion -> Lude.Text) (\s a -> s {semanticVersion = a} :: CreateApplicationVersion)
{-# DEPRECATED cavSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

-- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
--
-- /Note:/ Consider using 'sourceCodeURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavSourceCodeURL :: Lens.Lens' CreateApplicationVersion (Lude.Maybe Lude.Text)
cavSourceCodeURL = Lens.lens (sourceCodeURL :: CreateApplicationVersion -> Lude.Maybe Lude.Text) (\s a -> s {sourceCodeURL = a} :: CreateApplicationVersion)
{-# DEPRECATED cavSourceCodeURL "Use generic-lens or generic-optics with 'sourceCodeURL' instead." #-}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavApplicationId :: Lens.Lens' CreateApplicationVersion Lude.Text
cavApplicationId = Lens.lens (applicationId :: CreateApplicationVersion -> Lude.Text) (\s a -> s {applicationId = a} :: CreateApplicationVersion)
{-# DEPRECATED cavApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The raw packaged AWS SAM template of your application.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavTemplateBody :: Lens.Lens' CreateApplicationVersion (Lude.Maybe Lude.Text)
cavTemplateBody = Lens.lens (templateBody :: CreateApplicationVersion -> Lude.Maybe Lude.Text) (\s a -> s {templateBody = a} :: CreateApplicationVersion)
{-# DEPRECATED cavTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | A link to the packaged AWS SAM template of your application.
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavTemplateURL :: Lens.Lens' CreateApplicationVersion (Lude.Maybe Lude.Text)
cavTemplateURL = Lens.lens (templateURL :: CreateApplicationVersion -> Lude.Maybe Lude.Text) (\s a -> s {templateURL = a} :: CreateApplicationVersion)
{-# DEPRECATED cavTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

-- | A link to the S3 object that contains the ZIP archive of the source code for this version of your application.
--
-- Maximum size 50 MB
--
-- /Note:/ Consider using 'sourceCodeArchiveURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavSourceCodeArchiveURL :: Lens.Lens' CreateApplicationVersion (Lude.Maybe Lude.Text)
cavSourceCodeArchiveURL = Lens.lens (sourceCodeArchiveURL :: CreateApplicationVersion -> Lude.Maybe Lude.Text) (\s a -> s {sourceCodeArchiveURL = a} :: CreateApplicationVersion)
{-# DEPRECATED cavSourceCodeArchiveURL "Use generic-lens or generic-optics with 'sourceCodeArchiveURL' instead." #-}

instance Lude.AWSRequest CreateApplicationVersion where
  type Rs CreateApplicationVersion = CreateApplicationVersionResponse
  request = Req.putJSON serverlessApplicationRepositoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateApplicationVersionResponse'
            Lude.<$> (x Lude..?> "creationTime")
            Lude.<*> (x Lude..?> "resourcesSupported")
            Lude.<*> (x Lude..?> "requiredCapabilities" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "parameterDefinitions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "semanticVersion")
            Lude.<*> (x Lude..?> "sourceCodeUrl")
            Lude.<*> (x Lude..?> "applicationId")
            Lude.<*> (x Lude..?> "templateUrl")
            Lude.<*> (x Lude..?> "sourceCodeArchiveUrl")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateApplicationVersion where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateApplicationVersion where
  toJSON CreateApplicationVersion' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("sourceCodeUrl" Lude..=) Lude.<$> sourceCodeURL,
            ("templateBody" Lude..=) Lude.<$> templateBody,
            ("templateUrl" Lude..=) Lude.<$> templateURL,
            ("sourceCodeArchiveUrl" Lude..=) Lude.<$> sourceCodeArchiveURL
          ]
      )

instance Lude.ToPath CreateApplicationVersion where
  toPath CreateApplicationVersion' {..} =
    Lude.mconcat
      [ "/applications/",
        Lude.toBS applicationId,
        "/versions/",
        Lude.toBS semanticVersion
      ]

instance Lude.ToQuery CreateApplicationVersion where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateApplicationVersionResponse' smart constructor.
data CreateApplicationVersionResponse = CreateApplicationVersionResponse'
  { -- | The date and time this resource was created.
    creationTime :: Lude.Maybe Lude.Text,
    -- | Whether all of the AWS resources contained in this application are supported in the region
    --
    --  in which it is being retrieved.
    resourcesSupported :: Lude.Maybe Lude.Bool,
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
    requiredCapabilities :: Lude.Maybe [Capability],
    -- | An array of parameter types supported by the application.
    parameterDefinitions :: Lude.Maybe [ParameterDefinition],
    -- | The semantic version of the application:
    --
    -- <https://semver.org/ https://semver.org/>
    semanticVersion :: Lude.Maybe Lude.Text,
    -- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
    sourceCodeURL :: Lude.Maybe Lude.Text,
    -- | The application Amazon Resource Name (ARN).
    applicationId :: Lude.Maybe Lude.Text,
    -- | A link to the packaged AWS SAM template of your application.
    templateURL :: Lude.Maybe Lude.Text,
    -- | A link to the S3 object that contains the ZIP archive of the source code for this version of your application.
    --
    -- Maximum size 50 MB
    sourceCodeArchiveURL :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateApplicationVersionResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - The date and time this resource was created.
-- * 'resourcesSupported' - Whether all of the AWS resources contained in this application are supported in the region
--
--  in which it is being retrieved.
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
-- * 'parameterDefinitions' - An array of parameter types supported by the application.
-- * 'semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
-- * 'sourceCodeURL' - A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
-- * 'applicationId' - The application Amazon Resource Name (ARN).
-- * 'templateURL' - A link to the packaged AWS SAM template of your application.
-- * 'sourceCodeArchiveURL' - A link to the S3 object that contains the ZIP archive of the source code for this version of your application.
--
-- Maximum size 50 MB
-- * 'responseStatus' - The response status code.
mkCreateApplicationVersionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateApplicationVersionResponse
mkCreateApplicationVersionResponse pResponseStatus_ =
  CreateApplicationVersionResponse'
    { creationTime = Lude.Nothing,
      resourcesSupported = Lude.Nothing,
      requiredCapabilities = Lude.Nothing,
      parameterDefinitions = Lude.Nothing,
      semanticVersion = Lude.Nothing,
      sourceCodeURL = Lude.Nothing,
      applicationId = Lude.Nothing,
      templateURL = Lude.Nothing,
      sourceCodeArchiveURL = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The date and time this resource was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrsCreationTime :: Lens.Lens' CreateApplicationVersionResponse (Lude.Maybe Lude.Text)
cavrsCreationTime = Lens.lens (creationTime :: CreateApplicationVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTime = a} :: CreateApplicationVersionResponse)
{-# DEPRECATED cavrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Whether all of the AWS resources contained in this application are supported in the region
--
--  in which it is being retrieved.
--
-- /Note:/ Consider using 'resourcesSupported' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrsResourcesSupported :: Lens.Lens' CreateApplicationVersionResponse (Lude.Maybe Lude.Bool)
cavrsResourcesSupported = Lens.lens (resourcesSupported :: CreateApplicationVersionResponse -> Lude.Maybe Lude.Bool) (\s a -> s {resourcesSupported = a} :: CreateApplicationVersionResponse)
{-# DEPRECATED cavrsResourcesSupported "Use generic-lens or generic-optics with 'resourcesSupported' instead." #-}

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
cavrsRequiredCapabilities :: Lens.Lens' CreateApplicationVersionResponse (Lude.Maybe [Capability])
cavrsRequiredCapabilities = Lens.lens (requiredCapabilities :: CreateApplicationVersionResponse -> Lude.Maybe [Capability]) (\s a -> s {requiredCapabilities = a} :: CreateApplicationVersionResponse)
{-# DEPRECATED cavrsRequiredCapabilities "Use generic-lens or generic-optics with 'requiredCapabilities' instead." #-}

-- | An array of parameter types supported by the application.
--
-- /Note:/ Consider using 'parameterDefinitions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrsParameterDefinitions :: Lens.Lens' CreateApplicationVersionResponse (Lude.Maybe [ParameterDefinition])
cavrsParameterDefinitions = Lens.lens (parameterDefinitions :: CreateApplicationVersionResponse -> Lude.Maybe [ParameterDefinition]) (\s a -> s {parameterDefinitions = a} :: CreateApplicationVersionResponse)
{-# DEPRECATED cavrsParameterDefinitions "Use generic-lens or generic-optics with 'parameterDefinitions' instead." #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrsSemanticVersion :: Lens.Lens' CreateApplicationVersionResponse (Lude.Maybe Lude.Text)
cavrsSemanticVersion = Lens.lens (semanticVersion :: CreateApplicationVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {semanticVersion = a} :: CreateApplicationVersionResponse)
{-# DEPRECATED cavrsSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

-- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
--
-- /Note:/ Consider using 'sourceCodeURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrsSourceCodeURL :: Lens.Lens' CreateApplicationVersionResponse (Lude.Maybe Lude.Text)
cavrsSourceCodeURL = Lens.lens (sourceCodeURL :: CreateApplicationVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {sourceCodeURL = a} :: CreateApplicationVersionResponse)
{-# DEPRECATED cavrsSourceCodeURL "Use generic-lens or generic-optics with 'sourceCodeURL' instead." #-}

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrsApplicationId :: Lens.Lens' CreateApplicationVersionResponse (Lude.Maybe Lude.Text)
cavrsApplicationId = Lens.lens (applicationId :: CreateApplicationVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: CreateApplicationVersionResponse)
{-# DEPRECATED cavrsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | A link to the packaged AWS SAM template of your application.
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrsTemplateURL :: Lens.Lens' CreateApplicationVersionResponse (Lude.Maybe Lude.Text)
cavrsTemplateURL = Lens.lens (templateURL :: CreateApplicationVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {templateURL = a} :: CreateApplicationVersionResponse)
{-# DEPRECATED cavrsTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

-- | A link to the S3 object that contains the ZIP archive of the source code for this version of your application.
--
-- Maximum size 50 MB
--
-- /Note:/ Consider using 'sourceCodeArchiveURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrsSourceCodeArchiveURL :: Lens.Lens' CreateApplicationVersionResponse (Lude.Maybe Lude.Text)
cavrsSourceCodeArchiveURL = Lens.lens (sourceCodeArchiveURL :: CreateApplicationVersionResponse -> Lude.Maybe Lude.Text) (\s a -> s {sourceCodeArchiveURL = a} :: CreateApplicationVersionResponse)
{-# DEPRECATED cavrsSourceCodeArchiveURL "Use generic-lens or generic-optics with 'sourceCodeArchiveURL' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cavrsResponseStatus :: Lens.Lens' CreateApplicationVersionResponse Lude.Int
cavrsResponseStatus = Lens.lens (responseStatus :: CreateApplicationVersionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateApplicationVersionResponse)
{-# DEPRECATED cavrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
