{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    ccfcsClientToken,
    ccfcsTemplateId,
    ccfcsSemanticVersion,
    ccfcsNotificationARNs,
    ccfcsChangeSetName,
    ccfcsDescription,
    ccfcsCapabilities,
    ccfcsParameterOverrides,
    ccfcsRollbackConfiguration,
    ccfcsResourceTypes,
    ccfcsTags,
    ccfcsApplicationId,
    ccfcsStackName,

    -- * Destructuring the response
    CreateCloudFormationChangeSetResponse (..),
    mkCreateCloudFormationChangeSetResponse,

    -- ** Response lenses
    ccfcsrsSemanticVersion,
    ccfcsrsChangeSetId,
    ccfcsrsApplicationId,
    ccfcsrsStackId,
    ccfcsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'mkCreateCloudFormationChangeSet' smart constructor.
data CreateCloudFormationChangeSet = CreateCloudFormationChangeSet'
  { clientToken ::
      Lude.Maybe Lude.Text,
    templateId ::
      Lude.Maybe Lude.Text,
    semanticVersion ::
      Lude.Maybe Lude.Text,
    notificationARNs ::
      Lude.Maybe [Lude.Text],
    changeSetName ::
      Lude.Maybe Lude.Text,
    description ::
      Lude.Maybe Lude.Text,
    capabilities ::
      Lude.Maybe [Lude.Text],
    parameterOverrides ::
      Lude.Maybe [ParameterValue],
    rollbackConfiguration ::
      Lude.Maybe
        RollbackConfiguration,
    resourceTypes ::
      Lude.Maybe [Lude.Text],
    tags :: Lude.Maybe [Tag],
    applicationId :: Lude.Text,
    stackName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCloudFormationChangeSet' with the minimum fields required to make a request.
--
-- * 'applicationId' - The Amazon Resource Name (ARN) of the application.
-- * 'capabilities' - A list of values that you must specify before you can deploy certain applications.
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
-- * 'changeSetName' - This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
-- * 'clientToken' - This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
-- * 'description' - This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
-- * 'notificationARNs' - This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
-- * 'parameterOverrides' - A list of parameter values for the parameters of the application.
-- * 'resourceTypes' - This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
-- * 'rollbackConfiguration' - This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
-- * 'semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
-- * 'stackName' - This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
-- * 'tags' - This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
-- * 'templateId' - The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
mkCreateCloudFormationChangeSet ::
  -- | 'applicationId'
  Lude.Text ->
  -- | 'stackName'
  Lude.Text ->
  CreateCloudFormationChangeSet
mkCreateCloudFormationChangeSet pApplicationId_ pStackName_ =
  CreateCloudFormationChangeSet'
    { clientToken = Lude.Nothing,
      templateId = Lude.Nothing,
      semanticVersion = Lude.Nothing,
      notificationARNs = Lude.Nothing,
      changeSetName = Lude.Nothing,
      description = Lude.Nothing,
      capabilities = Lude.Nothing,
      parameterOverrides = Lude.Nothing,
      rollbackConfiguration = Lude.Nothing,
      resourceTypes = Lude.Nothing,
      tags = Lude.Nothing,
      applicationId = pApplicationId_,
      stackName = pStackName_
    }

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- /Note:/ Consider using 'clientToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsClientToken :: Lens.Lens' CreateCloudFormationChangeSet (Lude.Maybe Lude.Text)
ccfcsClientToken = Lens.lens (clientToken :: CreateCloudFormationChangeSet -> Lude.Maybe Lude.Text) (\s a -> s {clientToken = a} :: CreateCloudFormationChangeSet)
{-# DEPRECATED ccfcsClientToken "Use generic-lens or generic-optics with 'clientToken' instead." #-}

-- | The UUID returned by CreateCloudFormationTemplate.
--
-- Pattern: [0-9a-fA-F]{8}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{4}\-[0-9a-fA-F]{12}
--
-- /Note:/ Consider using 'templateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsTemplateId :: Lens.Lens' CreateCloudFormationChangeSet (Lude.Maybe Lude.Text)
ccfcsTemplateId = Lens.lens (templateId :: CreateCloudFormationChangeSet -> Lude.Maybe Lude.Text) (\s a -> s {templateId = a} :: CreateCloudFormationChangeSet)
{-# DEPRECATED ccfcsTemplateId "Use generic-lens or generic-optics with 'templateId' instead." #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsSemanticVersion :: Lens.Lens' CreateCloudFormationChangeSet (Lude.Maybe Lude.Text)
ccfcsSemanticVersion = Lens.lens (semanticVersion :: CreateCloudFormationChangeSet -> Lude.Maybe Lude.Text) (\s a -> s {semanticVersion = a} :: CreateCloudFormationChangeSet)
{-# DEPRECATED ccfcsSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- /Note:/ Consider using 'notificationARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsNotificationARNs :: Lens.Lens' CreateCloudFormationChangeSet (Lude.Maybe [Lude.Text])
ccfcsNotificationARNs = Lens.lens (notificationARNs :: CreateCloudFormationChangeSet -> Lude.Maybe [Lude.Text]) (\s a -> s {notificationARNs = a} :: CreateCloudFormationChangeSet)
{-# DEPRECATED ccfcsNotificationARNs "Use generic-lens or generic-optics with 'notificationARNs' instead." #-}

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- /Note:/ Consider using 'changeSetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsChangeSetName :: Lens.Lens' CreateCloudFormationChangeSet (Lude.Maybe Lude.Text)
ccfcsChangeSetName = Lens.lens (changeSetName :: CreateCloudFormationChangeSet -> Lude.Maybe Lude.Text) (\s a -> s {changeSetName = a} :: CreateCloudFormationChangeSet)
{-# DEPRECATED ccfcsChangeSetName "Use generic-lens or generic-optics with 'changeSetName' instead." #-}

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsDescription :: Lens.Lens' CreateCloudFormationChangeSet (Lude.Maybe Lude.Text)
ccfcsDescription = Lens.lens (description :: CreateCloudFormationChangeSet -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateCloudFormationChangeSet)
{-# DEPRECATED ccfcsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

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
ccfcsCapabilities :: Lens.Lens' CreateCloudFormationChangeSet (Lude.Maybe [Lude.Text])
ccfcsCapabilities = Lens.lens (capabilities :: CreateCloudFormationChangeSet -> Lude.Maybe [Lude.Text]) (\s a -> s {capabilities = a} :: CreateCloudFormationChangeSet)
{-# DEPRECATED ccfcsCapabilities "Use generic-lens or generic-optics with 'capabilities' instead." #-}

-- | A list of parameter values for the parameters of the application.
--
-- /Note:/ Consider using 'parameterOverrides' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsParameterOverrides :: Lens.Lens' CreateCloudFormationChangeSet (Lude.Maybe [ParameterValue])
ccfcsParameterOverrides = Lens.lens (parameterOverrides :: CreateCloudFormationChangeSet -> Lude.Maybe [ParameterValue]) (\s a -> s {parameterOverrides = a} :: CreateCloudFormationChangeSet)
{-# DEPRECATED ccfcsParameterOverrides "Use generic-lens or generic-optics with 'parameterOverrides' instead." #-}

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- /Note:/ Consider using 'rollbackConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsRollbackConfiguration :: Lens.Lens' CreateCloudFormationChangeSet (Lude.Maybe RollbackConfiguration)
ccfcsRollbackConfiguration = Lens.lens (rollbackConfiguration :: CreateCloudFormationChangeSet -> Lude.Maybe RollbackConfiguration) (\s a -> s {rollbackConfiguration = a} :: CreateCloudFormationChangeSet)
{-# DEPRECATED ccfcsRollbackConfiguration "Use generic-lens or generic-optics with 'rollbackConfiguration' instead." #-}

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- /Note:/ Consider using 'resourceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsResourceTypes :: Lens.Lens' CreateCloudFormationChangeSet (Lude.Maybe [Lude.Text])
ccfcsResourceTypes = Lens.lens (resourceTypes :: CreateCloudFormationChangeSet -> Lude.Maybe [Lude.Text]) (\s a -> s {resourceTypes = a} :: CreateCloudFormationChangeSet)
{-# DEPRECATED ccfcsResourceTypes "Use generic-lens or generic-optics with 'resourceTypes' instead." #-}

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsTags :: Lens.Lens' CreateCloudFormationChangeSet (Lude.Maybe [Tag])
ccfcsTags = Lens.lens (tags :: CreateCloudFormationChangeSet -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateCloudFormationChangeSet)
{-# DEPRECATED ccfcsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsApplicationId :: Lens.Lens' CreateCloudFormationChangeSet Lude.Text
ccfcsApplicationId = Lens.lens (applicationId :: CreateCloudFormationChangeSet -> Lude.Text) (\s a -> s {applicationId = a} :: CreateCloudFormationChangeSet)
{-# DEPRECATED ccfcsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | This property corresponds to the parameter of the same name for the /AWS CloudFormation <https:\/\/docs.aws.amazon.com\/goto\/WebAPI\/cloudformation-2010-05-15\/CreateChangeSet CreateChangeSet> / API.
--
-- /Note:/ Consider using 'stackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsStackName :: Lens.Lens' CreateCloudFormationChangeSet Lude.Text
ccfcsStackName = Lens.lens (stackName :: CreateCloudFormationChangeSet -> Lude.Text) (\s a -> s {stackName = a} :: CreateCloudFormationChangeSet)
{-# DEPRECATED ccfcsStackName "Use generic-lens or generic-optics with 'stackName' instead." #-}

instance Lude.AWSRequest CreateCloudFormationChangeSet where
  type
    Rs CreateCloudFormationChangeSet =
      CreateCloudFormationChangeSetResponse
  request = Req.postJSON serverlessApplicationRepositoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateCloudFormationChangeSetResponse'
            Lude.<$> (x Lude..?> "semanticVersion")
            Lude.<*> (x Lude..?> "changeSetId")
            Lude.<*> (x Lude..?> "applicationId")
            Lude.<*> (x Lude..?> "stackId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateCloudFormationChangeSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateCloudFormationChangeSet where
  toJSON CreateCloudFormationChangeSet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("clientToken" Lude..=) Lude.<$> clientToken,
            ("templateId" Lude..=) Lude.<$> templateId,
            ("semanticVersion" Lude..=) Lude.<$> semanticVersion,
            ("notificationArns" Lude..=) Lude.<$> notificationARNs,
            ("changeSetName" Lude..=) Lude.<$> changeSetName,
            ("description" Lude..=) Lude.<$> description,
            ("capabilities" Lude..=) Lude.<$> capabilities,
            ("parameterOverrides" Lude..=) Lude.<$> parameterOverrides,
            ("rollbackConfiguration" Lude..=) Lude.<$> rollbackConfiguration,
            ("resourceTypes" Lude..=) Lude.<$> resourceTypes,
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("stackName" Lude..= stackName)
          ]
      )

instance Lude.ToPath CreateCloudFormationChangeSet where
  toPath CreateCloudFormationChangeSet' {..} =
    Lude.mconcat
      ["/applications/", Lude.toBS applicationId, "/changesets"]

instance Lude.ToQuery CreateCloudFormationChangeSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateCloudFormationChangeSetResponse' smart constructor.
data CreateCloudFormationChangeSetResponse = CreateCloudFormationChangeSetResponse'
  { semanticVersion ::
      Lude.Maybe
        Lude.Text,
    changeSetId ::
      Lude.Maybe
        Lude.Text,
    applicationId ::
      Lude.Maybe
        Lude.Text,
    stackId ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateCloudFormationChangeSetResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The application Amazon Resource Name (ARN).
-- * 'changeSetId' - The Amazon Resource Name (ARN) of the change set.
--
-- Length constraints: Minimum length of 1.
-- Pattern: ARN:[-a-zA-Z0-9:/]*
-- * 'responseStatus' - The response status code.
-- * 'semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
-- * 'stackId' - The unique ID of the stack.
mkCreateCloudFormationChangeSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateCloudFormationChangeSetResponse
mkCreateCloudFormationChangeSetResponse pResponseStatus_ =
  CreateCloudFormationChangeSetResponse'
    { semanticVersion =
        Lude.Nothing,
      changeSetId = Lude.Nothing,
      applicationId = Lude.Nothing,
      stackId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsrsSemanticVersion :: Lens.Lens' CreateCloudFormationChangeSetResponse (Lude.Maybe Lude.Text)
ccfcsrsSemanticVersion = Lens.lens (semanticVersion :: CreateCloudFormationChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {semanticVersion = a} :: CreateCloudFormationChangeSetResponse)
{-# DEPRECATED ccfcsrsSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

-- | The Amazon Resource Name (ARN) of the change set.
--
-- Length constraints: Minimum length of 1.
-- Pattern: ARN:[-a-zA-Z0-9:/]*
--
-- /Note:/ Consider using 'changeSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsrsChangeSetId :: Lens.Lens' CreateCloudFormationChangeSetResponse (Lude.Maybe Lude.Text)
ccfcsrsChangeSetId = Lens.lens (changeSetId :: CreateCloudFormationChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {changeSetId = a} :: CreateCloudFormationChangeSetResponse)
{-# DEPRECATED ccfcsrsChangeSetId "Use generic-lens or generic-optics with 'changeSetId' instead." #-}

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsrsApplicationId :: Lens.Lens' CreateCloudFormationChangeSetResponse (Lude.Maybe Lude.Text)
ccfcsrsApplicationId = Lens.lens (applicationId :: CreateCloudFormationChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: CreateCloudFormationChangeSetResponse)
{-# DEPRECATED ccfcsrsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The unique ID of the stack.
--
-- /Note:/ Consider using 'stackId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsrsStackId :: Lens.Lens' CreateCloudFormationChangeSetResponse (Lude.Maybe Lude.Text)
ccfcsrsStackId = Lens.lens (stackId :: CreateCloudFormationChangeSetResponse -> Lude.Maybe Lude.Text) (\s a -> s {stackId = a} :: CreateCloudFormationChangeSetResponse)
{-# DEPRECATED ccfcsrsStackId "Use generic-lens or generic-optics with 'stackId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccfcsrsResponseStatus :: Lens.Lens' CreateCloudFormationChangeSetResponse Lude.Int
ccfcsrsResponseStatus = Lens.lens (responseStatus :: CreateCloudFormationChangeSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateCloudFormationChangeSetResponse)
{-# DEPRECATED ccfcsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
