{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.CreateNotebookInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon SageMaker notebook instance. A notebook instance is a machine learning (ML) compute instance running on a Jupyter notebook.
--
-- In a @CreateNotebookInstance@ request, specify the type of ML compute instance that you want to run. Amazon SageMaker launches the instance, installs common libraries that you can use to explore datasets for model training, and attaches an ML storage volume to the notebook instance.
-- Amazon SageMaker also provides a set of example notebooks. Each notebook demonstrates how to use Amazon SageMaker with a specific algorithm or with a machine learning framework.
-- After receiving the request, Amazon SageMaker does the following:
--
--     * Creates a network interface in the Amazon SageMaker VPC.
--
--
--     * (Option) If you specified @SubnetId@ , Amazon SageMaker creates a network interface in your own VPC, which is inferred from the subnet ID that you provide in the input. When creating this network interface, Amazon SageMaker attaches the security group that you specified in the request to the network interface that it creates in your VPC.
--
--
--     * Launches an EC2 instance of the type specified in the request in the Amazon SageMaker VPC. If you specified @SubnetId@ of your VPC, Amazon SageMaker specifies both network interfaces when launching this instance. This enables inbound traffic from your own VPC to the notebook instance, assuming that the security groups allow it.
--
--
-- After creating the notebook instance, Amazon SageMaker returns its Amazon Resource Name (ARN). You can't change the name of a notebook instance after you create it.
-- After Amazon SageMaker creates the notebook instance, you can connect to the Jupyter server and work in Jupyter notebooks. For example, you can write code to explore a dataset that you can use for model training, train a model, host models by creating Amazon SageMaker endpoints, and validate hosted models.
-- For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/how-it-works.html How It Works> .
module Network.AWS.SageMaker.CreateNotebookInstance
  ( -- * Creating a request
    CreateNotebookInstance (..),
    mkCreateNotebookInstance,

    -- ** Request lenses
    cniNotebookInstanceName,
    cniInstanceType,
    cniRoleArn,
    cniAcceleratorTypes,
    cniAdditionalCodeRepositories,
    cniDefaultCodeRepository,
    cniDirectInternetAccess,
    cniKmsKeyId,
    cniLifecycleConfigName,
    cniRootAccess,
    cniSecurityGroupIds,
    cniSubnetId,
    cniTags,
    cniVolumeSizeInGB,

    -- * Destructuring the response
    CreateNotebookInstanceResponse (..),
    mkCreateNotebookInstanceResponse,

    -- ** Response lenses
    cnirrsNotebookInstanceArn,
    cnirrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SageMaker.Types as Types

-- | /See:/ 'mkCreateNotebookInstance' smart constructor.
data CreateNotebookInstance = CreateNotebookInstance'
  { -- | The name of the new notebook instance.
    notebookInstanceName :: Types.NotebookInstanceName,
    -- | The type of ML compute instance to launch for the notebook instance.
    instanceType :: Types.InstanceType,
    -- | When you send any requests to AWS resources from the notebook instance, Amazon SageMaker assumes this role to perform tasks on your behalf. You must grant this role necessary permissions so Amazon SageMaker can perform these tasks. The policy must allow the Amazon SageMaker service principal (sagemaker.amazonaws.com) permissions to assume this role. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
    roleArn :: Types.RoleArn,
    -- | A list of Elastic Inference (EI) instance types to associate with this notebook instance. Currently, only one instance type can be associated with a notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
    acceleratorTypes :: Core.Maybe [Types.NotebookInstanceAcceleratorType],
    -- | An array of up to three Git repositories to associate with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
    additionalCodeRepositories :: Core.Maybe [Types.CodeRepositoryNameOrUrl],
    -- | A Git repository to associate with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
    defaultCodeRepository :: Core.Maybe Types.DefaultCodeRepository,
    -- | Sets whether Amazon SageMaker provides internet access to the notebook instance. If you set this to @Disabled@ this notebook instance will be able to access resources only in your VPC, and will not be able to connect to Amazon SageMaker training and endpoint services unless your configure a NAT Gateway in your VPC.
    --
    -- For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/appendix-additional-considerations.html#appendix-notebook-and-internet-access Notebook Instances Are Internet-Enabled by Default> . You can set the value of this parameter to @Disabled@ only if you set a value for the @SubnetId@ parameter.
    directInternetAccess :: Core.Maybe Types.DirectInternetAccess,
    -- | The Amazon Resource Name (ARN) of a AWS Key Management Service key that Amazon SageMaker uses to encrypt data on the storage volume attached to your notebook instance. The KMS key you provide must be enabled. For information, see <https://docs.aws.amazon.com/kms/latest/developerguide/enabling-keys.html Enabling and Disabling Keys> in the /AWS Key Management Service Developer Guide/ .
    kmsKeyId :: Core.Maybe Types.KmsKeyId,
    -- | The name of a lifecycle configuration to associate with the notebook instance. For information about lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
    lifecycleConfigName :: Core.Maybe Types.LifecycleConfigName,
    -- | Whether root access is enabled or disabled for users of the notebook instance. The default value is @Enabled@ .
    rootAccess :: Core.Maybe Types.RootAccess,
    -- | The VPC security group IDs, in the form sg-xxxxxxxx. The security groups must be for the same VPC as specified in the subnet.
    securityGroupIds :: Core.Maybe [Types.SecurityGroupId],
    -- | The ID of the subnet in a VPC to which you would like to have a connectivity from your ML compute instance.
    subnetId :: Core.Maybe Types.SubnetId,
    -- | A list of tags to associate with the notebook instance. You can add tags later by using the @CreateTags@ API.
    tags :: Core.Maybe [Types.Tag],
    -- | The size, in GB, of the ML storage volume to attach to the notebook instance. The default value is 5 GB.
    volumeSizeInGB :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNotebookInstance' value with any optional fields omitted.
mkCreateNotebookInstance ::
  -- | 'notebookInstanceName'
  Types.NotebookInstanceName ->
  -- | 'instanceType'
  Types.InstanceType ->
  -- | 'roleArn'
  Types.RoleArn ->
  CreateNotebookInstance
mkCreateNotebookInstance notebookInstanceName instanceType roleArn =
  CreateNotebookInstance'
    { notebookInstanceName,
      instanceType,
      roleArn,
      acceleratorTypes = Core.Nothing,
      additionalCodeRepositories = Core.Nothing,
      defaultCodeRepository = Core.Nothing,
      directInternetAccess = Core.Nothing,
      kmsKeyId = Core.Nothing,
      lifecycleConfigName = Core.Nothing,
      rootAccess = Core.Nothing,
      securityGroupIds = Core.Nothing,
      subnetId = Core.Nothing,
      tags = Core.Nothing,
      volumeSizeInGB = Core.Nothing
    }

-- | The name of the new notebook instance.
--
-- /Note:/ Consider using 'notebookInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniNotebookInstanceName :: Lens.Lens' CreateNotebookInstance Types.NotebookInstanceName
cniNotebookInstanceName = Lens.field @"notebookInstanceName"
{-# DEPRECATED cniNotebookInstanceName "Use generic-lens or generic-optics with 'notebookInstanceName' instead." #-}

-- | The type of ML compute instance to launch for the notebook instance.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniInstanceType :: Lens.Lens' CreateNotebookInstance Types.InstanceType
cniInstanceType = Lens.field @"instanceType"
{-# DEPRECATED cniInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | When you send any requests to AWS resources from the notebook instance, Amazon SageMaker assumes this role to perform tasks on your behalf. You must grant this role necessary permissions so Amazon SageMaker can perform these tasks. The policy must allow the Amazon SageMaker service principal (sagemaker.amazonaws.com) permissions to assume this role. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
--
-- /Note:/ Consider using 'roleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniRoleArn :: Lens.Lens' CreateNotebookInstance Types.RoleArn
cniRoleArn = Lens.field @"roleArn"
{-# DEPRECATED cniRoleArn "Use generic-lens or generic-optics with 'roleArn' instead." #-}

-- | A list of Elastic Inference (EI) instance types to associate with this notebook instance. Currently, only one instance type can be associated with a notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
--
-- /Note:/ Consider using 'acceleratorTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniAcceleratorTypes :: Lens.Lens' CreateNotebookInstance (Core.Maybe [Types.NotebookInstanceAcceleratorType])
cniAcceleratorTypes = Lens.field @"acceleratorTypes"
{-# DEPRECATED cniAcceleratorTypes "Use generic-lens or generic-optics with 'acceleratorTypes' instead." #-}

-- | An array of up to three Git repositories to associate with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- /Note:/ Consider using 'additionalCodeRepositories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniAdditionalCodeRepositories :: Lens.Lens' CreateNotebookInstance (Core.Maybe [Types.CodeRepositoryNameOrUrl])
cniAdditionalCodeRepositories = Lens.field @"additionalCodeRepositories"
{-# DEPRECATED cniAdditionalCodeRepositories "Use generic-lens or generic-optics with 'additionalCodeRepositories' instead." #-}

-- | A Git repository to associate with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- /Note:/ Consider using 'defaultCodeRepository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniDefaultCodeRepository :: Lens.Lens' CreateNotebookInstance (Core.Maybe Types.DefaultCodeRepository)
cniDefaultCodeRepository = Lens.field @"defaultCodeRepository"
{-# DEPRECATED cniDefaultCodeRepository "Use generic-lens or generic-optics with 'defaultCodeRepository' instead." #-}

-- | Sets whether Amazon SageMaker provides internet access to the notebook instance. If you set this to @Disabled@ this notebook instance will be able to access resources only in your VPC, and will not be able to connect to Amazon SageMaker training and endpoint services unless your configure a NAT Gateway in your VPC.
--
-- For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/appendix-additional-considerations.html#appendix-notebook-and-internet-access Notebook Instances Are Internet-Enabled by Default> . You can set the value of this parameter to @Disabled@ only if you set a value for the @SubnetId@ parameter.
--
-- /Note:/ Consider using 'directInternetAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniDirectInternetAccess :: Lens.Lens' CreateNotebookInstance (Core.Maybe Types.DirectInternetAccess)
cniDirectInternetAccess = Lens.field @"directInternetAccess"
{-# DEPRECATED cniDirectInternetAccess "Use generic-lens or generic-optics with 'directInternetAccess' instead." #-}

-- | The Amazon Resource Name (ARN) of a AWS Key Management Service key that Amazon SageMaker uses to encrypt data on the storage volume attached to your notebook instance. The KMS key you provide must be enabled. For information, see <https://docs.aws.amazon.com/kms/latest/developerguide/enabling-keys.html Enabling and Disabling Keys> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniKmsKeyId :: Lens.Lens' CreateNotebookInstance (Core.Maybe Types.KmsKeyId)
cniKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED cniKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | The name of a lifecycle configuration to associate with the notebook instance. For information about lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
--
-- /Note:/ Consider using 'lifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniLifecycleConfigName :: Lens.Lens' CreateNotebookInstance (Core.Maybe Types.LifecycleConfigName)
cniLifecycleConfigName = Lens.field @"lifecycleConfigName"
{-# DEPRECATED cniLifecycleConfigName "Use generic-lens or generic-optics with 'lifecycleConfigName' instead." #-}

-- | Whether root access is enabled or disabled for users of the notebook instance. The default value is @Enabled@ .
--
-- /Note:/ Consider using 'rootAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniRootAccess :: Lens.Lens' CreateNotebookInstance (Core.Maybe Types.RootAccess)
cniRootAccess = Lens.field @"rootAccess"
{-# DEPRECATED cniRootAccess "Use generic-lens or generic-optics with 'rootAccess' instead." #-}

-- | The VPC security group IDs, in the form sg-xxxxxxxx. The security groups must be for the same VPC as specified in the subnet.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniSecurityGroupIds :: Lens.Lens' CreateNotebookInstance (Core.Maybe [Types.SecurityGroupId])
cniSecurityGroupIds = Lens.field @"securityGroupIds"
{-# DEPRECATED cniSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | The ID of the subnet in a VPC to which you would like to have a connectivity from your ML compute instance.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniSubnetId :: Lens.Lens' CreateNotebookInstance (Core.Maybe Types.SubnetId)
cniSubnetId = Lens.field @"subnetId"
{-# DEPRECATED cniSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | A list of tags to associate with the notebook instance. You can add tags later by using the @CreateTags@ API.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniTags :: Lens.Lens' CreateNotebookInstance (Core.Maybe [Types.Tag])
cniTags = Lens.field @"tags"
{-# DEPRECATED cniTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The size, in GB, of the ML storage volume to attach to the notebook instance. The default value is 5 GB.
--
-- /Note:/ Consider using 'volumeSizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniVolumeSizeInGB :: Lens.Lens' CreateNotebookInstance (Core.Maybe Core.Natural)
cniVolumeSizeInGB = Lens.field @"volumeSizeInGB"
{-# DEPRECATED cniVolumeSizeInGB "Use generic-lens or generic-optics with 'volumeSizeInGB' instead." #-}

instance Core.FromJSON CreateNotebookInstance where
  toJSON CreateNotebookInstance {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("NotebookInstanceName" Core..= notebookInstanceName),
            Core.Just ("InstanceType" Core..= instanceType),
            Core.Just ("RoleArn" Core..= roleArn),
            ("AcceleratorTypes" Core..=) Core.<$> acceleratorTypes,
            ("AdditionalCodeRepositories" Core..=)
              Core.<$> additionalCodeRepositories,
            ("DefaultCodeRepository" Core..=) Core.<$> defaultCodeRepository,
            ("DirectInternetAccess" Core..=) Core.<$> directInternetAccess,
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId,
            ("LifecycleConfigName" Core..=) Core.<$> lifecycleConfigName,
            ("RootAccess" Core..=) Core.<$> rootAccess,
            ("SecurityGroupIds" Core..=) Core.<$> securityGroupIds,
            ("SubnetId" Core..=) Core.<$> subnetId,
            ("Tags" Core..=) Core.<$> tags,
            ("VolumeSizeInGB" Core..=) Core.<$> volumeSizeInGB
          ]
      )

instance Core.AWSRequest CreateNotebookInstance where
  type Rs CreateNotebookInstance = CreateNotebookInstanceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "SageMaker.CreateNotebookInstance")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNotebookInstanceResponse'
            Core.<$> (x Core..:? "NotebookInstanceArn")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateNotebookInstanceResponse' smart constructor.
data CreateNotebookInstanceResponse = CreateNotebookInstanceResponse'
  { -- | The Amazon Resource Name (ARN) of the notebook instance.
    notebookInstanceArn :: Core.Maybe Types.NotebookInstanceArn,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateNotebookInstanceResponse' value with any optional fields omitted.
mkCreateNotebookInstanceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateNotebookInstanceResponse
mkCreateNotebookInstanceResponse responseStatus =
  CreateNotebookInstanceResponse'
    { notebookInstanceArn =
        Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the notebook instance.
--
-- /Note:/ Consider using 'notebookInstanceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnirrsNotebookInstanceArn :: Lens.Lens' CreateNotebookInstanceResponse (Core.Maybe Types.NotebookInstanceArn)
cnirrsNotebookInstanceArn = Lens.field @"notebookInstanceArn"
{-# DEPRECATED cnirrsNotebookInstanceArn "Use generic-lens or generic-optics with 'notebookInstanceArn' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnirrsResponseStatus :: Lens.Lens' CreateNotebookInstanceResponse Core.Int
cnirrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cnirrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
