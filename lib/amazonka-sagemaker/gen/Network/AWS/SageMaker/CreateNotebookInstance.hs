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
    cniAcceleratorTypes,
    cniNotebookInstanceName,
    cniSecurityGroupIds,
    cniAdditionalCodeRepositories,
    cniLifecycleConfigName,
    cniSubnetId,
    cniInstanceType,
    cniDefaultCodeRepository,
    cniVolumeSizeInGB,
    cniKMSKeyId,
    cniRootAccess,
    cniDirectInternetAccess,
    cniTags,
    cniRoleARN,

    -- * Destructuring the response
    CreateNotebookInstanceResponse (..),
    mkCreateNotebookInstanceResponse,

    -- ** Response lenses
    cnirsNotebookInstanceARN,
    cnirsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SageMaker.Types

-- | /See:/ 'mkCreateNotebookInstance' smart constructor.
data CreateNotebookInstance = CreateNotebookInstance'
  { -- | A list of Elastic Inference (EI) instance types to associate with this notebook instance. Currently, only one instance type can be associated with a notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
    acceleratorTypes :: Lude.Maybe [NotebookInstanceAcceleratorType],
    -- | The name of the new notebook instance.
    notebookInstanceName :: Lude.Text,
    -- | The VPC security group IDs, in the form sg-xxxxxxxx. The security groups must be for the same VPC as specified in the subnet.
    securityGroupIds :: Lude.Maybe [Lude.Text],
    -- | An array of up to three Git repositories to associate with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
    additionalCodeRepositories :: Lude.Maybe [Lude.Text],
    -- | The name of a lifecycle configuration to associate with the notebook instance. For information about lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
    lifecycleConfigName :: Lude.Maybe Lude.Text,
    -- | The ID of the subnet in a VPC to which you would like to have a connectivity from your ML compute instance.
    subnetId :: Lude.Maybe Lude.Text,
    -- | The type of ML compute instance to launch for the notebook instance.
    instanceType :: InstanceType,
    -- | A Git repository to associate with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
    defaultCodeRepository :: Lude.Maybe Lude.Text,
    -- | The size, in GB, of the ML storage volume to attach to the notebook instance. The default value is 5 GB.
    volumeSizeInGB :: Lude.Maybe Lude.Natural,
    -- | The Amazon Resource Name (ARN) of a AWS Key Management Service key that Amazon SageMaker uses to encrypt data on the storage volume attached to your notebook instance. The KMS key you provide must be enabled. For information, see <https://docs.aws.amazon.com/kms/latest/developerguide/enabling-keys.html Enabling and Disabling Keys> in the /AWS Key Management Service Developer Guide/ .
    kmsKeyId :: Lude.Maybe Lude.Text,
    -- | Whether root access is enabled or disabled for users of the notebook instance. The default value is @Enabled@ .
    rootAccess :: Lude.Maybe RootAccess,
    -- | Sets whether Amazon SageMaker provides internet access to the notebook instance. If you set this to @Disabled@ this notebook instance will be able to access resources only in your VPC, and will not be able to connect to Amazon SageMaker training and endpoint services unless your configure a NAT Gateway in your VPC.
    --
    -- For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/appendix-additional-considerations.html#appendix-notebook-and-internet-access Notebook Instances Are Internet-Enabled by Default> . You can set the value of this parameter to @Disabled@ only if you set a value for the @SubnetId@ parameter.
    directInternetAccess :: Lude.Maybe DirectInternetAccess,
    -- | A list of tags to associate with the notebook instance. You can add tags later by using the @CreateTags@ API.
    tags :: Lude.Maybe [Tag],
    -- | When you send any requests to AWS resources from the notebook instance, Amazon SageMaker assumes this role to perform tasks on your behalf. You must grant this role necessary permissions so Amazon SageMaker can perform these tasks. The policy must allow the Amazon SageMaker service principal (sagemaker.amazonaws.com) permissions to assume this role. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNotebookInstance' with the minimum fields required to make a request.
--
-- * 'acceleratorTypes' - A list of Elastic Inference (EI) instance types to associate with this notebook instance. Currently, only one instance type can be associated with a notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
-- * 'notebookInstanceName' - The name of the new notebook instance.
-- * 'securityGroupIds' - The VPC security group IDs, in the form sg-xxxxxxxx. The security groups must be for the same VPC as specified in the subnet.
-- * 'additionalCodeRepositories' - An array of up to three Git repositories to associate with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
-- * 'lifecycleConfigName' - The name of a lifecycle configuration to associate with the notebook instance. For information about lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
-- * 'subnetId' - The ID of the subnet in a VPC to which you would like to have a connectivity from your ML compute instance.
-- * 'instanceType' - The type of ML compute instance to launch for the notebook instance.
-- * 'defaultCodeRepository' - A Git repository to associate with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
-- * 'volumeSizeInGB' - The size, in GB, of the ML storage volume to attach to the notebook instance. The default value is 5 GB.
-- * 'kmsKeyId' - The Amazon Resource Name (ARN) of a AWS Key Management Service key that Amazon SageMaker uses to encrypt data on the storage volume attached to your notebook instance. The KMS key you provide must be enabled. For information, see <https://docs.aws.amazon.com/kms/latest/developerguide/enabling-keys.html Enabling and Disabling Keys> in the /AWS Key Management Service Developer Guide/ .
-- * 'rootAccess' - Whether root access is enabled or disabled for users of the notebook instance. The default value is @Enabled@ .
-- * 'directInternetAccess' - Sets whether Amazon SageMaker provides internet access to the notebook instance. If you set this to @Disabled@ this notebook instance will be able to access resources only in your VPC, and will not be able to connect to Amazon SageMaker training and endpoint services unless your configure a NAT Gateway in your VPC.
--
-- For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/appendix-additional-considerations.html#appendix-notebook-and-internet-access Notebook Instances Are Internet-Enabled by Default> . You can set the value of this parameter to @Disabled@ only if you set a value for the @SubnetId@ parameter.
-- * 'tags' - A list of tags to associate with the notebook instance. You can add tags later by using the @CreateTags@ API.
-- * 'roleARN' - When you send any requests to AWS resources from the notebook instance, Amazon SageMaker assumes this role to perform tasks on your behalf. You must grant this role necessary permissions so Amazon SageMaker can perform these tasks. The policy must allow the Amazon SageMaker service principal (sagemaker.amazonaws.com) permissions to assume this role. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
mkCreateNotebookInstance ::
  -- | 'notebookInstanceName'
  Lude.Text ->
  -- | 'instanceType'
  InstanceType ->
  -- | 'roleARN'
  Lude.Text ->
  CreateNotebookInstance
mkCreateNotebookInstance
  pNotebookInstanceName_
  pInstanceType_
  pRoleARN_ =
    CreateNotebookInstance'
      { acceleratorTypes = Lude.Nothing,
        notebookInstanceName = pNotebookInstanceName_,
        securityGroupIds = Lude.Nothing,
        additionalCodeRepositories = Lude.Nothing,
        lifecycleConfigName = Lude.Nothing,
        subnetId = Lude.Nothing,
        instanceType = pInstanceType_,
        defaultCodeRepository = Lude.Nothing,
        volumeSizeInGB = Lude.Nothing,
        kmsKeyId = Lude.Nothing,
        rootAccess = Lude.Nothing,
        directInternetAccess = Lude.Nothing,
        tags = Lude.Nothing,
        roleARN = pRoleARN_
      }

-- | A list of Elastic Inference (EI) instance types to associate with this notebook instance. Currently, only one instance type can be associated with a notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker> .
--
-- /Note:/ Consider using 'acceleratorTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniAcceleratorTypes :: Lens.Lens' CreateNotebookInstance (Lude.Maybe [NotebookInstanceAcceleratorType])
cniAcceleratorTypes = Lens.lens (acceleratorTypes :: CreateNotebookInstance -> Lude.Maybe [NotebookInstanceAcceleratorType]) (\s a -> s {acceleratorTypes = a} :: CreateNotebookInstance)
{-# DEPRECATED cniAcceleratorTypes "Use generic-lens or generic-optics with 'acceleratorTypes' instead." #-}

-- | The name of the new notebook instance.
--
-- /Note:/ Consider using 'notebookInstanceName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniNotebookInstanceName :: Lens.Lens' CreateNotebookInstance Lude.Text
cniNotebookInstanceName = Lens.lens (notebookInstanceName :: CreateNotebookInstance -> Lude.Text) (\s a -> s {notebookInstanceName = a} :: CreateNotebookInstance)
{-# DEPRECATED cniNotebookInstanceName "Use generic-lens or generic-optics with 'notebookInstanceName' instead." #-}

-- | The VPC security group IDs, in the form sg-xxxxxxxx. The security groups must be for the same VPC as specified in the subnet.
--
-- /Note:/ Consider using 'securityGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniSecurityGroupIds :: Lens.Lens' CreateNotebookInstance (Lude.Maybe [Lude.Text])
cniSecurityGroupIds = Lens.lens (securityGroupIds :: CreateNotebookInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {securityGroupIds = a} :: CreateNotebookInstance)
{-# DEPRECATED cniSecurityGroupIds "Use generic-lens or generic-optics with 'securityGroupIds' instead." #-}

-- | An array of up to three Git repositories to associate with the notebook instance. These can be either the names of Git repositories stored as resources in your account, or the URL of Git repositories in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. These repositories are cloned at the same level as the default repository of your notebook instance. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- /Note:/ Consider using 'additionalCodeRepositories' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniAdditionalCodeRepositories :: Lens.Lens' CreateNotebookInstance (Lude.Maybe [Lude.Text])
cniAdditionalCodeRepositories = Lens.lens (additionalCodeRepositories :: CreateNotebookInstance -> Lude.Maybe [Lude.Text]) (\s a -> s {additionalCodeRepositories = a} :: CreateNotebookInstance)
{-# DEPRECATED cniAdditionalCodeRepositories "Use generic-lens or generic-optics with 'additionalCodeRepositories' instead." #-}

-- | The name of a lifecycle configuration to associate with the notebook instance. For information about lifestyle configurations, see <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance> .
--
-- /Note:/ Consider using 'lifecycleConfigName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniLifecycleConfigName :: Lens.Lens' CreateNotebookInstance (Lude.Maybe Lude.Text)
cniLifecycleConfigName = Lens.lens (lifecycleConfigName :: CreateNotebookInstance -> Lude.Maybe Lude.Text) (\s a -> s {lifecycleConfigName = a} :: CreateNotebookInstance)
{-# DEPRECATED cniLifecycleConfigName "Use generic-lens or generic-optics with 'lifecycleConfigName' instead." #-}

-- | The ID of the subnet in a VPC to which you would like to have a connectivity from your ML compute instance.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniSubnetId :: Lens.Lens' CreateNotebookInstance (Lude.Maybe Lude.Text)
cniSubnetId = Lens.lens (subnetId :: CreateNotebookInstance -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: CreateNotebookInstance)
{-# DEPRECATED cniSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The type of ML compute instance to launch for the notebook instance.
--
-- /Note:/ Consider using 'instanceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniInstanceType :: Lens.Lens' CreateNotebookInstance InstanceType
cniInstanceType = Lens.lens (instanceType :: CreateNotebookInstance -> InstanceType) (\s a -> s {instanceType = a} :: CreateNotebookInstance)
{-# DEPRECATED cniInstanceType "Use generic-lens or generic-optics with 'instanceType' instead." #-}

-- | A Git repository to associate with the notebook instance as its default code repository. This can be either the name of a Git repository stored as a resource in your account, or the URL of a Git repository in <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit> or in any other Git repository. When you open a notebook instance, it opens in the directory that contains this repository. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances> .
--
-- /Note:/ Consider using 'defaultCodeRepository' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniDefaultCodeRepository :: Lens.Lens' CreateNotebookInstance (Lude.Maybe Lude.Text)
cniDefaultCodeRepository = Lens.lens (defaultCodeRepository :: CreateNotebookInstance -> Lude.Maybe Lude.Text) (\s a -> s {defaultCodeRepository = a} :: CreateNotebookInstance)
{-# DEPRECATED cniDefaultCodeRepository "Use generic-lens or generic-optics with 'defaultCodeRepository' instead." #-}

-- | The size, in GB, of the ML storage volume to attach to the notebook instance. The default value is 5 GB.
--
-- /Note:/ Consider using 'volumeSizeInGB' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniVolumeSizeInGB :: Lens.Lens' CreateNotebookInstance (Lude.Maybe Lude.Natural)
cniVolumeSizeInGB = Lens.lens (volumeSizeInGB :: CreateNotebookInstance -> Lude.Maybe Lude.Natural) (\s a -> s {volumeSizeInGB = a} :: CreateNotebookInstance)
{-# DEPRECATED cniVolumeSizeInGB "Use generic-lens or generic-optics with 'volumeSizeInGB' instead." #-}

-- | The Amazon Resource Name (ARN) of a AWS Key Management Service key that Amazon SageMaker uses to encrypt data on the storage volume attached to your notebook instance. The KMS key you provide must be enabled. For information, see <https://docs.aws.amazon.com/kms/latest/developerguide/enabling-keys.html Enabling and Disabling Keys> in the /AWS Key Management Service Developer Guide/ .
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniKMSKeyId :: Lens.Lens' CreateNotebookInstance (Lude.Maybe Lude.Text)
cniKMSKeyId = Lens.lens (kmsKeyId :: CreateNotebookInstance -> Lude.Maybe Lude.Text) (\s a -> s {kmsKeyId = a} :: CreateNotebookInstance)
{-# DEPRECATED cniKMSKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

-- | Whether root access is enabled or disabled for users of the notebook instance. The default value is @Enabled@ .
--
-- /Note:/ Consider using 'rootAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniRootAccess :: Lens.Lens' CreateNotebookInstance (Lude.Maybe RootAccess)
cniRootAccess = Lens.lens (rootAccess :: CreateNotebookInstance -> Lude.Maybe RootAccess) (\s a -> s {rootAccess = a} :: CreateNotebookInstance)
{-# DEPRECATED cniRootAccess "Use generic-lens or generic-optics with 'rootAccess' instead." #-}

-- | Sets whether Amazon SageMaker provides internet access to the notebook instance. If you set this to @Disabled@ this notebook instance will be able to access resources only in your VPC, and will not be able to connect to Amazon SageMaker training and endpoint services unless your configure a NAT Gateway in your VPC.
--
-- For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/appendix-additional-considerations.html#appendix-notebook-and-internet-access Notebook Instances Are Internet-Enabled by Default> . You can set the value of this parameter to @Disabled@ only if you set a value for the @SubnetId@ parameter.
--
-- /Note:/ Consider using 'directInternetAccess' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniDirectInternetAccess :: Lens.Lens' CreateNotebookInstance (Lude.Maybe DirectInternetAccess)
cniDirectInternetAccess = Lens.lens (directInternetAccess :: CreateNotebookInstance -> Lude.Maybe DirectInternetAccess) (\s a -> s {directInternetAccess = a} :: CreateNotebookInstance)
{-# DEPRECATED cniDirectInternetAccess "Use generic-lens or generic-optics with 'directInternetAccess' instead." #-}

-- | A list of tags to associate with the notebook instance. You can add tags later by using the @CreateTags@ API.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniTags :: Lens.Lens' CreateNotebookInstance (Lude.Maybe [Tag])
cniTags = Lens.lens (tags :: CreateNotebookInstance -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateNotebookInstance)
{-# DEPRECATED cniTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | When you send any requests to AWS resources from the notebook instance, Amazon SageMaker assumes this role to perform tasks on your behalf. You must grant this role necessary permissions so Amazon SageMaker can perform these tasks. The policy must allow the Amazon SageMaker service principal (sagemaker.amazonaws.com) permissions to assume this role. For more information, see <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles> .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cniRoleARN :: Lens.Lens' CreateNotebookInstance Lude.Text
cniRoleARN = Lens.lens (roleARN :: CreateNotebookInstance -> Lude.Text) (\s a -> s {roleARN = a} :: CreateNotebookInstance)
{-# DEPRECATED cniRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateNotebookInstance where
  type Rs CreateNotebookInstance = CreateNotebookInstanceResponse
  request = Req.postJSON sageMakerService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateNotebookInstanceResponse'
            Lude.<$> (x Lude..?> "NotebookInstanceArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateNotebookInstance where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("SageMaker.CreateNotebookInstance" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateNotebookInstance where
  toJSON CreateNotebookInstance' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AcceleratorTypes" Lude..=) Lude.<$> acceleratorTypes,
            Lude.Just ("NotebookInstanceName" Lude..= notebookInstanceName),
            ("SecurityGroupIds" Lude..=) Lude.<$> securityGroupIds,
            ("AdditionalCodeRepositories" Lude..=)
              Lude.<$> additionalCodeRepositories,
            ("LifecycleConfigName" Lude..=) Lude.<$> lifecycleConfigName,
            ("SubnetId" Lude..=) Lude.<$> subnetId,
            Lude.Just ("InstanceType" Lude..= instanceType),
            ("DefaultCodeRepository" Lude..=) Lude.<$> defaultCodeRepository,
            ("VolumeSizeInGB" Lude..=) Lude.<$> volumeSizeInGB,
            ("KmsKeyId" Lude..=) Lude.<$> kmsKeyId,
            ("RootAccess" Lude..=) Lude.<$> rootAccess,
            ("DirectInternetAccess" Lude..=) Lude.<$> directInternetAccess,
            ("Tags" Lude..=) Lude.<$> tags,
            Lude.Just ("RoleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath CreateNotebookInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateNotebookInstance where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateNotebookInstanceResponse' smart constructor.
data CreateNotebookInstanceResponse = CreateNotebookInstanceResponse'
  { -- | The Amazon Resource Name (ARN) of the notebook instance.
    notebookInstanceARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateNotebookInstanceResponse' with the minimum fields required to make a request.
--
-- * 'notebookInstanceARN' - The Amazon Resource Name (ARN) of the notebook instance.
-- * 'responseStatus' - The response status code.
mkCreateNotebookInstanceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateNotebookInstanceResponse
mkCreateNotebookInstanceResponse pResponseStatus_ =
  CreateNotebookInstanceResponse'
    { notebookInstanceARN =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the notebook instance.
--
-- /Note:/ Consider using 'notebookInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnirsNotebookInstanceARN :: Lens.Lens' CreateNotebookInstanceResponse (Lude.Maybe Lude.Text)
cnirsNotebookInstanceARN = Lens.lens (notebookInstanceARN :: CreateNotebookInstanceResponse -> Lude.Maybe Lude.Text) (\s a -> s {notebookInstanceARN = a} :: CreateNotebookInstanceResponse)
{-# DEPRECATED cnirsNotebookInstanceARN "Use generic-lens or generic-optics with 'notebookInstanceARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cnirsResponseStatus :: Lens.Lens' CreateNotebookInstanceResponse Lude.Int
cnirsResponseStatus = Lens.lens (responseStatus :: CreateNotebookInstanceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateNotebookInstanceResponse)
{-# DEPRECATED cnirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
