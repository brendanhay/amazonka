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
-- Module      : Amazonka.SageMaker.CreateNotebookInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon SageMaker notebook instance. A notebook instance is a
-- machine learning (ML) compute instance running on a Jupyter notebook.
--
-- In a @CreateNotebookInstance@ request, specify the type of ML compute
-- instance that you want to run. Amazon SageMaker launches the instance,
-- installs common libraries that you can use to explore datasets for model
-- training, and attaches an ML storage volume to the notebook instance.
--
-- Amazon SageMaker also provides a set of example notebooks. Each notebook
-- demonstrates how to use Amazon SageMaker with a specific algorithm or
-- with a machine learning framework.
--
-- After receiving the request, Amazon SageMaker does the following:
--
-- 1.  Creates a network interface in the Amazon SageMaker VPC.
--
-- 2.  (Option) If you specified @SubnetId@, Amazon SageMaker creates a
--     network interface in your own VPC, which is inferred from the subnet
--     ID that you provide in the input. When creating this network
--     interface, Amazon SageMaker attaches the security group that you
--     specified in the request to the network interface that it creates in
--     your VPC.
--
-- 3.  Launches an EC2 instance of the type specified in the request in the
--     Amazon SageMaker VPC. If you specified @SubnetId@ of your VPC,
--     Amazon SageMaker specifies both network interfaces when launching
--     this instance. This enables inbound traffic from your own VPC to the
--     notebook instance, assuming that the security groups allow it.
--
-- After creating the notebook instance, Amazon SageMaker returns its
-- Amazon Resource Name (ARN). You can\'t change the name of a notebook
-- instance after you create it.
--
-- After Amazon SageMaker creates the notebook instance, you can connect to
-- the Jupyter server and work in Jupyter notebooks. For example, you can
-- write code to explore a dataset that you can use for model training,
-- train a model, host models by creating Amazon SageMaker endpoints, and
-- validate hosted models.
--
-- For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/how-it-works.html How It Works>.
module Amazonka.SageMaker.CreateNotebookInstance
  ( -- * Creating a Request
    CreateNotebookInstance (..),
    newCreateNotebookInstance,

    -- * Request Lenses
    createNotebookInstance_acceleratorTypes,
    createNotebookInstance_platformIdentifier,
    createNotebookInstance_securityGroupIds,
    createNotebookInstance_additionalCodeRepositories,
    createNotebookInstance_lifecycleConfigName,
    createNotebookInstance_subnetId,
    createNotebookInstance_defaultCodeRepository,
    createNotebookInstance_volumeSizeInGB,
    createNotebookInstance_kmsKeyId,
    createNotebookInstance_rootAccess,
    createNotebookInstance_directInternetAccess,
    createNotebookInstance_tags,
    createNotebookInstance_notebookInstanceName,
    createNotebookInstance_instanceType,
    createNotebookInstance_roleArn,

    -- * Destructuring the Response
    CreateNotebookInstanceResponse (..),
    newCreateNotebookInstanceResponse,

    -- * Response Lenses
    createNotebookInstanceResponse_notebookInstanceArn,
    createNotebookInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateNotebookInstance' smart constructor.
data CreateNotebookInstance = CreateNotebookInstance'
  { -- | A list of Elastic Inference (EI) instance types to associate with this
    -- notebook instance. Currently, only one instance type can be associated
    -- with a notebook instance. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
    acceleratorTypes :: Prelude.Maybe [NotebookInstanceAcceleratorType],
    -- | The platform identifier of the notebook instance runtime environment.
    platformIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The VPC security group IDs, in the form sg-xxxxxxxx. The security groups
    -- must be for the same VPC as specified in the subnet.
    securityGroupIds :: Prelude.Maybe [Prelude.Text],
    -- | An array of up to three Git repositories to associate with the notebook
    -- instance. These can be either the names of Git repositories stored as
    -- resources in your account, or the URL of Git repositories in
    -- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html Amazon Web Services CodeCommit>
    -- or in any other Git repository. These repositories are cloned at the
    -- same level as the default repository of your notebook instance. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances>.
    additionalCodeRepositories :: Prelude.Maybe [Prelude.Text],
    -- | The name of a lifecycle configuration to associate with the notebook
    -- instance. For information about lifestyle configurations, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>.
    lifecycleConfigName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet in a VPC to which you would like to have a
    -- connectivity from your ML compute instance.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | A Git repository to associate with the notebook instance as its default
    -- code repository. This can be either the name of a Git repository stored
    -- as a resource in your account, or the URL of a Git repository in
    -- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html Amazon Web Services CodeCommit>
    -- or in any other Git repository. When you open a notebook instance, it
    -- opens in the directory that contains this repository. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances>.
    defaultCodeRepository :: Prelude.Maybe Prelude.Text,
    -- | The size, in GB, of the ML storage volume to attach to the notebook
    -- instance. The default value is 5 GB.
    volumeSizeInGB :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of a Amazon Web Services Key Management
    -- Service key that Amazon SageMaker uses to encrypt data on the storage
    -- volume attached to your notebook instance. The KMS key you provide must
    -- be enabled. For information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/enabling-keys.html Enabling and Disabling Keys>
    -- in the /Amazon Web Services Key Management Service Developer Guide/.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | Whether root access is enabled or disabled for users of the notebook
    -- instance. The default value is @Enabled@.
    --
    -- Lifecycle configurations need root access to be able to set up a
    -- notebook instance. Because of this, lifecycle configurations associated
    -- with a notebook instance always run with root access even if you disable
    -- root access for users.
    rootAccess :: Prelude.Maybe RootAccess,
    -- | Sets whether Amazon SageMaker provides internet access to the notebook
    -- instance. If you set this to @Disabled@ this notebook instance is able
    -- to access resources only in your VPC, and is not be able to connect to
    -- Amazon SageMaker training and endpoint services unless you configure a
    -- NAT Gateway in your VPC.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/appendix-additional-considerations.html#appendix-notebook-and-internet-access Notebook Instances Are Internet-Enabled by Default>.
    -- You can set the value of this parameter to @Disabled@ only if you set a
    -- value for the @SubnetId@ parameter.
    directInternetAccess :: Prelude.Maybe DirectInternetAccess,
    -- | An array of key-value pairs. You can use tags to categorize your Amazon
    -- Web Services resources in different ways, for example, by purpose,
    -- owner, or environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the new notebook instance.
    notebookInstanceName :: Prelude.Text,
    -- | The type of ML compute instance to launch for the notebook instance.
    instanceType :: InstanceType,
    -- | When you send any requests to Amazon Web Services resources from the
    -- notebook instance, Amazon SageMaker assumes this role to perform tasks
    -- on your behalf. You must grant this role necessary permissions so Amazon
    -- SageMaker can perform these tasks. The policy must allow the Amazon
    -- SageMaker service principal (sagemaker.amazonaws.com) permissions to
    -- assume this role. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles>.
    --
    -- To be able to pass this role to Amazon SageMaker, the caller of this API
    -- must have the @iam:PassRole@ permission.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNotebookInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorTypes', 'createNotebookInstance_acceleratorTypes' - A list of Elastic Inference (EI) instance types to associate with this
-- notebook instance. Currently, only one instance type can be associated
-- with a notebook instance. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
--
-- 'platformIdentifier', 'createNotebookInstance_platformIdentifier' - The platform identifier of the notebook instance runtime environment.
--
-- 'securityGroupIds', 'createNotebookInstance_securityGroupIds' - The VPC security group IDs, in the form sg-xxxxxxxx. The security groups
-- must be for the same VPC as specified in the subnet.
--
-- 'additionalCodeRepositories', 'createNotebookInstance_additionalCodeRepositories' - An array of up to three Git repositories to associate with the notebook
-- instance. These can be either the names of Git repositories stored as
-- resources in your account, or the URL of Git repositories in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html Amazon Web Services CodeCommit>
-- or in any other Git repository. These repositories are cloned at the
-- same level as the default repository of your notebook instance. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances>.
--
-- 'lifecycleConfigName', 'createNotebookInstance_lifecycleConfigName' - The name of a lifecycle configuration to associate with the notebook
-- instance. For information about lifestyle configurations, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>.
--
-- 'subnetId', 'createNotebookInstance_subnetId' - The ID of the subnet in a VPC to which you would like to have a
-- connectivity from your ML compute instance.
--
-- 'defaultCodeRepository', 'createNotebookInstance_defaultCodeRepository' - A Git repository to associate with the notebook instance as its default
-- code repository. This can be either the name of a Git repository stored
-- as a resource in your account, or the URL of a Git repository in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html Amazon Web Services CodeCommit>
-- or in any other Git repository. When you open a notebook instance, it
-- opens in the directory that contains this repository. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances>.
--
-- 'volumeSizeInGB', 'createNotebookInstance_volumeSizeInGB' - The size, in GB, of the ML storage volume to attach to the notebook
-- instance. The default value is 5 GB.
--
-- 'kmsKeyId', 'createNotebookInstance_kmsKeyId' - The Amazon Resource Name (ARN) of a Amazon Web Services Key Management
-- Service key that Amazon SageMaker uses to encrypt data on the storage
-- volume attached to your notebook instance. The KMS key you provide must
-- be enabled. For information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/enabling-keys.html Enabling and Disabling Keys>
-- in the /Amazon Web Services Key Management Service Developer Guide/.
--
-- 'rootAccess', 'createNotebookInstance_rootAccess' - Whether root access is enabled or disabled for users of the notebook
-- instance. The default value is @Enabled@.
--
-- Lifecycle configurations need root access to be able to set up a
-- notebook instance. Because of this, lifecycle configurations associated
-- with a notebook instance always run with root access even if you disable
-- root access for users.
--
-- 'directInternetAccess', 'createNotebookInstance_directInternetAccess' - Sets whether Amazon SageMaker provides internet access to the notebook
-- instance. If you set this to @Disabled@ this notebook instance is able
-- to access resources only in your VPC, and is not be able to connect to
-- Amazon SageMaker training and endpoint services unless you configure a
-- NAT Gateway in your VPC.
--
-- For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/appendix-additional-considerations.html#appendix-notebook-and-internet-access Notebook Instances Are Internet-Enabled by Default>.
-- You can set the value of this parameter to @Disabled@ only if you set a
-- value for the @SubnetId@ parameter.
--
-- 'tags', 'createNotebookInstance_tags' - An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
--
-- 'notebookInstanceName', 'createNotebookInstance_notebookInstanceName' - The name of the new notebook instance.
--
-- 'instanceType', 'createNotebookInstance_instanceType' - The type of ML compute instance to launch for the notebook instance.
--
-- 'roleArn', 'createNotebookInstance_roleArn' - When you send any requests to Amazon Web Services resources from the
-- notebook instance, Amazon SageMaker assumes this role to perform tasks
-- on your behalf. You must grant this role necessary permissions so Amazon
-- SageMaker can perform these tasks. The policy must allow the Amazon
-- SageMaker service principal (sagemaker.amazonaws.com) permissions to
-- assume this role. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles>.
--
-- To be able to pass this role to Amazon SageMaker, the caller of this API
-- must have the @iam:PassRole@ permission.
newCreateNotebookInstance ::
  -- | 'notebookInstanceName'
  Prelude.Text ->
  -- | 'instanceType'
  InstanceType ->
  -- | 'roleArn'
  Prelude.Text ->
  CreateNotebookInstance
newCreateNotebookInstance
  pNotebookInstanceName_
  pInstanceType_
  pRoleArn_ =
    CreateNotebookInstance'
      { acceleratorTypes =
          Prelude.Nothing,
        platformIdentifier = Prelude.Nothing,
        securityGroupIds = Prelude.Nothing,
        additionalCodeRepositories = Prelude.Nothing,
        lifecycleConfigName = Prelude.Nothing,
        subnetId = Prelude.Nothing,
        defaultCodeRepository = Prelude.Nothing,
        volumeSizeInGB = Prelude.Nothing,
        kmsKeyId = Prelude.Nothing,
        rootAccess = Prelude.Nothing,
        directInternetAccess = Prelude.Nothing,
        tags = Prelude.Nothing,
        notebookInstanceName = pNotebookInstanceName_,
        instanceType = pInstanceType_,
        roleArn = pRoleArn_
      }

-- | A list of Elastic Inference (EI) instance types to associate with this
-- notebook instance. Currently, only one instance type can be associated
-- with a notebook instance. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
createNotebookInstance_acceleratorTypes :: Lens.Lens' CreateNotebookInstance (Prelude.Maybe [NotebookInstanceAcceleratorType])
createNotebookInstance_acceleratorTypes = Lens.lens (\CreateNotebookInstance' {acceleratorTypes} -> acceleratorTypes) (\s@CreateNotebookInstance' {} a -> s {acceleratorTypes = a} :: CreateNotebookInstance) Prelude.. Lens.mapping Lens.coerced

-- | The platform identifier of the notebook instance runtime environment.
createNotebookInstance_platformIdentifier :: Lens.Lens' CreateNotebookInstance (Prelude.Maybe Prelude.Text)
createNotebookInstance_platformIdentifier = Lens.lens (\CreateNotebookInstance' {platformIdentifier} -> platformIdentifier) (\s@CreateNotebookInstance' {} a -> s {platformIdentifier = a} :: CreateNotebookInstance)

-- | The VPC security group IDs, in the form sg-xxxxxxxx. The security groups
-- must be for the same VPC as specified in the subnet.
createNotebookInstance_securityGroupIds :: Lens.Lens' CreateNotebookInstance (Prelude.Maybe [Prelude.Text])
createNotebookInstance_securityGroupIds = Lens.lens (\CreateNotebookInstance' {securityGroupIds} -> securityGroupIds) (\s@CreateNotebookInstance' {} a -> s {securityGroupIds = a} :: CreateNotebookInstance) Prelude.. Lens.mapping Lens.coerced

-- | An array of up to three Git repositories to associate with the notebook
-- instance. These can be either the names of Git repositories stored as
-- resources in your account, or the URL of Git repositories in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html Amazon Web Services CodeCommit>
-- or in any other Git repository. These repositories are cloned at the
-- same level as the default repository of your notebook instance. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances>.
createNotebookInstance_additionalCodeRepositories :: Lens.Lens' CreateNotebookInstance (Prelude.Maybe [Prelude.Text])
createNotebookInstance_additionalCodeRepositories = Lens.lens (\CreateNotebookInstance' {additionalCodeRepositories} -> additionalCodeRepositories) (\s@CreateNotebookInstance' {} a -> s {additionalCodeRepositories = a} :: CreateNotebookInstance) Prelude.. Lens.mapping Lens.coerced

-- | The name of a lifecycle configuration to associate with the notebook
-- instance. For information about lifestyle configurations, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>.
createNotebookInstance_lifecycleConfigName :: Lens.Lens' CreateNotebookInstance (Prelude.Maybe Prelude.Text)
createNotebookInstance_lifecycleConfigName = Lens.lens (\CreateNotebookInstance' {lifecycleConfigName} -> lifecycleConfigName) (\s@CreateNotebookInstance' {} a -> s {lifecycleConfigName = a} :: CreateNotebookInstance)

-- | The ID of the subnet in a VPC to which you would like to have a
-- connectivity from your ML compute instance.
createNotebookInstance_subnetId :: Lens.Lens' CreateNotebookInstance (Prelude.Maybe Prelude.Text)
createNotebookInstance_subnetId = Lens.lens (\CreateNotebookInstance' {subnetId} -> subnetId) (\s@CreateNotebookInstance' {} a -> s {subnetId = a} :: CreateNotebookInstance)

-- | A Git repository to associate with the notebook instance as its default
-- code repository. This can be either the name of a Git repository stored
-- as a resource in your account, or the URL of a Git repository in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html Amazon Web Services CodeCommit>
-- or in any other Git repository. When you open a notebook instance, it
-- opens in the directory that contains this repository. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances>.
createNotebookInstance_defaultCodeRepository :: Lens.Lens' CreateNotebookInstance (Prelude.Maybe Prelude.Text)
createNotebookInstance_defaultCodeRepository = Lens.lens (\CreateNotebookInstance' {defaultCodeRepository} -> defaultCodeRepository) (\s@CreateNotebookInstance' {} a -> s {defaultCodeRepository = a} :: CreateNotebookInstance)

-- | The size, in GB, of the ML storage volume to attach to the notebook
-- instance. The default value is 5 GB.
createNotebookInstance_volumeSizeInGB :: Lens.Lens' CreateNotebookInstance (Prelude.Maybe Prelude.Natural)
createNotebookInstance_volumeSizeInGB = Lens.lens (\CreateNotebookInstance' {volumeSizeInGB} -> volumeSizeInGB) (\s@CreateNotebookInstance' {} a -> s {volumeSizeInGB = a} :: CreateNotebookInstance)

-- | The Amazon Resource Name (ARN) of a Amazon Web Services Key Management
-- Service key that Amazon SageMaker uses to encrypt data on the storage
-- volume attached to your notebook instance. The KMS key you provide must
-- be enabled. For information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/enabling-keys.html Enabling and Disabling Keys>
-- in the /Amazon Web Services Key Management Service Developer Guide/.
createNotebookInstance_kmsKeyId :: Lens.Lens' CreateNotebookInstance (Prelude.Maybe Prelude.Text)
createNotebookInstance_kmsKeyId = Lens.lens (\CreateNotebookInstance' {kmsKeyId} -> kmsKeyId) (\s@CreateNotebookInstance' {} a -> s {kmsKeyId = a} :: CreateNotebookInstance)

-- | Whether root access is enabled or disabled for users of the notebook
-- instance. The default value is @Enabled@.
--
-- Lifecycle configurations need root access to be able to set up a
-- notebook instance. Because of this, lifecycle configurations associated
-- with a notebook instance always run with root access even if you disable
-- root access for users.
createNotebookInstance_rootAccess :: Lens.Lens' CreateNotebookInstance (Prelude.Maybe RootAccess)
createNotebookInstance_rootAccess = Lens.lens (\CreateNotebookInstance' {rootAccess} -> rootAccess) (\s@CreateNotebookInstance' {} a -> s {rootAccess = a} :: CreateNotebookInstance)

-- | Sets whether Amazon SageMaker provides internet access to the notebook
-- instance. If you set this to @Disabled@ this notebook instance is able
-- to access resources only in your VPC, and is not be able to connect to
-- Amazon SageMaker training and endpoint services unless you configure a
-- NAT Gateway in your VPC.
--
-- For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/appendix-additional-considerations.html#appendix-notebook-and-internet-access Notebook Instances Are Internet-Enabled by Default>.
-- You can set the value of this parameter to @Disabled@ only if you set a
-- value for the @SubnetId@ parameter.
createNotebookInstance_directInternetAccess :: Lens.Lens' CreateNotebookInstance (Prelude.Maybe DirectInternetAccess)
createNotebookInstance_directInternetAccess = Lens.lens (\CreateNotebookInstance' {directInternetAccess} -> directInternetAccess) (\s@CreateNotebookInstance' {} a -> s {directInternetAccess = a} :: CreateNotebookInstance)

-- | An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
createNotebookInstance_tags :: Lens.Lens' CreateNotebookInstance (Prelude.Maybe [Tag])
createNotebookInstance_tags = Lens.lens (\CreateNotebookInstance' {tags} -> tags) (\s@CreateNotebookInstance' {} a -> s {tags = a} :: CreateNotebookInstance) Prelude.. Lens.mapping Lens.coerced

-- | The name of the new notebook instance.
createNotebookInstance_notebookInstanceName :: Lens.Lens' CreateNotebookInstance Prelude.Text
createNotebookInstance_notebookInstanceName = Lens.lens (\CreateNotebookInstance' {notebookInstanceName} -> notebookInstanceName) (\s@CreateNotebookInstance' {} a -> s {notebookInstanceName = a} :: CreateNotebookInstance)

-- | The type of ML compute instance to launch for the notebook instance.
createNotebookInstance_instanceType :: Lens.Lens' CreateNotebookInstance InstanceType
createNotebookInstance_instanceType = Lens.lens (\CreateNotebookInstance' {instanceType} -> instanceType) (\s@CreateNotebookInstance' {} a -> s {instanceType = a} :: CreateNotebookInstance)

-- | When you send any requests to Amazon Web Services resources from the
-- notebook instance, Amazon SageMaker assumes this role to perform tasks
-- on your behalf. You must grant this role necessary permissions so Amazon
-- SageMaker can perform these tasks. The policy must allow the Amazon
-- SageMaker service principal (sagemaker.amazonaws.com) permissions to
-- assume this role. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles>.
--
-- To be able to pass this role to Amazon SageMaker, the caller of this API
-- must have the @iam:PassRole@ permission.
createNotebookInstance_roleArn :: Lens.Lens' CreateNotebookInstance Prelude.Text
createNotebookInstance_roleArn = Lens.lens (\CreateNotebookInstance' {roleArn} -> roleArn) (\s@CreateNotebookInstance' {} a -> s {roleArn = a} :: CreateNotebookInstance)

instance Core.AWSRequest CreateNotebookInstance where
  type
    AWSResponse CreateNotebookInstance =
      CreateNotebookInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNotebookInstanceResponse'
            Prelude.<$> (x Core..?> "NotebookInstanceArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNotebookInstance

instance Prelude.NFData CreateNotebookInstance

instance Core.ToHeaders CreateNotebookInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.CreateNotebookInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateNotebookInstance where
  toJSON CreateNotebookInstance' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("AcceleratorTypes" Core..=)
              Prelude.<$> acceleratorTypes,
            ("PlatformIdentifier" Core..=)
              Prelude.<$> platformIdentifier,
            ("SecurityGroupIds" Core..=)
              Prelude.<$> securityGroupIds,
            ("AdditionalCodeRepositories" Core..=)
              Prelude.<$> additionalCodeRepositories,
            ("LifecycleConfigName" Core..=)
              Prelude.<$> lifecycleConfigName,
            ("SubnetId" Core..=) Prelude.<$> subnetId,
            ("DefaultCodeRepository" Core..=)
              Prelude.<$> defaultCodeRepository,
            ("VolumeSizeInGB" Core..=)
              Prelude.<$> volumeSizeInGB,
            ("KmsKeyId" Core..=) Prelude.<$> kmsKeyId,
            ("RootAccess" Core..=) Prelude.<$> rootAccess,
            ("DirectInternetAccess" Core..=)
              Prelude.<$> directInternetAccess,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ( "NotebookInstanceName"
                  Core..= notebookInstanceName
              ),
            Prelude.Just ("InstanceType" Core..= instanceType),
            Prelude.Just ("RoleArn" Core..= roleArn)
          ]
      )

instance Core.ToPath CreateNotebookInstance where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateNotebookInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateNotebookInstanceResponse' smart constructor.
data CreateNotebookInstanceResponse = CreateNotebookInstanceResponse'
  { -- | The Amazon Resource Name (ARN) of the notebook instance.
    notebookInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNotebookInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookInstanceArn', 'createNotebookInstanceResponse_notebookInstanceArn' - The Amazon Resource Name (ARN) of the notebook instance.
--
-- 'httpStatus', 'createNotebookInstanceResponse_httpStatus' - The response's http status code.
newCreateNotebookInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateNotebookInstanceResponse
newCreateNotebookInstanceResponse pHttpStatus_ =
  CreateNotebookInstanceResponse'
    { notebookInstanceArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the notebook instance.
createNotebookInstanceResponse_notebookInstanceArn :: Lens.Lens' CreateNotebookInstanceResponse (Prelude.Maybe Prelude.Text)
createNotebookInstanceResponse_notebookInstanceArn = Lens.lens (\CreateNotebookInstanceResponse' {notebookInstanceArn} -> notebookInstanceArn) (\s@CreateNotebookInstanceResponse' {} a -> s {notebookInstanceArn = a} :: CreateNotebookInstanceResponse)

-- | The response's http status code.
createNotebookInstanceResponse_httpStatus :: Lens.Lens' CreateNotebookInstanceResponse Prelude.Int
createNotebookInstanceResponse_httpStatus = Lens.lens (\CreateNotebookInstanceResponse' {httpStatus} -> httpStatus) (\s@CreateNotebookInstanceResponse' {} a -> s {httpStatus = a} :: CreateNotebookInstanceResponse)

instance
  Prelude.NFData
    CreateNotebookInstanceResponse
