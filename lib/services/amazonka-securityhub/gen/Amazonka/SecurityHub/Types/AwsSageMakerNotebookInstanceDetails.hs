{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SecurityHub.Types.AwsSageMakerNotebookInstanceDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsSageMakerNotebookInstanceDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsSageMakerNotebookInstanceMetadataServiceConfigurationDetails

-- | Provides details about an Amazon SageMaker notebook instance.
--
-- /See:/ 'newAwsSageMakerNotebookInstanceDetails' smart constructor.
data AwsSageMakerNotebookInstanceDetails = AwsSageMakerNotebookInstanceDetails'
  { -- | A list of Amazon Elastic Inference instance types to associate with the
    -- notebook instance. Currently, only one instance type can be associated
    -- with a notebook instance.
    acceleratorTypes :: Prelude.Maybe [Prelude.Text],
    -- | An array of up to three Git repositories associated with the notebook
    -- instance. These can be either the names of Git repositories stored as
    -- resources in your account, or the URL of Git repositories in
    -- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit>
    -- or in any other Git repository. These repositories are cloned at the
    -- same level as the default repository of your notebook instance. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git repositories with SageMaker notebook instances>
    -- in the /Amazon SageMaker Developer Guide/.
    additionalCodeRepositories :: Prelude.Maybe [Prelude.Text],
    -- | The Git repository associated with the notebook instance as its default
    -- code repository. This can be either the name of a Git repository stored
    -- as a resource in your account, or the URL of a Git repository in
    -- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit>
    -- or in any other Git repository. When you open a notebook instance, it
    -- opens in the directory that contains this repository. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git repositories with SageMaker notebook instances>
    -- in the /Amazon SageMaker Developer Guide/.
    defaultCodeRepository :: Prelude.Maybe Prelude.Text,
    -- | Sets whether SageMaker provides internet access to the notebook
    -- instance. If you set this to @Disabled@, this notebook instance is able
    -- to access resources only in your VPC, and is not be able to connect to
    -- SageMaker training and endpoint services unless you configure a Network
    -- Address Translation (NAT) Gateway in your VPC.
    directInternetAccess :: Prelude.Maybe Prelude.Text,
    -- | If status of the instance is @Failed@, the reason it failed.
    failureReason :: Prelude.Maybe Prelude.Text,
    -- | Information on the IMDS configuration of the notebook instance.
    instanceMetadataServiceConfiguration :: Prelude.Maybe AwsSageMakerNotebookInstanceMetadataServiceConfigurationDetails,
    -- | The type of machine learning (ML) compute instance to launch for the
    -- notebook instance.
    instanceType :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an Key Management Service (KMS) key
    -- that SageMaker uses to encrypt data on the storage volume attached to
    -- your notebook instance. The KMS key you provide must be enabled. For
    -- information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/enabling-keys.html Enabling and disabling keys>
    -- in the /Key Management Service Developer Guide/.
    kmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The network interface ID that SageMaker created when the instance was
    -- created.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the notebook instance.
    notebookInstanceArn :: Prelude.Maybe Prelude.Text,
    -- | The name of a notebook instance lifecycle configuration.
    notebookInstanceLifecycleConfigName :: Prelude.Maybe Prelude.Text,
    -- | The name of the new notebook instance.
    notebookInstanceName :: Prelude.Maybe Prelude.Text,
    -- | The status of the notebook instance.
    notebookInstanceStatus :: Prelude.Maybe Prelude.Text,
    -- | The platform identifier of the notebook instance runtime environment.
    platformIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role associated with the
    -- instance.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Whether root access is enabled or disabled for users of the notebook
    -- instance.
    rootAccess :: Prelude.Maybe Prelude.Text,
    -- | The VPC security group IDs.
    securityGroups :: Prelude.Maybe [Prelude.Text],
    -- | The ID of the VPC subnet to which you have a connectivity from your ML
    -- compute instance.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The URL that you use to connect to the Jupyter notebook that is running
    -- in your notebook instance.
    url :: Prelude.Maybe Prelude.Text,
    -- | The size, in GB, of the ML storage volume to attach to the notebook
    -- instance.
    volumeSizeInGB :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsSageMakerNotebookInstanceDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorTypes', 'awsSageMakerNotebookInstanceDetails_acceleratorTypes' - A list of Amazon Elastic Inference instance types to associate with the
-- notebook instance. Currently, only one instance type can be associated
-- with a notebook instance.
--
-- 'additionalCodeRepositories', 'awsSageMakerNotebookInstanceDetails_additionalCodeRepositories' - An array of up to three Git repositories associated with the notebook
-- instance. These can be either the names of Git repositories stored as
-- resources in your account, or the URL of Git repositories in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit>
-- or in any other Git repository. These repositories are cloned at the
-- same level as the default repository of your notebook instance. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git repositories with SageMaker notebook instances>
-- in the /Amazon SageMaker Developer Guide/.
--
-- 'defaultCodeRepository', 'awsSageMakerNotebookInstanceDetails_defaultCodeRepository' - The Git repository associated with the notebook instance as its default
-- code repository. This can be either the name of a Git repository stored
-- as a resource in your account, or the URL of a Git repository in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit>
-- or in any other Git repository. When you open a notebook instance, it
-- opens in the directory that contains this repository. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git repositories with SageMaker notebook instances>
-- in the /Amazon SageMaker Developer Guide/.
--
-- 'directInternetAccess', 'awsSageMakerNotebookInstanceDetails_directInternetAccess' - Sets whether SageMaker provides internet access to the notebook
-- instance. If you set this to @Disabled@, this notebook instance is able
-- to access resources only in your VPC, and is not be able to connect to
-- SageMaker training and endpoint services unless you configure a Network
-- Address Translation (NAT) Gateway in your VPC.
--
-- 'failureReason', 'awsSageMakerNotebookInstanceDetails_failureReason' - If status of the instance is @Failed@, the reason it failed.
--
-- 'instanceMetadataServiceConfiguration', 'awsSageMakerNotebookInstanceDetails_instanceMetadataServiceConfiguration' - Information on the IMDS configuration of the notebook instance.
--
-- 'instanceType', 'awsSageMakerNotebookInstanceDetails_instanceType' - The type of machine learning (ML) compute instance to launch for the
-- notebook instance.
--
-- 'kmsKeyId', 'awsSageMakerNotebookInstanceDetails_kmsKeyId' - The Amazon Resource Name (ARN) of an Key Management Service (KMS) key
-- that SageMaker uses to encrypt data on the storage volume attached to
-- your notebook instance. The KMS key you provide must be enabled. For
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/enabling-keys.html Enabling and disabling keys>
-- in the /Key Management Service Developer Guide/.
--
-- 'networkInterfaceId', 'awsSageMakerNotebookInstanceDetails_networkInterfaceId' - The network interface ID that SageMaker created when the instance was
-- created.
--
-- 'notebookInstanceArn', 'awsSageMakerNotebookInstanceDetails_notebookInstanceArn' - The Amazon Resource Name (ARN) of the notebook instance.
--
-- 'notebookInstanceLifecycleConfigName', 'awsSageMakerNotebookInstanceDetails_notebookInstanceLifecycleConfigName' - The name of a notebook instance lifecycle configuration.
--
-- 'notebookInstanceName', 'awsSageMakerNotebookInstanceDetails_notebookInstanceName' - The name of the new notebook instance.
--
-- 'notebookInstanceStatus', 'awsSageMakerNotebookInstanceDetails_notebookInstanceStatus' - The status of the notebook instance.
--
-- 'platformIdentifier', 'awsSageMakerNotebookInstanceDetails_platformIdentifier' - The platform identifier of the notebook instance runtime environment.
--
-- 'roleArn', 'awsSageMakerNotebookInstanceDetails_roleArn' - The Amazon Resource Name (ARN) of the IAM role associated with the
-- instance.
--
-- 'rootAccess', 'awsSageMakerNotebookInstanceDetails_rootAccess' - Whether root access is enabled or disabled for users of the notebook
-- instance.
--
-- 'securityGroups', 'awsSageMakerNotebookInstanceDetails_securityGroups' - The VPC security group IDs.
--
-- 'subnetId', 'awsSageMakerNotebookInstanceDetails_subnetId' - The ID of the VPC subnet to which you have a connectivity from your ML
-- compute instance.
--
-- 'url', 'awsSageMakerNotebookInstanceDetails_url' - The URL that you use to connect to the Jupyter notebook that is running
-- in your notebook instance.
--
-- 'volumeSizeInGB', 'awsSageMakerNotebookInstanceDetails_volumeSizeInGB' - The size, in GB, of the ML storage volume to attach to the notebook
-- instance.
newAwsSageMakerNotebookInstanceDetails ::
  AwsSageMakerNotebookInstanceDetails
newAwsSageMakerNotebookInstanceDetails =
  AwsSageMakerNotebookInstanceDetails'
    { acceleratorTypes =
        Prelude.Nothing,
      additionalCodeRepositories =
        Prelude.Nothing,
      defaultCodeRepository =
        Prelude.Nothing,
      directInternetAccess = Prelude.Nothing,
      failureReason = Prelude.Nothing,
      instanceMetadataServiceConfiguration =
        Prelude.Nothing,
      instanceType = Prelude.Nothing,
      kmsKeyId = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      notebookInstanceArn = Prelude.Nothing,
      notebookInstanceLifecycleConfigName =
        Prelude.Nothing,
      notebookInstanceName = Prelude.Nothing,
      notebookInstanceStatus =
        Prelude.Nothing,
      platformIdentifier = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      rootAccess = Prelude.Nothing,
      securityGroups = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      url = Prelude.Nothing,
      volumeSizeInGB = Prelude.Nothing
    }

-- | A list of Amazon Elastic Inference instance types to associate with the
-- notebook instance. Currently, only one instance type can be associated
-- with a notebook instance.
awsSageMakerNotebookInstanceDetails_acceleratorTypes :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe [Prelude.Text])
awsSageMakerNotebookInstanceDetails_acceleratorTypes = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {acceleratorTypes} -> acceleratorTypes) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {acceleratorTypes = a} :: AwsSageMakerNotebookInstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | An array of up to three Git repositories associated with the notebook
-- instance. These can be either the names of Git repositories stored as
-- resources in your account, or the URL of Git repositories in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit>
-- or in any other Git repository. These repositories are cloned at the
-- same level as the default repository of your notebook instance. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git repositories with SageMaker notebook instances>
-- in the /Amazon SageMaker Developer Guide/.
awsSageMakerNotebookInstanceDetails_additionalCodeRepositories :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe [Prelude.Text])
awsSageMakerNotebookInstanceDetails_additionalCodeRepositories = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {additionalCodeRepositories} -> additionalCodeRepositories) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {additionalCodeRepositories = a} :: AwsSageMakerNotebookInstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The Git repository associated with the notebook instance as its default
-- code repository. This can be either the name of a Git repository stored
-- as a resource in your account, or the URL of a Git repository in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit>
-- or in any other Git repository. When you open a notebook instance, it
-- opens in the directory that contains this repository. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git repositories with SageMaker notebook instances>
-- in the /Amazon SageMaker Developer Guide/.
awsSageMakerNotebookInstanceDetails_defaultCodeRepository :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe Prelude.Text)
awsSageMakerNotebookInstanceDetails_defaultCodeRepository = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {defaultCodeRepository} -> defaultCodeRepository) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {defaultCodeRepository = a} :: AwsSageMakerNotebookInstanceDetails)

-- | Sets whether SageMaker provides internet access to the notebook
-- instance. If you set this to @Disabled@, this notebook instance is able
-- to access resources only in your VPC, and is not be able to connect to
-- SageMaker training and endpoint services unless you configure a Network
-- Address Translation (NAT) Gateway in your VPC.
awsSageMakerNotebookInstanceDetails_directInternetAccess :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe Prelude.Text)
awsSageMakerNotebookInstanceDetails_directInternetAccess = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {directInternetAccess} -> directInternetAccess) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {directInternetAccess = a} :: AwsSageMakerNotebookInstanceDetails)

-- | If status of the instance is @Failed@, the reason it failed.
awsSageMakerNotebookInstanceDetails_failureReason :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe Prelude.Text)
awsSageMakerNotebookInstanceDetails_failureReason = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {failureReason} -> failureReason) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {failureReason = a} :: AwsSageMakerNotebookInstanceDetails)

-- | Information on the IMDS configuration of the notebook instance.
awsSageMakerNotebookInstanceDetails_instanceMetadataServiceConfiguration :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe AwsSageMakerNotebookInstanceMetadataServiceConfigurationDetails)
awsSageMakerNotebookInstanceDetails_instanceMetadataServiceConfiguration = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {instanceMetadataServiceConfiguration} -> instanceMetadataServiceConfiguration) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {instanceMetadataServiceConfiguration = a} :: AwsSageMakerNotebookInstanceDetails)

-- | The type of machine learning (ML) compute instance to launch for the
-- notebook instance.
awsSageMakerNotebookInstanceDetails_instanceType :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe Prelude.Text)
awsSageMakerNotebookInstanceDetails_instanceType = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {instanceType} -> instanceType) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {instanceType = a} :: AwsSageMakerNotebookInstanceDetails)

-- | The Amazon Resource Name (ARN) of an Key Management Service (KMS) key
-- that SageMaker uses to encrypt data on the storage volume attached to
-- your notebook instance. The KMS key you provide must be enabled. For
-- information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/enabling-keys.html Enabling and disabling keys>
-- in the /Key Management Service Developer Guide/.
awsSageMakerNotebookInstanceDetails_kmsKeyId :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe Prelude.Text)
awsSageMakerNotebookInstanceDetails_kmsKeyId = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {kmsKeyId} -> kmsKeyId) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {kmsKeyId = a} :: AwsSageMakerNotebookInstanceDetails)

-- | The network interface ID that SageMaker created when the instance was
-- created.
awsSageMakerNotebookInstanceDetails_networkInterfaceId :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe Prelude.Text)
awsSageMakerNotebookInstanceDetails_networkInterfaceId = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {networkInterfaceId} -> networkInterfaceId) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {networkInterfaceId = a} :: AwsSageMakerNotebookInstanceDetails)

-- | The Amazon Resource Name (ARN) of the notebook instance.
awsSageMakerNotebookInstanceDetails_notebookInstanceArn :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe Prelude.Text)
awsSageMakerNotebookInstanceDetails_notebookInstanceArn = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {notebookInstanceArn} -> notebookInstanceArn) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {notebookInstanceArn = a} :: AwsSageMakerNotebookInstanceDetails)

-- | The name of a notebook instance lifecycle configuration.
awsSageMakerNotebookInstanceDetails_notebookInstanceLifecycleConfigName :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe Prelude.Text)
awsSageMakerNotebookInstanceDetails_notebookInstanceLifecycleConfigName = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {notebookInstanceLifecycleConfigName} -> notebookInstanceLifecycleConfigName) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {notebookInstanceLifecycleConfigName = a} :: AwsSageMakerNotebookInstanceDetails)

-- | The name of the new notebook instance.
awsSageMakerNotebookInstanceDetails_notebookInstanceName :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe Prelude.Text)
awsSageMakerNotebookInstanceDetails_notebookInstanceName = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {notebookInstanceName} -> notebookInstanceName) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {notebookInstanceName = a} :: AwsSageMakerNotebookInstanceDetails)

-- | The status of the notebook instance.
awsSageMakerNotebookInstanceDetails_notebookInstanceStatus :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe Prelude.Text)
awsSageMakerNotebookInstanceDetails_notebookInstanceStatus = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {notebookInstanceStatus} -> notebookInstanceStatus) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {notebookInstanceStatus = a} :: AwsSageMakerNotebookInstanceDetails)

-- | The platform identifier of the notebook instance runtime environment.
awsSageMakerNotebookInstanceDetails_platformIdentifier :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe Prelude.Text)
awsSageMakerNotebookInstanceDetails_platformIdentifier = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {platformIdentifier} -> platformIdentifier) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {platformIdentifier = a} :: AwsSageMakerNotebookInstanceDetails)

-- | The Amazon Resource Name (ARN) of the IAM role associated with the
-- instance.
awsSageMakerNotebookInstanceDetails_roleArn :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe Prelude.Text)
awsSageMakerNotebookInstanceDetails_roleArn = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {roleArn} -> roleArn) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {roleArn = a} :: AwsSageMakerNotebookInstanceDetails)

-- | Whether root access is enabled or disabled for users of the notebook
-- instance.
awsSageMakerNotebookInstanceDetails_rootAccess :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe Prelude.Text)
awsSageMakerNotebookInstanceDetails_rootAccess = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {rootAccess} -> rootAccess) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {rootAccess = a} :: AwsSageMakerNotebookInstanceDetails)

-- | The VPC security group IDs.
awsSageMakerNotebookInstanceDetails_securityGroups :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe [Prelude.Text])
awsSageMakerNotebookInstanceDetails_securityGroups = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {securityGroups} -> securityGroups) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {securityGroups = a} :: AwsSageMakerNotebookInstanceDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC subnet to which you have a connectivity from your ML
-- compute instance.
awsSageMakerNotebookInstanceDetails_subnetId :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe Prelude.Text)
awsSageMakerNotebookInstanceDetails_subnetId = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {subnetId} -> subnetId) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {subnetId = a} :: AwsSageMakerNotebookInstanceDetails)

-- | The URL that you use to connect to the Jupyter notebook that is running
-- in your notebook instance.
awsSageMakerNotebookInstanceDetails_url :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe Prelude.Text)
awsSageMakerNotebookInstanceDetails_url = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {url} -> url) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {url = a} :: AwsSageMakerNotebookInstanceDetails)

-- | The size, in GB, of the ML storage volume to attach to the notebook
-- instance.
awsSageMakerNotebookInstanceDetails_volumeSizeInGB :: Lens.Lens' AwsSageMakerNotebookInstanceDetails (Prelude.Maybe Prelude.Int)
awsSageMakerNotebookInstanceDetails_volumeSizeInGB = Lens.lens (\AwsSageMakerNotebookInstanceDetails' {volumeSizeInGB} -> volumeSizeInGB) (\s@AwsSageMakerNotebookInstanceDetails' {} a -> s {volumeSizeInGB = a} :: AwsSageMakerNotebookInstanceDetails)

instance
  Data.FromJSON
    AwsSageMakerNotebookInstanceDetails
  where
  parseJSON =
    Data.withObject
      "AwsSageMakerNotebookInstanceDetails"
      ( \x ->
          AwsSageMakerNotebookInstanceDetails'
            Prelude.<$> ( x
                            Data..:? "AcceleratorTypes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> ( x
                            Data..:? "AdditionalCodeRepositories"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "DefaultCodeRepository")
            Prelude.<*> (x Data..:? "DirectInternetAccess")
            Prelude.<*> (x Data..:? "FailureReason")
            Prelude.<*> (x Data..:? "InstanceMetadataServiceConfiguration")
            Prelude.<*> (x Data..:? "InstanceType")
            Prelude.<*> (x Data..:? "KmsKeyId")
            Prelude.<*> (x Data..:? "NetworkInterfaceId")
            Prelude.<*> (x Data..:? "NotebookInstanceArn")
            Prelude.<*> (x Data..:? "NotebookInstanceLifecycleConfigName")
            Prelude.<*> (x Data..:? "NotebookInstanceName")
            Prelude.<*> (x Data..:? "NotebookInstanceStatus")
            Prelude.<*> (x Data..:? "PlatformIdentifier")
            Prelude.<*> (x Data..:? "RoleArn")
            Prelude.<*> (x Data..:? "RootAccess")
            Prelude.<*> (x Data..:? "SecurityGroups" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "SubnetId")
            Prelude.<*> (x Data..:? "Url")
            Prelude.<*> (x Data..:? "VolumeSizeInGB")
      )

instance
  Prelude.Hashable
    AwsSageMakerNotebookInstanceDetails
  where
  hashWithSalt
    _salt
    AwsSageMakerNotebookInstanceDetails' {..} =
      _salt
        `Prelude.hashWithSalt` acceleratorTypes
        `Prelude.hashWithSalt` additionalCodeRepositories
        `Prelude.hashWithSalt` defaultCodeRepository
        `Prelude.hashWithSalt` directInternetAccess
        `Prelude.hashWithSalt` failureReason
        `Prelude.hashWithSalt` instanceMetadataServiceConfiguration
        `Prelude.hashWithSalt` instanceType
        `Prelude.hashWithSalt` kmsKeyId
        `Prelude.hashWithSalt` networkInterfaceId
        `Prelude.hashWithSalt` notebookInstanceArn
        `Prelude.hashWithSalt` notebookInstanceLifecycleConfigName
        `Prelude.hashWithSalt` notebookInstanceName
        `Prelude.hashWithSalt` notebookInstanceStatus
        `Prelude.hashWithSalt` platformIdentifier
        `Prelude.hashWithSalt` roleArn
        `Prelude.hashWithSalt` rootAccess
        `Prelude.hashWithSalt` securityGroups
        `Prelude.hashWithSalt` subnetId
        `Prelude.hashWithSalt` url
        `Prelude.hashWithSalt` volumeSizeInGB

instance
  Prelude.NFData
    AwsSageMakerNotebookInstanceDetails
  where
  rnf AwsSageMakerNotebookInstanceDetails' {..} =
    Prelude.rnf acceleratorTypes
      `Prelude.seq` Prelude.rnf additionalCodeRepositories
      `Prelude.seq` Prelude.rnf defaultCodeRepository
      `Prelude.seq` Prelude.rnf directInternetAccess
      `Prelude.seq` Prelude.rnf failureReason
      `Prelude.seq` Prelude.rnf instanceMetadataServiceConfiguration
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf kmsKeyId
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf notebookInstanceArn
      `Prelude.seq` Prelude.rnf notebookInstanceLifecycleConfigName
      `Prelude.seq` Prelude.rnf notebookInstanceName
      `Prelude.seq` Prelude.rnf notebookInstanceStatus
      `Prelude.seq` Prelude.rnf platformIdentifier
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf rootAccess
      `Prelude.seq` Prelude.rnf securityGroups
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf volumeSizeInGB

instance
  Data.ToJSON
    AwsSageMakerNotebookInstanceDetails
  where
  toJSON AwsSageMakerNotebookInstanceDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceleratorTypes" Data..=)
              Prelude.<$> acceleratorTypes,
            ("AdditionalCodeRepositories" Data..=)
              Prelude.<$> additionalCodeRepositories,
            ("DefaultCodeRepository" Data..=)
              Prelude.<$> defaultCodeRepository,
            ("DirectInternetAccess" Data..=)
              Prelude.<$> directInternetAccess,
            ("FailureReason" Data..=) Prelude.<$> failureReason,
            ("InstanceMetadataServiceConfiguration" Data..=)
              Prelude.<$> instanceMetadataServiceConfiguration,
            ("InstanceType" Data..=) Prelude.<$> instanceType,
            ("KmsKeyId" Data..=) Prelude.<$> kmsKeyId,
            ("NetworkInterfaceId" Data..=)
              Prelude.<$> networkInterfaceId,
            ("NotebookInstanceArn" Data..=)
              Prelude.<$> notebookInstanceArn,
            ("NotebookInstanceLifecycleConfigName" Data..=)
              Prelude.<$> notebookInstanceLifecycleConfigName,
            ("NotebookInstanceName" Data..=)
              Prelude.<$> notebookInstanceName,
            ("NotebookInstanceStatus" Data..=)
              Prelude.<$> notebookInstanceStatus,
            ("PlatformIdentifier" Data..=)
              Prelude.<$> platformIdentifier,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("RootAccess" Data..=) Prelude.<$> rootAccess,
            ("SecurityGroups" Data..=)
              Prelude.<$> securityGroups,
            ("SubnetId" Data..=) Prelude.<$> subnetId,
            ("Url" Data..=) Prelude.<$> url,
            ("VolumeSizeInGB" Data..=)
              Prelude.<$> volumeSizeInGB
          ]
      )
