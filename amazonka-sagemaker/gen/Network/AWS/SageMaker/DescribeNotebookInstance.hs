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
-- Module      : Network.AWS.SageMaker.DescribeNotebookInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a notebook instance.
module Network.AWS.SageMaker.DescribeNotebookInstance
  ( -- * Creating a Request
    DescribeNotebookInstance (..),
    newDescribeNotebookInstance,

    -- * Request Lenses
    describeNotebookInstance_notebookInstanceName,

    -- * Destructuring the Response
    DescribeNotebookInstanceResponse (..),
    newDescribeNotebookInstanceResponse,

    -- * Response Lenses
    describeNotebookInstanceResponse_notebookInstanceName,
    describeNotebookInstanceResponse_creationTime,
    describeNotebookInstanceResponse_acceleratorTypes,
    describeNotebookInstanceResponse_defaultCodeRepository,
    describeNotebookInstanceResponse_roleArn,
    describeNotebookInstanceResponse_instanceType,
    describeNotebookInstanceResponse_notebookInstanceLifecycleConfigName,
    describeNotebookInstanceResponse_additionalCodeRepositories,
    describeNotebookInstanceResponse_securityGroups,
    describeNotebookInstanceResponse_kmsKeyId,
    describeNotebookInstanceResponse_volumeSizeInGB,
    describeNotebookInstanceResponse_notebookInstanceStatus,
    describeNotebookInstanceResponse_failureReason,
    describeNotebookInstanceResponse_networkInterfaceId,
    describeNotebookInstanceResponse_lastModifiedTime,
    describeNotebookInstanceResponse_subnetId,
    describeNotebookInstanceResponse_url,
    describeNotebookInstanceResponse_notebookInstanceArn,
    describeNotebookInstanceResponse_directInternetAccess,
    describeNotebookInstanceResponse_rootAccess,
    describeNotebookInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeNotebookInstance' smart constructor.
data DescribeNotebookInstance = DescribeNotebookInstance'
  { -- | The name of the notebook instance that you want information about.
    notebookInstanceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNotebookInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookInstanceName', 'describeNotebookInstance_notebookInstanceName' - The name of the notebook instance that you want information about.
newDescribeNotebookInstance ::
  -- | 'notebookInstanceName'
  Core.Text ->
  DescribeNotebookInstance
newDescribeNotebookInstance pNotebookInstanceName_ =
  DescribeNotebookInstance'
    { notebookInstanceName =
        pNotebookInstanceName_
    }

-- | The name of the notebook instance that you want information about.
describeNotebookInstance_notebookInstanceName :: Lens.Lens' DescribeNotebookInstance Core.Text
describeNotebookInstance_notebookInstanceName = Lens.lens (\DescribeNotebookInstance' {notebookInstanceName} -> notebookInstanceName) (\s@DescribeNotebookInstance' {} a -> s {notebookInstanceName = a} :: DescribeNotebookInstance)

instance Core.AWSRequest DescribeNotebookInstance where
  type
    AWSResponse DescribeNotebookInstance =
      DescribeNotebookInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNotebookInstanceResponse'
            Core.<$> (x Core..?> "NotebookInstanceName")
            Core.<*> (x Core..?> "CreationTime")
            Core.<*> (x Core..?> "AcceleratorTypes" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "DefaultCodeRepository")
            Core.<*> (x Core..?> "RoleArn")
            Core.<*> (x Core..?> "InstanceType")
            Core.<*> (x Core..?> "NotebookInstanceLifecycleConfigName")
            Core.<*> ( x Core..?> "AdditionalCodeRepositories"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "SecurityGroups" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "KmsKeyId")
            Core.<*> (x Core..?> "VolumeSizeInGB")
            Core.<*> (x Core..?> "NotebookInstanceStatus")
            Core.<*> (x Core..?> "FailureReason")
            Core.<*> (x Core..?> "NetworkInterfaceId")
            Core.<*> (x Core..?> "LastModifiedTime")
            Core.<*> (x Core..?> "SubnetId")
            Core.<*> (x Core..?> "Url")
            Core.<*> (x Core..?> "NotebookInstanceArn")
            Core.<*> (x Core..?> "DirectInternetAccess")
            Core.<*> (x Core..?> "RootAccess")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeNotebookInstance

instance Core.NFData DescribeNotebookInstance

instance Core.ToHeaders DescribeNotebookInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeNotebookInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeNotebookInstance where
  toJSON DescribeNotebookInstance' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "NotebookInstanceName"
                  Core..= notebookInstanceName
              )
          ]
      )

instance Core.ToPath DescribeNotebookInstance where
  toPath = Core.const "/"

instance Core.ToQuery DescribeNotebookInstance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeNotebookInstanceResponse' smart constructor.
data DescribeNotebookInstanceResponse = DescribeNotebookInstanceResponse'
  { -- | The name of the Amazon SageMaker notebook instance.
    notebookInstanceName :: Core.Maybe Core.Text,
    -- | A timestamp. Use this parameter to return the time when the notebook
    -- instance was created
    creationTime :: Core.Maybe Core.POSIX,
    -- | A list of the Elastic Inference (EI) instance types associated with this
    -- notebook instance. Currently only one EI instance type can be associated
    -- with a notebook instance. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
    acceleratorTypes :: Core.Maybe [NotebookInstanceAcceleratorType],
    -- | The Git repository associated with the notebook instance as its default
    -- code repository. This can be either the name of a Git repository stored
    -- as a resource in your account, or the URL of a Git repository in
    -- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit>
    -- or in any other Git repository. When you open a notebook instance, it
    -- opens in the directory that contains this repository. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances>.
    defaultCodeRepository :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role associated with the
    -- instance.
    roleArn :: Core.Maybe Core.Text,
    -- | The type of ML compute instance running on the notebook instance.
    instanceType :: Core.Maybe InstanceType,
    -- | Returns the name of a notebook instance lifecycle configuration.
    --
    -- For information about notebook instance lifestyle configurations, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>
    notebookInstanceLifecycleConfigName :: Core.Maybe Core.Text,
    -- | An array of up to three Git repositories associated with the notebook
    -- instance. These can be either the names of Git repositories stored as
    -- resources in your account, or the URL of Git repositories in
    -- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit>
    -- or in any other Git repository. These repositories are cloned at the
    -- same level as the default repository of your notebook instance. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances>.
    additionalCodeRepositories :: Core.Maybe [Core.Text],
    -- | The IDs of the VPC security groups.
    securityGroups :: Core.Maybe [Core.Text],
    -- | The AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it
    -- on the ML storage volume attached to the instance.
    kmsKeyId :: Core.Maybe Core.Text,
    -- | The size, in GB, of the ML storage volume attached to the notebook
    -- instance.
    volumeSizeInGB :: Core.Maybe Core.Natural,
    -- | The status of the notebook instance.
    notebookInstanceStatus :: Core.Maybe NotebookInstanceStatus,
    -- | If status is @Failed@, the reason it failed.
    failureReason :: Core.Maybe Core.Text,
    -- | The network interface IDs that Amazon SageMaker created at the time of
    -- creating the instance.
    networkInterfaceId :: Core.Maybe Core.Text,
    -- | A timestamp. Use this parameter to retrieve the time when the notebook
    -- instance was last modified.
    lastModifiedTime :: Core.Maybe Core.POSIX,
    -- | The ID of the VPC subnet.
    subnetId :: Core.Maybe Core.Text,
    -- | The URL that you use to connect to the Jupyter notebook that is running
    -- in your notebook instance.
    url :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the notebook instance.
    notebookInstanceArn :: Core.Maybe Core.Text,
    -- | Describes whether Amazon SageMaker provides internet access to the
    -- notebook instance. If this value is set to /Disabled/, the notebook
    -- instance does not have internet access, and cannot connect to Amazon
    -- SageMaker training and endpoint services.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/appendix-additional-considerations.html#appendix-notebook-and-internet-access Notebook Instances Are Internet-Enabled by Default>.
    directInternetAccess :: Core.Maybe DirectInternetAccess,
    -- | Whether root access is enabled or disabled for users of the notebook
    -- instance.
    --
    -- Lifecycle configurations need root access to be able to set up a
    -- notebook instance. Because of this, lifecycle configurations associated
    -- with a notebook instance always run with root access even if you disable
    -- root access for users.
    rootAccess :: Core.Maybe RootAccess,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeNotebookInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'notebookInstanceName', 'describeNotebookInstanceResponse_notebookInstanceName' - The name of the Amazon SageMaker notebook instance.
--
-- 'creationTime', 'describeNotebookInstanceResponse_creationTime' - A timestamp. Use this parameter to return the time when the notebook
-- instance was created
--
-- 'acceleratorTypes', 'describeNotebookInstanceResponse_acceleratorTypes' - A list of the Elastic Inference (EI) instance types associated with this
-- notebook instance. Currently only one EI instance type can be associated
-- with a notebook instance. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
--
-- 'defaultCodeRepository', 'describeNotebookInstanceResponse_defaultCodeRepository' - The Git repository associated with the notebook instance as its default
-- code repository. This can be either the name of a Git repository stored
-- as a resource in your account, or the URL of a Git repository in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit>
-- or in any other Git repository. When you open a notebook instance, it
-- opens in the directory that contains this repository. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances>.
--
-- 'roleArn', 'describeNotebookInstanceResponse_roleArn' - The Amazon Resource Name (ARN) of the IAM role associated with the
-- instance.
--
-- 'instanceType', 'describeNotebookInstanceResponse_instanceType' - The type of ML compute instance running on the notebook instance.
--
-- 'notebookInstanceLifecycleConfigName', 'describeNotebookInstanceResponse_notebookInstanceLifecycleConfigName' - Returns the name of a notebook instance lifecycle configuration.
--
-- For information about notebook instance lifestyle configurations, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>
--
-- 'additionalCodeRepositories', 'describeNotebookInstanceResponse_additionalCodeRepositories' - An array of up to three Git repositories associated with the notebook
-- instance. These can be either the names of Git repositories stored as
-- resources in your account, or the URL of Git repositories in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit>
-- or in any other Git repository. These repositories are cloned at the
-- same level as the default repository of your notebook instance. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances>.
--
-- 'securityGroups', 'describeNotebookInstanceResponse_securityGroups' - The IDs of the VPC security groups.
--
-- 'kmsKeyId', 'describeNotebookInstanceResponse_kmsKeyId' - The AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it
-- on the ML storage volume attached to the instance.
--
-- 'volumeSizeInGB', 'describeNotebookInstanceResponse_volumeSizeInGB' - The size, in GB, of the ML storage volume attached to the notebook
-- instance.
--
-- 'notebookInstanceStatus', 'describeNotebookInstanceResponse_notebookInstanceStatus' - The status of the notebook instance.
--
-- 'failureReason', 'describeNotebookInstanceResponse_failureReason' - If status is @Failed@, the reason it failed.
--
-- 'networkInterfaceId', 'describeNotebookInstanceResponse_networkInterfaceId' - The network interface IDs that Amazon SageMaker created at the time of
-- creating the instance.
--
-- 'lastModifiedTime', 'describeNotebookInstanceResponse_lastModifiedTime' - A timestamp. Use this parameter to retrieve the time when the notebook
-- instance was last modified.
--
-- 'subnetId', 'describeNotebookInstanceResponse_subnetId' - The ID of the VPC subnet.
--
-- 'url', 'describeNotebookInstanceResponse_url' - The URL that you use to connect to the Jupyter notebook that is running
-- in your notebook instance.
--
-- 'notebookInstanceArn', 'describeNotebookInstanceResponse_notebookInstanceArn' - The Amazon Resource Name (ARN) of the notebook instance.
--
-- 'directInternetAccess', 'describeNotebookInstanceResponse_directInternetAccess' - Describes whether Amazon SageMaker provides internet access to the
-- notebook instance. If this value is set to /Disabled/, the notebook
-- instance does not have internet access, and cannot connect to Amazon
-- SageMaker training and endpoint services.
--
-- For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/appendix-additional-considerations.html#appendix-notebook-and-internet-access Notebook Instances Are Internet-Enabled by Default>.
--
-- 'rootAccess', 'describeNotebookInstanceResponse_rootAccess' - Whether root access is enabled or disabled for users of the notebook
-- instance.
--
-- Lifecycle configurations need root access to be able to set up a
-- notebook instance. Because of this, lifecycle configurations associated
-- with a notebook instance always run with root access even if you disable
-- root access for users.
--
-- 'httpStatus', 'describeNotebookInstanceResponse_httpStatus' - The response's http status code.
newDescribeNotebookInstanceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeNotebookInstanceResponse
newDescribeNotebookInstanceResponse pHttpStatus_ =
  DescribeNotebookInstanceResponse'
    { notebookInstanceName =
        Core.Nothing,
      creationTime = Core.Nothing,
      acceleratorTypes = Core.Nothing,
      defaultCodeRepository = Core.Nothing,
      roleArn = Core.Nothing,
      instanceType = Core.Nothing,
      notebookInstanceLifecycleConfigName =
        Core.Nothing,
      additionalCodeRepositories = Core.Nothing,
      securityGroups = Core.Nothing,
      kmsKeyId = Core.Nothing,
      volumeSizeInGB = Core.Nothing,
      notebookInstanceStatus = Core.Nothing,
      failureReason = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      lastModifiedTime = Core.Nothing,
      subnetId = Core.Nothing,
      url = Core.Nothing,
      notebookInstanceArn = Core.Nothing,
      directInternetAccess = Core.Nothing,
      rootAccess = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the Amazon SageMaker notebook instance.
describeNotebookInstanceResponse_notebookInstanceName :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Core.Text)
describeNotebookInstanceResponse_notebookInstanceName = Lens.lens (\DescribeNotebookInstanceResponse' {notebookInstanceName} -> notebookInstanceName) (\s@DescribeNotebookInstanceResponse' {} a -> s {notebookInstanceName = a} :: DescribeNotebookInstanceResponse)

-- | A timestamp. Use this parameter to return the time when the notebook
-- instance was created
describeNotebookInstanceResponse_creationTime :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Core.UTCTime)
describeNotebookInstanceResponse_creationTime = Lens.lens (\DescribeNotebookInstanceResponse' {creationTime} -> creationTime) (\s@DescribeNotebookInstanceResponse' {} a -> s {creationTime = a} :: DescribeNotebookInstanceResponse) Core.. Lens.mapping Core._Time

-- | A list of the Elastic Inference (EI) instance types associated with this
-- notebook instance. Currently only one EI instance type can be associated
-- with a notebook instance. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
describeNotebookInstanceResponse_acceleratorTypes :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe [NotebookInstanceAcceleratorType])
describeNotebookInstanceResponse_acceleratorTypes = Lens.lens (\DescribeNotebookInstanceResponse' {acceleratorTypes} -> acceleratorTypes) (\s@DescribeNotebookInstanceResponse' {} a -> s {acceleratorTypes = a} :: DescribeNotebookInstanceResponse) Core.. Lens.mapping Lens._Coerce

-- | The Git repository associated with the notebook instance as its default
-- code repository. This can be either the name of a Git repository stored
-- as a resource in your account, or the URL of a Git repository in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit>
-- or in any other Git repository. When you open a notebook instance, it
-- opens in the directory that contains this repository. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances>.
describeNotebookInstanceResponse_defaultCodeRepository :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Core.Text)
describeNotebookInstanceResponse_defaultCodeRepository = Lens.lens (\DescribeNotebookInstanceResponse' {defaultCodeRepository} -> defaultCodeRepository) (\s@DescribeNotebookInstanceResponse' {} a -> s {defaultCodeRepository = a} :: DescribeNotebookInstanceResponse)

-- | The Amazon Resource Name (ARN) of the IAM role associated with the
-- instance.
describeNotebookInstanceResponse_roleArn :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Core.Text)
describeNotebookInstanceResponse_roleArn = Lens.lens (\DescribeNotebookInstanceResponse' {roleArn} -> roleArn) (\s@DescribeNotebookInstanceResponse' {} a -> s {roleArn = a} :: DescribeNotebookInstanceResponse)

-- | The type of ML compute instance running on the notebook instance.
describeNotebookInstanceResponse_instanceType :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe InstanceType)
describeNotebookInstanceResponse_instanceType = Lens.lens (\DescribeNotebookInstanceResponse' {instanceType} -> instanceType) (\s@DescribeNotebookInstanceResponse' {} a -> s {instanceType = a} :: DescribeNotebookInstanceResponse)

-- | Returns the name of a notebook instance lifecycle configuration.
--
-- For information about notebook instance lifestyle configurations, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>
describeNotebookInstanceResponse_notebookInstanceLifecycleConfigName :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Core.Text)
describeNotebookInstanceResponse_notebookInstanceLifecycleConfigName = Lens.lens (\DescribeNotebookInstanceResponse' {notebookInstanceLifecycleConfigName} -> notebookInstanceLifecycleConfigName) (\s@DescribeNotebookInstanceResponse' {} a -> s {notebookInstanceLifecycleConfigName = a} :: DescribeNotebookInstanceResponse)

-- | An array of up to three Git repositories associated with the notebook
-- instance. These can be either the names of Git repositories stored as
-- resources in your account, or the URL of Git repositories in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit>
-- or in any other Git repository. These repositories are cloned at the
-- same level as the default repository of your notebook instance. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances>.
describeNotebookInstanceResponse_additionalCodeRepositories :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe [Core.Text])
describeNotebookInstanceResponse_additionalCodeRepositories = Lens.lens (\DescribeNotebookInstanceResponse' {additionalCodeRepositories} -> additionalCodeRepositories) (\s@DescribeNotebookInstanceResponse' {} a -> s {additionalCodeRepositories = a} :: DescribeNotebookInstanceResponse) Core.. Lens.mapping Lens._Coerce

-- | The IDs of the VPC security groups.
describeNotebookInstanceResponse_securityGroups :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe [Core.Text])
describeNotebookInstanceResponse_securityGroups = Lens.lens (\DescribeNotebookInstanceResponse' {securityGroups} -> securityGroups) (\s@DescribeNotebookInstanceResponse' {} a -> s {securityGroups = a} :: DescribeNotebookInstanceResponse) Core.. Lens.mapping Lens._Coerce

-- | The AWS KMS key ID Amazon SageMaker uses to encrypt data when storing it
-- on the ML storage volume attached to the instance.
describeNotebookInstanceResponse_kmsKeyId :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Core.Text)
describeNotebookInstanceResponse_kmsKeyId = Lens.lens (\DescribeNotebookInstanceResponse' {kmsKeyId} -> kmsKeyId) (\s@DescribeNotebookInstanceResponse' {} a -> s {kmsKeyId = a} :: DescribeNotebookInstanceResponse)

-- | The size, in GB, of the ML storage volume attached to the notebook
-- instance.
describeNotebookInstanceResponse_volumeSizeInGB :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Core.Natural)
describeNotebookInstanceResponse_volumeSizeInGB = Lens.lens (\DescribeNotebookInstanceResponse' {volumeSizeInGB} -> volumeSizeInGB) (\s@DescribeNotebookInstanceResponse' {} a -> s {volumeSizeInGB = a} :: DescribeNotebookInstanceResponse)

-- | The status of the notebook instance.
describeNotebookInstanceResponse_notebookInstanceStatus :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe NotebookInstanceStatus)
describeNotebookInstanceResponse_notebookInstanceStatus = Lens.lens (\DescribeNotebookInstanceResponse' {notebookInstanceStatus} -> notebookInstanceStatus) (\s@DescribeNotebookInstanceResponse' {} a -> s {notebookInstanceStatus = a} :: DescribeNotebookInstanceResponse)

-- | If status is @Failed@, the reason it failed.
describeNotebookInstanceResponse_failureReason :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Core.Text)
describeNotebookInstanceResponse_failureReason = Lens.lens (\DescribeNotebookInstanceResponse' {failureReason} -> failureReason) (\s@DescribeNotebookInstanceResponse' {} a -> s {failureReason = a} :: DescribeNotebookInstanceResponse)

-- | The network interface IDs that Amazon SageMaker created at the time of
-- creating the instance.
describeNotebookInstanceResponse_networkInterfaceId :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Core.Text)
describeNotebookInstanceResponse_networkInterfaceId = Lens.lens (\DescribeNotebookInstanceResponse' {networkInterfaceId} -> networkInterfaceId) (\s@DescribeNotebookInstanceResponse' {} a -> s {networkInterfaceId = a} :: DescribeNotebookInstanceResponse)

-- | A timestamp. Use this parameter to retrieve the time when the notebook
-- instance was last modified.
describeNotebookInstanceResponse_lastModifiedTime :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Core.UTCTime)
describeNotebookInstanceResponse_lastModifiedTime = Lens.lens (\DescribeNotebookInstanceResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeNotebookInstanceResponse' {} a -> s {lastModifiedTime = a} :: DescribeNotebookInstanceResponse) Core.. Lens.mapping Core._Time

-- | The ID of the VPC subnet.
describeNotebookInstanceResponse_subnetId :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Core.Text)
describeNotebookInstanceResponse_subnetId = Lens.lens (\DescribeNotebookInstanceResponse' {subnetId} -> subnetId) (\s@DescribeNotebookInstanceResponse' {} a -> s {subnetId = a} :: DescribeNotebookInstanceResponse)

-- | The URL that you use to connect to the Jupyter notebook that is running
-- in your notebook instance.
describeNotebookInstanceResponse_url :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Core.Text)
describeNotebookInstanceResponse_url = Lens.lens (\DescribeNotebookInstanceResponse' {url} -> url) (\s@DescribeNotebookInstanceResponse' {} a -> s {url = a} :: DescribeNotebookInstanceResponse)

-- | The Amazon Resource Name (ARN) of the notebook instance.
describeNotebookInstanceResponse_notebookInstanceArn :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe Core.Text)
describeNotebookInstanceResponse_notebookInstanceArn = Lens.lens (\DescribeNotebookInstanceResponse' {notebookInstanceArn} -> notebookInstanceArn) (\s@DescribeNotebookInstanceResponse' {} a -> s {notebookInstanceArn = a} :: DescribeNotebookInstanceResponse)

-- | Describes whether Amazon SageMaker provides internet access to the
-- notebook instance. If this value is set to /Disabled/, the notebook
-- instance does not have internet access, and cannot connect to Amazon
-- SageMaker training and endpoint services.
--
-- For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/appendix-additional-considerations.html#appendix-notebook-and-internet-access Notebook Instances Are Internet-Enabled by Default>.
describeNotebookInstanceResponse_directInternetAccess :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe DirectInternetAccess)
describeNotebookInstanceResponse_directInternetAccess = Lens.lens (\DescribeNotebookInstanceResponse' {directInternetAccess} -> directInternetAccess) (\s@DescribeNotebookInstanceResponse' {} a -> s {directInternetAccess = a} :: DescribeNotebookInstanceResponse)

-- | Whether root access is enabled or disabled for users of the notebook
-- instance.
--
-- Lifecycle configurations need root access to be able to set up a
-- notebook instance. Because of this, lifecycle configurations associated
-- with a notebook instance always run with root access even if you disable
-- root access for users.
describeNotebookInstanceResponse_rootAccess :: Lens.Lens' DescribeNotebookInstanceResponse (Core.Maybe RootAccess)
describeNotebookInstanceResponse_rootAccess = Lens.lens (\DescribeNotebookInstanceResponse' {rootAccess} -> rootAccess) (\s@DescribeNotebookInstanceResponse' {} a -> s {rootAccess = a} :: DescribeNotebookInstanceResponse)

-- | The response's http status code.
describeNotebookInstanceResponse_httpStatus :: Lens.Lens' DescribeNotebookInstanceResponse Core.Int
describeNotebookInstanceResponse_httpStatus = Lens.lens (\DescribeNotebookInstanceResponse' {httpStatus} -> httpStatus) (\s@DescribeNotebookInstanceResponse' {} a -> s {httpStatus = a} :: DescribeNotebookInstanceResponse)

instance Core.NFData DescribeNotebookInstanceResponse
