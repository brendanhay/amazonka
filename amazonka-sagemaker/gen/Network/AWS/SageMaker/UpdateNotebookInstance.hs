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
-- Module      : Network.AWS.SageMaker.UpdateNotebookInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a notebook instance. NotebookInstance updates include upgrading
-- or downgrading the ML compute instance used for your notebook instance
-- to accommodate changes in your workload requirements.
module Network.AWS.SageMaker.UpdateNotebookInstance
  ( -- * Creating a Request
    UpdateNotebookInstance (..),
    newUpdateNotebookInstance,

    -- * Request Lenses
    updateNotebookInstance_acceleratorTypes,
    updateNotebookInstance_defaultCodeRepository,
    updateNotebookInstance_roleArn,
    updateNotebookInstance_instanceType,
    updateNotebookInstance_disassociateDefaultCodeRepository,
    updateNotebookInstance_disassociateAcceleratorTypes,
    updateNotebookInstance_disassociateLifecycleConfig,
    updateNotebookInstance_additionalCodeRepositories,
    updateNotebookInstance_disassociateAdditionalCodeRepositories,
    updateNotebookInstance_volumeSizeInGB,
    updateNotebookInstance_lifecycleConfigName,
    updateNotebookInstance_rootAccess,
    updateNotebookInstance_notebookInstanceName,

    -- * Destructuring the Response
    UpdateNotebookInstanceResponse (..),
    newUpdateNotebookInstanceResponse,

    -- * Response Lenses
    updateNotebookInstanceResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateNotebookInstance' smart constructor.
data UpdateNotebookInstance = UpdateNotebookInstance'
  { -- | A list of the Elastic Inference (EI) instance types to associate with
    -- this notebook instance. Currently only one EI instance type can be
    -- associated with a notebook instance. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
    acceleratorTypes :: Core.Maybe [NotebookInstanceAcceleratorType],
    -- | The Git repository to associate with the notebook instance as its
    -- default code repository. This can be either the name of a Git repository
    -- stored as a resource in your account, or the URL of a Git repository in
    -- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit>
    -- or in any other Git repository. When you open a notebook instance, it
    -- opens in the directory that contains this repository. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances>.
    defaultCodeRepository :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker can
    -- assume to access the notebook instance. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles>.
    --
    -- To be able to pass this role to Amazon SageMaker, the caller of this API
    -- must have the @iam:PassRole@ permission.
    roleArn :: Core.Maybe Core.Text,
    -- | The Amazon ML compute instance type.
    instanceType :: Core.Maybe InstanceType,
    -- | The name or URL of the default Git repository to remove from this
    -- notebook instance. This operation is idempotent. If you specify a Git
    -- repository that is not associated with the notebook instance when you
    -- call this method, it does not throw an error.
    disassociateDefaultCodeRepository :: Core.Maybe Core.Bool,
    -- | A list of the Elastic Inference (EI) instance types to remove from this
    -- notebook instance. This operation is idempotent. If you specify an
    -- accelerator type that is not associated with the notebook instance when
    -- you call this method, it does not throw an error.
    disassociateAcceleratorTypes :: Core.Maybe Core.Bool,
    -- | Set to @true@ to remove the notebook instance lifecycle configuration
    -- currently associated with the notebook instance. This operation is
    -- idempotent. If you specify a lifecycle configuration that is not
    -- associated with the notebook instance when you call this method, it does
    -- not throw an error.
    disassociateLifecycleConfig :: Core.Maybe Core.Bool,
    -- | An array of up to three Git repositories to associate with the notebook
    -- instance. These can be either the names of Git repositories stored as
    -- resources in your account, or the URL of Git repositories in
    -- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit>
    -- or in any other Git repository. These repositories are cloned at the
    -- same level as the default repository of your notebook instance. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances>.
    additionalCodeRepositories :: Core.Maybe [Core.Text],
    -- | A list of names or URLs of the default Git repositories to remove from
    -- this notebook instance. This operation is idempotent. If you specify a
    -- Git repository that is not associated with the notebook instance when
    -- you call this method, it does not throw an error.
    disassociateAdditionalCodeRepositories :: Core.Maybe Core.Bool,
    -- | The size, in GB, of the ML storage volume to attach to the notebook
    -- instance. The default value is 5 GB. ML storage volumes are encrypted,
    -- so Amazon SageMaker can\'t determine the amount of available free space
    -- on the volume. Because of this, you can increase the volume size when
    -- you update a notebook instance, but you can\'t decrease the volume size.
    -- If you want to decrease the size of the ML storage volume in use, create
    -- a new notebook instance with the desired size.
    volumeSizeInGB :: Core.Maybe Core.Natural,
    -- | The name of a lifecycle configuration to associate with the notebook
    -- instance. For information about lifestyle configurations, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>.
    lifecycleConfigName :: Core.Maybe Core.Text,
    -- | Whether root access is enabled or disabled for users of the notebook
    -- instance. The default value is @Enabled@.
    --
    -- If you set this to @Disabled@, users don\'t have root access on the
    -- notebook instance, but lifecycle configuration scripts still run with
    -- root permissions.
    rootAccess :: Core.Maybe RootAccess,
    -- | The name of the notebook instance to update.
    notebookInstanceName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateNotebookInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceleratorTypes', 'updateNotebookInstance_acceleratorTypes' - A list of the Elastic Inference (EI) instance types to associate with
-- this notebook instance. Currently only one EI instance type can be
-- associated with a notebook instance. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
--
-- 'defaultCodeRepository', 'updateNotebookInstance_defaultCodeRepository' - The Git repository to associate with the notebook instance as its
-- default code repository. This can be either the name of a Git repository
-- stored as a resource in your account, or the URL of a Git repository in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit>
-- or in any other Git repository. When you open a notebook instance, it
-- opens in the directory that contains this repository. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances>.
--
-- 'roleArn', 'updateNotebookInstance_roleArn' - The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker can
-- assume to access the notebook instance. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles>.
--
-- To be able to pass this role to Amazon SageMaker, the caller of this API
-- must have the @iam:PassRole@ permission.
--
-- 'instanceType', 'updateNotebookInstance_instanceType' - The Amazon ML compute instance type.
--
-- 'disassociateDefaultCodeRepository', 'updateNotebookInstance_disassociateDefaultCodeRepository' - The name or URL of the default Git repository to remove from this
-- notebook instance. This operation is idempotent. If you specify a Git
-- repository that is not associated with the notebook instance when you
-- call this method, it does not throw an error.
--
-- 'disassociateAcceleratorTypes', 'updateNotebookInstance_disassociateAcceleratorTypes' - A list of the Elastic Inference (EI) instance types to remove from this
-- notebook instance. This operation is idempotent. If you specify an
-- accelerator type that is not associated with the notebook instance when
-- you call this method, it does not throw an error.
--
-- 'disassociateLifecycleConfig', 'updateNotebookInstance_disassociateLifecycleConfig' - Set to @true@ to remove the notebook instance lifecycle configuration
-- currently associated with the notebook instance. This operation is
-- idempotent. If you specify a lifecycle configuration that is not
-- associated with the notebook instance when you call this method, it does
-- not throw an error.
--
-- 'additionalCodeRepositories', 'updateNotebookInstance_additionalCodeRepositories' - An array of up to three Git repositories to associate with the notebook
-- instance. These can be either the names of Git repositories stored as
-- resources in your account, or the URL of Git repositories in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit>
-- or in any other Git repository. These repositories are cloned at the
-- same level as the default repository of your notebook instance. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances>.
--
-- 'disassociateAdditionalCodeRepositories', 'updateNotebookInstance_disassociateAdditionalCodeRepositories' - A list of names or URLs of the default Git repositories to remove from
-- this notebook instance. This operation is idempotent. If you specify a
-- Git repository that is not associated with the notebook instance when
-- you call this method, it does not throw an error.
--
-- 'volumeSizeInGB', 'updateNotebookInstance_volumeSizeInGB' - The size, in GB, of the ML storage volume to attach to the notebook
-- instance. The default value is 5 GB. ML storage volumes are encrypted,
-- so Amazon SageMaker can\'t determine the amount of available free space
-- on the volume. Because of this, you can increase the volume size when
-- you update a notebook instance, but you can\'t decrease the volume size.
-- If you want to decrease the size of the ML storage volume in use, create
-- a new notebook instance with the desired size.
--
-- 'lifecycleConfigName', 'updateNotebookInstance_lifecycleConfigName' - The name of a lifecycle configuration to associate with the notebook
-- instance. For information about lifestyle configurations, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>.
--
-- 'rootAccess', 'updateNotebookInstance_rootAccess' - Whether root access is enabled or disabled for users of the notebook
-- instance. The default value is @Enabled@.
--
-- If you set this to @Disabled@, users don\'t have root access on the
-- notebook instance, but lifecycle configuration scripts still run with
-- root permissions.
--
-- 'notebookInstanceName', 'updateNotebookInstance_notebookInstanceName' - The name of the notebook instance to update.
newUpdateNotebookInstance ::
  -- | 'notebookInstanceName'
  Core.Text ->
  UpdateNotebookInstance
newUpdateNotebookInstance pNotebookInstanceName_ =
  UpdateNotebookInstance'
    { acceleratorTypes =
        Core.Nothing,
      defaultCodeRepository = Core.Nothing,
      roleArn = Core.Nothing,
      instanceType = Core.Nothing,
      disassociateDefaultCodeRepository = Core.Nothing,
      disassociateAcceleratorTypes = Core.Nothing,
      disassociateLifecycleConfig = Core.Nothing,
      additionalCodeRepositories = Core.Nothing,
      disassociateAdditionalCodeRepositories =
        Core.Nothing,
      volumeSizeInGB = Core.Nothing,
      lifecycleConfigName = Core.Nothing,
      rootAccess = Core.Nothing,
      notebookInstanceName = pNotebookInstanceName_
    }

-- | A list of the Elastic Inference (EI) instance types to associate with
-- this notebook instance. Currently only one EI instance type can be
-- associated with a notebook instance. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
updateNotebookInstance_acceleratorTypes :: Lens.Lens' UpdateNotebookInstance (Core.Maybe [NotebookInstanceAcceleratorType])
updateNotebookInstance_acceleratorTypes = Lens.lens (\UpdateNotebookInstance' {acceleratorTypes} -> acceleratorTypes) (\s@UpdateNotebookInstance' {} a -> s {acceleratorTypes = a} :: UpdateNotebookInstance) Core.. Lens.mapping Lens._Coerce

-- | The Git repository to associate with the notebook instance as its
-- default code repository. This can be either the name of a Git repository
-- stored as a resource in your account, or the URL of a Git repository in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit>
-- or in any other Git repository. When you open a notebook instance, it
-- opens in the directory that contains this repository. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances>.
updateNotebookInstance_defaultCodeRepository :: Lens.Lens' UpdateNotebookInstance (Core.Maybe Core.Text)
updateNotebookInstance_defaultCodeRepository = Lens.lens (\UpdateNotebookInstance' {defaultCodeRepository} -> defaultCodeRepository) (\s@UpdateNotebookInstance' {} a -> s {defaultCodeRepository = a} :: UpdateNotebookInstance)

-- | The Amazon Resource Name (ARN) of the IAM role that Amazon SageMaker can
-- assume to access the notebook instance. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html Amazon SageMaker Roles>.
--
-- To be able to pass this role to Amazon SageMaker, the caller of this API
-- must have the @iam:PassRole@ permission.
updateNotebookInstance_roleArn :: Lens.Lens' UpdateNotebookInstance (Core.Maybe Core.Text)
updateNotebookInstance_roleArn = Lens.lens (\UpdateNotebookInstance' {roleArn} -> roleArn) (\s@UpdateNotebookInstance' {} a -> s {roleArn = a} :: UpdateNotebookInstance)

-- | The Amazon ML compute instance type.
updateNotebookInstance_instanceType :: Lens.Lens' UpdateNotebookInstance (Core.Maybe InstanceType)
updateNotebookInstance_instanceType = Lens.lens (\UpdateNotebookInstance' {instanceType} -> instanceType) (\s@UpdateNotebookInstance' {} a -> s {instanceType = a} :: UpdateNotebookInstance)

-- | The name or URL of the default Git repository to remove from this
-- notebook instance. This operation is idempotent. If you specify a Git
-- repository that is not associated with the notebook instance when you
-- call this method, it does not throw an error.
updateNotebookInstance_disassociateDefaultCodeRepository :: Lens.Lens' UpdateNotebookInstance (Core.Maybe Core.Bool)
updateNotebookInstance_disassociateDefaultCodeRepository = Lens.lens (\UpdateNotebookInstance' {disassociateDefaultCodeRepository} -> disassociateDefaultCodeRepository) (\s@UpdateNotebookInstance' {} a -> s {disassociateDefaultCodeRepository = a} :: UpdateNotebookInstance)

-- | A list of the Elastic Inference (EI) instance types to remove from this
-- notebook instance. This operation is idempotent. If you specify an
-- accelerator type that is not associated with the notebook instance when
-- you call this method, it does not throw an error.
updateNotebookInstance_disassociateAcceleratorTypes :: Lens.Lens' UpdateNotebookInstance (Core.Maybe Core.Bool)
updateNotebookInstance_disassociateAcceleratorTypes = Lens.lens (\UpdateNotebookInstance' {disassociateAcceleratorTypes} -> disassociateAcceleratorTypes) (\s@UpdateNotebookInstance' {} a -> s {disassociateAcceleratorTypes = a} :: UpdateNotebookInstance)

-- | Set to @true@ to remove the notebook instance lifecycle configuration
-- currently associated with the notebook instance. This operation is
-- idempotent. If you specify a lifecycle configuration that is not
-- associated with the notebook instance when you call this method, it does
-- not throw an error.
updateNotebookInstance_disassociateLifecycleConfig :: Lens.Lens' UpdateNotebookInstance (Core.Maybe Core.Bool)
updateNotebookInstance_disassociateLifecycleConfig = Lens.lens (\UpdateNotebookInstance' {disassociateLifecycleConfig} -> disassociateLifecycleConfig) (\s@UpdateNotebookInstance' {} a -> s {disassociateLifecycleConfig = a} :: UpdateNotebookInstance)

-- | An array of up to three Git repositories to associate with the notebook
-- instance. These can be either the names of Git repositories stored as
-- resources in your account, or the URL of Git repositories in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html AWS CodeCommit>
-- or in any other Git repository. These repositories are cloned at the
-- same level as the default repository of your notebook instance. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with Amazon SageMaker Notebook Instances>.
updateNotebookInstance_additionalCodeRepositories :: Lens.Lens' UpdateNotebookInstance (Core.Maybe [Core.Text])
updateNotebookInstance_additionalCodeRepositories = Lens.lens (\UpdateNotebookInstance' {additionalCodeRepositories} -> additionalCodeRepositories) (\s@UpdateNotebookInstance' {} a -> s {additionalCodeRepositories = a} :: UpdateNotebookInstance) Core.. Lens.mapping Lens._Coerce

-- | A list of names or URLs of the default Git repositories to remove from
-- this notebook instance. This operation is idempotent. If you specify a
-- Git repository that is not associated with the notebook instance when
-- you call this method, it does not throw an error.
updateNotebookInstance_disassociateAdditionalCodeRepositories :: Lens.Lens' UpdateNotebookInstance (Core.Maybe Core.Bool)
updateNotebookInstance_disassociateAdditionalCodeRepositories = Lens.lens (\UpdateNotebookInstance' {disassociateAdditionalCodeRepositories} -> disassociateAdditionalCodeRepositories) (\s@UpdateNotebookInstance' {} a -> s {disassociateAdditionalCodeRepositories = a} :: UpdateNotebookInstance)

-- | The size, in GB, of the ML storage volume to attach to the notebook
-- instance. The default value is 5 GB. ML storage volumes are encrypted,
-- so Amazon SageMaker can\'t determine the amount of available free space
-- on the volume. Because of this, you can increase the volume size when
-- you update a notebook instance, but you can\'t decrease the volume size.
-- If you want to decrease the size of the ML storage volume in use, create
-- a new notebook instance with the desired size.
updateNotebookInstance_volumeSizeInGB :: Lens.Lens' UpdateNotebookInstance (Core.Maybe Core.Natural)
updateNotebookInstance_volumeSizeInGB = Lens.lens (\UpdateNotebookInstance' {volumeSizeInGB} -> volumeSizeInGB) (\s@UpdateNotebookInstance' {} a -> s {volumeSizeInGB = a} :: UpdateNotebookInstance)

-- | The name of a lifecycle configuration to associate with the notebook
-- instance. For information about lifestyle configurations, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>.
updateNotebookInstance_lifecycleConfigName :: Lens.Lens' UpdateNotebookInstance (Core.Maybe Core.Text)
updateNotebookInstance_lifecycleConfigName = Lens.lens (\UpdateNotebookInstance' {lifecycleConfigName} -> lifecycleConfigName) (\s@UpdateNotebookInstance' {} a -> s {lifecycleConfigName = a} :: UpdateNotebookInstance)

-- | Whether root access is enabled or disabled for users of the notebook
-- instance. The default value is @Enabled@.
--
-- If you set this to @Disabled@, users don\'t have root access on the
-- notebook instance, but lifecycle configuration scripts still run with
-- root permissions.
updateNotebookInstance_rootAccess :: Lens.Lens' UpdateNotebookInstance (Core.Maybe RootAccess)
updateNotebookInstance_rootAccess = Lens.lens (\UpdateNotebookInstance' {rootAccess} -> rootAccess) (\s@UpdateNotebookInstance' {} a -> s {rootAccess = a} :: UpdateNotebookInstance)

-- | The name of the notebook instance to update.
updateNotebookInstance_notebookInstanceName :: Lens.Lens' UpdateNotebookInstance Core.Text
updateNotebookInstance_notebookInstanceName = Lens.lens (\UpdateNotebookInstance' {notebookInstanceName} -> notebookInstanceName) (\s@UpdateNotebookInstance' {} a -> s {notebookInstanceName = a} :: UpdateNotebookInstance)

instance Core.AWSRequest UpdateNotebookInstance where
  type
    AWSResponse UpdateNotebookInstance =
      UpdateNotebookInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNotebookInstanceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateNotebookInstance

instance Core.NFData UpdateNotebookInstance

instance Core.ToHeaders UpdateNotebookInstance where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.UpdateNotebookInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateNotebookInstance where
  toJSON UpdateNotebookInstance' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AcceleratorTypes" Core..=)
              Core.<$> acceleratorTypes,
            ("DefaultCodeRepository" Core..=)
              Core.<$> defaultCodeRepository,
            ("RoleArn" Core..=) Core.<$> roleArn,
            ("InstanceType" Core..=) Core.<$> instanceType,
            ("DisassociateDefaultCodeRepository" Core..=)
              Core.<$> disassociateDefaultCodeRepository,
            ("DisassociateAcceleratorTypes" Core..=)
              Core.<$> disassociateAcceleratorTypes,
            ("DisassociateLifecycleConfig" Core..=)
              Core.<$> disassociateLifecycleConfig,
            ("AdditionalCodeRepositories" Core..=)
              Core.<$> additionalCodeRepositories,
            ("DisassociateAdditionalCodeRepositories" Core..=)
              Core.<$> disassociateAdditionalCodeRepositories,
            ("VolumeSizeInGB" Core..=) Core.<$> volumeSizeInGB,
            ("LifecycleConfigName" Core..=)
              Core.<$> lifecycleConfigName,
            ("RootAccess" Core..=) Core.<$> rootAccess,
            Core.Just
              ( "NotebookInstanceName"
                  Core..= notebookInstanceName
              )
          ]
      )

instance Core.ToPath UpdateNotebookInstance where
  toPath = Core.const "/"

instance Core.ToQuery UpdateNotebookInstance where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateNotebookInstanceResponse' smart constructor.
data UpdateNotebookInstanceResponse = UpdateNotebookInstanceResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateNotebookInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateNotebookInstanceResponse_httpStatus' - The response's http status code.
newUpdateNotebookInstanceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateNotebookInstanceResponse
newUpdateNotebookInstanceResponse pHttpStatus_ =
  UpdateNotebookInstanceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateNotebookInstanceResponse_httpStatus :: Lens.Lens' UpdateNotebookInstanceResponse Core.Int
updateNotebookInstanceResponse_httpStatus = Lens.lens (\UpdateNotebookInstanceResponse' {httpStatus} -> httpStatus) (\s@UpdateNotebookInstanceResponse' {} a -> s {httpStatus = a} :: UpdateNotebookInstanceResponse)

instance Core.NFData UpdateNotebookInstanceResponse
