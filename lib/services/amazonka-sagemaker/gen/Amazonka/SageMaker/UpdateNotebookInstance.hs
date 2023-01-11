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
-- Module      : Amazonka.SageMaker.UpdateNotebookInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a notebook instance. NotebookInstance updates include upgrading
-- or downgrading the ML compute instance used for your notebook instance
-- to accommodate changes in your workload requirements.
module Amazonka.SageMaker.UpdateNotebookInstance
  ( -- * Creating a Request
    UpdateNotebookInstance (..),
    newUpdateNotebookInstance,

    -- * Request Lenses
    updateNotebookInstance_acceleratorTypes,
    updateNotebookInstance_additionalCodeRepositories,
    updateNotebookInstance_defaultCodeRepository,
    updateNotebookInstance_disassociateAcceleratorTypes,
    updateNotebookInstance_disassociateAdditionalCodeRepositories,
    updateNotebookInstance_disassociateDefaultCodeRepository,
    updateNotebookInstance_disassociateLifecycleConfig,
    updateNotebookInstance_instanceMetadataServiceConfiguration,
    updateNotebookInstance_instanceType,
    updateNotebookInstance_lifecycleConfigName,
    updateNotebookInstance_roleArn,
    updateNotebookInstance_rootAccess,
    updateNotebookInstance_volumeSizeInGB,
    updateNotebookInstance_notebookInstanceName,

    -- * Destructuring the Response
    UpdateNotebookInstanceResponse (..),
    newUpdateNotebookInstanceResponse,

    -- * Response Lenses
    updateNotebookInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateNotebookInstance' smart constructor.
data UpdateNotebookInstance = UpdateNotebookInstance'
  { -- | A list of the Elastic Inference (EI) instance types to associate with
    -- this notebook instance. Currently only one EI instance type can be
    -- associated with a notebook instance. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
    acceleratorTypes :: Prelude.Maybe [NotebookInstanceAcceleratorType],
    -- | An array of up to three Git repositories to associate with the notebook
    -- instance. These can be either the names of Git repositories stored as
    -- resources in your account, or the URL of Git repositories in
    -- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html Amazon Web Services CodeCommit>
    -- or in any other Git repository. These repositories are cloned at the
    -- same level as the default repository of your notebook instance. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with SageMaker Notebook Instances>.
    additionalCodeRepositories :: Prelude.Maybe [Prelude.Text],
    -- | The Git repository to associate with the notebook instance as its
    -- default code repository. This can be either the name of a Git repository
    -- stored as a resource in your account, or the URL of a Git repository in
    -- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html Amazon Web Services CodeCommit>
    -- or in any other Git repository. When you open a notebook instance, it
    -- opens in the directory that contains this repository. For more
    -- information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with SageMaker Notebook Instances>.
    defaultCodeRepository :: Prelude.Maybe Prelude.Text,
    -- | A list of the Elastic Inference (EI) instance types to remove from this
    -- notebook instance. This operation is idempotent. If you specify an
    -- accelerator type that is not associated with the notebook instance when
    -- you call this method, it does not throw an error.
    disassociateAcceleratorTypes :: Prelude.Maybe Prelude.Bool,
    -- | A list of names or URLs of the default Git repositories to remove from
    -- this notebook instance. This operation is idempotent. If you specify a
    -- Git repository that is not associated with the notebook instance when
    -- you call this method, it does not throw an error.
    disassociateAdditionalCodeRepositories :: Prelude.Maybe Prelude.Bool,
    -- | The name or URL of the default Git repository to remove from this
    -- notebook instance. This operation is idempotent. If you specify a Git
    -- repository that is not associated with the notebook instance when you
    -- call this method, it does not throw an error.
    disassociateDefaultCodeRepository :: Prelude.Maybe Prelude.Bool,
    -- | Set to @true@ to remove the notebook instance lifecycle configuration
    -- currently associated with the notebook instance. This operation is
    -- idempotent. If you specify a lifecycle configuration that is not
    -- associated with the notebook instance when you call this method, it does
    -- not throw an error.
    disassociateLifecycleConfig :: Prelude.Maybe Prelude.Bool,
    -- | Information on the IMDS configuration of the notebook instance
    instanceMetadataServiceConfiguration :: Prelude.Maybe InstanceMetadataServiceConfiguration,
    -- | The Amazon ML compute instance type.
    instanceType :: Prelude.Maybe InstanceType,
    -- | The name of a lifecycle configuration to associate with the notebook
    -- instance. For information about lifestyle configurations, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>.
    lifecycleConfigName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that SageMaker can assume
    -- to access the notebook instance. For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html SageMaker Roles>.
    --
    -- To be able to pass this role to SageMaker, the caller of this API must
    -- have the @iam:PassRole@ permission.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | Whether root access is enabled or disabled for users of the notebook
    -- instance. The default value is @Enabled@.
    --
    -- If you set this to @Disabled@, users don\'t have root access on the
    -- notebook instance, but lifecycle configuration scripts still run with
    -- root permissions.
    rootAccess :: Prelude.Maybe RootAccess,
    -- | The size, in GB, of the ML storage volume to attach to the notebook
    -- instance. The default value is 5 GB. ML storage volumes are encrypted,
    -- so SageMaker can\'t determine the amount of available free space on the
    -- volume. Because of this, you can increase the volume size when you
    -- update a notebook instance, but you can\'t decrease the volume size. If
    -- you want to decrease the size of the ML storage volume in use, create a
    -- new notebook instance with the desired size.
    volumeSizeInGB :: Prelude.Maybe Prelude.Natural,
    -- | The name of the notebook instance to update.
    notebookInstanceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'additionalCodeRepositories', 'updateNotebookInstance_additionalCodeRepositories' - An array of up to three Git repositories to associate with the notebook
-- instance. These can be either the names of Git repositories stored as
-- resources in your account, or the URL of Git repositories in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html Amazon Web Services CodeCommit>
-- or in any other Git repository. These repositories are cloned at the
-- same level as the default repository of your notebook instance. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with SageMaker Notebook Instances>.
--
-- 'defaultCodeRepository', 'updateNotebookInstance_defaultCodeRepository' - The Git repository to associate with the notebook instance as its
-- default code repository. This can be either the name of a Git repository
-- stored as a resource in your account, or the URL of a Git repository in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html Amazon Web Services CodeCommit>
-- or in any other Git repository. When you open a notebook instance, it
-- opens in the directory that contains this repository. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with SageMaker Notebook Instances>.
--
-- 'disassociateAcceleratorTypes', 'updateNotebookInstance_disassociateAcceleratorTypes' - A list of the Elastic Inference (EI) instance types to remove from this
-- notebook instance. This operation is idempotent. If you specify an
-- accelerator type that is not associated with the notebook instance when
-- you call this method, it does not throw an error.
--
-- 'disassociateAdditionalCodeRepositories', 'updateNotebookInstance_disassociateAdditionalCodeRepositories' - A list of names or URLs of the default Git repositories to remove from
-- this notebook instance. This operation is idempotent. If you specify a
-- Git repository that is not associated with the notebook instance when
-- you call this method, it does not throw an error.
--
-- 'disassociateDefaultCodeRepository', 'updateNotebookInstance_disassociateDefaultCodeRepository' - The name or URL of the default Git repository to remove from this
-- notebook instance. This operation is idempotent. If you specify a Git
-- repository that is not associated with the notebook instance when you
-- call this method, it does not throw an error.
--
-- 'disassociateLifecycleConfig', 'updateNotebookInstance_disassociateLifecycleConfig' - Set to @true@ to remove the notebook instance lifecycle configuration
-- currently associated with the notebook instance. This operation is
-- idempotent. If you specify a lifecycle configuration that is not
-- associated with the notebook instance when you call this method, it does
-- not throw an error.
--
-- 'instanceMetadataServiceConfiguration', 'updateNotebookInstance_instanceMetadataServiceConfiguration' - Information on the IMDS configuration of the notebook instance
--
-- 'instanceType', 'updateNotebookInstance_instanceType' - The Amazon ML compute instance type.
--
-- 'lifecycleConfigName', 'updateNotebookInstance_lifecycleConfigName' - The name of a lifecycle configuration to associate with the notebook
-- instance. For information about lifestyle configurations, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>.
--
-- 'roleArn', 'updateNotebookInstance_roleArn' - The Amazon Resource Name (ARN) of the IAM role that SageMaker can assume
-- to access the notebook instance. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html SageMaker Roles>.
--
-- To be able to pass this role to SageMaker, the caller of this API must
-- have the @iam:PassRole@ permission.
--
-- 'rootAccess', 'updateNotebookInstance_rootAccess' - Whether root access is enabled or disabled for users of the notebook
-- instance. The default value is @Enabled@.
--
-- If you set this to @Disabled@, users don\'t have root access on the
-- notebook instance, but lifecycle configuration scripts still run with
-- root permissions.
--
-- 'volumeSizeInGB', 'updateNotebookInstance_volumeSizeInGB' - The size, in GB, of the ML storage volume to attach to the notebook
-- instance. The default value is 5 GB. ML storage volumes are encrypted,
-- so SageMaker can\'t determine the amount of available free space on the
-- volume. Because of this, you can increase the volume size when you
-- update a notebook instance, but you can\'t decrease the volume size. If
-- you want to decrease the size of the ML storage volume in use, create a
-- new notebook instance with the desired size.
--
-- 'notebookInstanceName', 'updateNotebookInstance_notebookInstanceName' - The name of the notebook instance to update.
newUpdateNotebookInstance ::
  -- | 'notebookInstanceName'
  Prelude.Text ->
  UpdateNotebookInstance
newUpdateNotebookInstance pNotebookInstanceName_ =
  UpdateNotebookInstance'
    { acceleratorTypes =
        Prelude.Nothing,
      additionalCodeRepositories = Prelude.Nothing,
      defaultCodeRepository = Prelude.Nothing,
      disassociateAcceleratorTypes = Prelude.Nothing,
      disassociateAdditionalCodeRepositories =
        Prelude.Nothing,
      disassociateDefaultCodeRepository = Prelude.Nothing,
      disassociateLifecycleConfig = Prelude.Nothing,
      instanceMetadataServiceConfiguration =
        Prelude.Nothing,
      instanceType = Prelude.Nothing,
      lifecycleConfigName = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      rootAccess = Prelude.Nothing,
      volumeSizeInGB = Prelude.Nothing,
      notebookInstanceName = pNotebookInstanceName_
    }

-- | A list of the Elastic Inference (EI) instance types to associate with
-- this notebook instance. Currently only one EI instance type can be
-- associated with a notebook instance. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/ei.html Using Elastic Inference in Amazon SageMaker>.
updateNotebookInstance_acceleratorTypes :: Lens.Lens' UpdateNotebookInstance (Prelude.Maybe [NotebookInstanceAcceleratorType])
updateNotebookInstance_acceleratorTypes = Lens.lens (\UpdateNotebookInstance' {acceleratorTypes} -> acceleratorTypes) (\s@UpdateNotebookInstance' {} a -> s {acceleratorTypes = a} :: UpdateNotebookInstance) Prelude.. Lens.mapping Lens.coerced

-- | An array of up to three Git repositories to associate with the notebook
-- instance. These can be either the names of Git repositories stored as
-- resources in your account, or the URL of Git repositories in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html Amazon Web Services CodeCommit>
-- or in any other Git repository. These repositories are cloned at the
-- same level as the default repository of your notebook instance. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with SageMaker Notebook Instances>.
updateNotebookInstance_additionalCodeRepositories :: Lens.Lens' UpdateNotebookInstance (Prelude.Maybe [Prelude.Text])
updateNotebookInstance_additionalCodeRepositories = Lens.lens (\UpdateNotebookInstance' {additionalCodeRepositories} -> additionalCodeRepositories) (\s@UpdateNotebookInstance' {} a -> s {additionalCodeRepositories = a} :: UpdateNotebookInstance) Prelude.. Lens.mapping Lens.coerced

-- | The Git repository to associate with the notebook instance as its
-- default code repository. This can be either the name of a Git repository
-- stored as a resource in your account, or the URL of a Git repository in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html Amazon Web Services CodeCommit>
-- or in any other Git repository. When you open a notebook instance, it
-- opens in the directory that contains this repository. For more
-- information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/nbi-git-repo.html Associating Git Repositories with SageMaker Notebook Instances>.
updateNotebookInstance_defaultCodeRepository :: Lens.Lens' UpdateNotebookInstance (Prelude.Maybe Prelude.Text)
updateNotebookInstance_defaultCodeRepository = Lens.lens (\UpdateNotebookInstance' {defaultCodeRepository} -> defaultCodeRepository) (\s@UpdateNotebookInstance' {} a -> s {defaultCodeRepository = a} :: UpdateNotebookInstance)

-- | A list of the Elastic Inference (EI) instance types to remove from this
-- notebook instance. This operation is idempotent. If you specify an
-- accelerator type that is not associated with the notebook instance when
-- you call this method, it does not throw an error.
updateNotebookInstance_disassociateAcceleratorTypes :: Lens.Lens' UpdateNotebookInstance (Prelude.Maybe Prelude.Bool)
updateNotebookInstance_disassociateAcceleratorTypes = Lens.lens (\UpdateNotebookInstance' {disassociateAcceleratorTypes} -> disassociateAcceleratorTypes) (\s@UpdateNotebookInstance' {} a -> s {disassociateAcceleratorTypes = a} :: UpdateNotebookInstance)

-- | A list of names or URLs of the default Git repositories to remove from
-- this notebook instance. This operation is idempotent. If you specify a
-- Git repository that is not associated with the notebook instance when
-- you call this method, it does not throw an error.
updateNotebookInstance_disassociateAdditionalCodeRepositories :: Lens.Lens' UpdateNotebookInstance (Prelude.Maybe Prelude.Bool)
updateNotebookInstance_disassociateAdditionalCodeRepositories = Lens.lens (\UpdateNotebookInstance' {disassociateAdditionalCodeRepositories} -> disassociateAdditionalCodeRepositories) (\s@UpdateNotebookInstance' {} a -> s {disassociateAdditionalCodeRepositories = a} :: UpdateNotebookInstance)

-- | The name or URL of the default Git repository to remove from this
-- notebook instance. This operation is idempotent. If you specify a Git
-- repository that is not associated with the notebook instance when you
-- call this method, it does not throw an error.
updateNotebookInstance_disassociateDefaultCodeRepository :: Lens.Lens' UpdateNotebookInstance (Prelude.Maybe Prelude.Bool)
updateNotebookInstance_disassociateDefaultCodeRepository = Lens.lens (\UpdateNotebookInstance' {disassociateDefaultCodeRepository} -> disassociateDefaultCodeRepository) (\s@UpdateNotebookInstance' {} a -> s {disassociateDefaultCodeRepository = a} :: UpdateNotebookInstance)

-- | Set to @true@ to remove the notebook instance lifecycle configuration
-- currently associated with the notebook instance. This operation is
-- idempotent. If you specify a lifecycle configuration that is not
-- associated with the notebook instance when you call this method, it does
-- not throw an error.
updateNotebookInstance_disassociateLifecycleConfig :: Lens.Lens' UpdateNotebookInstance (Prelude.Maybe Prelude.Bool)
updateNotebookInstance_disassociateLifecycleConfig = Lens.lens (\UpdateNotebookInstance' {disassociateLifecycleConfig} -> disassociateLifecycleConfig) (\s@UpdateNotebookInstance' {} a -> s {disassociateLifecycleConfig = a} :: UpdateNotebookInstance)

-- | Information on the IMDS configuration of the notebook instance
updateNotebookInstance_instanceMetadataServiceConfiguration :: Lens.Lens' UpdateNotebookInstance (Prelude.Maybe InstanceMetadataServiceConfiguration)
updateNotebookInstance_instanceMetadataServiceConfiguration = Lens.lens (\UpdateNotebookInstance' {instanceMetadataServiceConfiguration} -> instanceMetadataServiceConfiguration) (\s@UpdateNotebookInstance' {} a -> s {instanceMetadataServiceConfiguration = a} :: UpdateNotebookInstance)

-- | The Amazon ML compute instance type.
updateNotebookInstance_instanceType :: Lens.Lens' UpdateNotebookInstance (Prelude.Maybe InstanceType)
updateNotebookInstance_instanceType = Lens.lens (\UpdateNotebookInstance' {instanceType} -> instanceType) (\s@UpdateNotebookInstance' {} a -> s {instanceType = a} :: UpdateNotebookInstance)

-- | The name of a lifecycle configuration to associate with the notebook
-- instance. For information about lifestyle configurations, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/notebook-lifecycle-config.html Step 2.1: (Optional) Customize a Notebook Instance>.
updateNotebookInstance_lifecycleConfigName :: Lens.Lens' UpdateNotebookInstance (Prelude.Maybe Prelude.Text)
updateNotebookInstance_lifecycleConfigName = Lens.lens (\UpdateNotebookInstance' {lifecycleConfigName} -> lifecycleConfigName) (\s@UpdateNotebookInstance' {} a -> s {lifecycleConfigName = a} :: UpdateNotebookInstance)

-- | The Amazon Resource Name (ARN) of the IAM role that SageMaker can assume
-- to access the notebook instance. For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/sagemaker-roles.html SageMaker Roles>.
--
-- To be able to pass this role to SageMaker, the caller of this API must
-- have the @iam:PassRole@ permission.
updateNotebookInstance_roleArn :: Lens.Lens' UpdateNotebookInstance (Prelude.Maybe Prelude.Text)
updateNotebookInstance_roleArn = Lens.lens (\UpdateNotebookInstance' {roleArn} -> roleArn) (\s@UpdateNotebookInstance' {} a -> s {roleArn = a} :: UpdateNotebookInstance)

-- | Whether root access is enabled or disabled for users of the notebook
-- instance. The default value is @Enabled@.
--
-- If you set this to @Disabled@, users don\'t have root access on the
-- notebook instance, but lifecycle configuration scripts still run with
-- root permissions.
updateNotebookInstance_rootAccess :: Lens.Lens' UpdateNotebookInstance (Prelude.Maybe RootAccess)
updateNotebookInstance_rootAccess = Lens.lens (\UpdateNotebookInstance' {rootAccess} -> rootAccess) (\s@UpdateNotebookInstance' {} a -> s {rootAccess = a} :: UpdateNotebookInstance)

-- | The size, in GB, of the ML storage volume to attach to the notebook
-- instance. The default value is 5 GB. ML storage volumes are encrypted,
-- so SageMaker can\'t determine the amount of available free space on the
-- volume. Because of this, you can increase the volume size when you
-- update a notebook instance, but you can\'t decrease the volume size. If
-- you want to decrease the size of the ML storage volume in use, create a
-- new notebook instance with the desired size.
updateNotebookInstance_volumeSizeInGB :: Lens.Lens' UpdateNotebookInstance (Prelude.Maybe Prelude.Natural)
updateNotebookInstance_volumeSizeInGB = Lens.lens (\UpdateNotebookInstance' {volumeSizeInGB} -> volumeSizeInGB) (\s@UpdateNotebookInstance' {} a -> s {volumeSizeInGB = a} :: UpdateNotebookInstance)

-- | The name of the notebook instance to update.
updateNotebookInstance_notebookInstanceName :: Lens.Lens' UpdateNotebookInstance Prelude.Text
updateNotebookInstance_notebookInstanceName = Lens.lens (\UpdateNotebookInstance' {notebookInstanceName} -> notebookInstanceName) (\s@UpdateNotebookInstance' {} a -> s {notebookInstanceName = a} :: UpdateNotebookInstance)

instance Core.AWSRequest UpdateNotebookInstance where
  type
    AWSResponse UpdateNotebookInstance =
      UpdateNotebookInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNotebookInstanceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateNotebookInstance where
  hashWithSalt _salt UpdateNotebookInstance' {..} =
    _salt `Prelude.hashWithSalt` acceleratorTypes
      `Prelude.hashWithSalt` additionalCodeRepositories
      `Prelude.hashWithSalt` defaultCodeRepository
      `Prelude.hashWithSalt` disassociateAcceleratorTypes
      `Prelude.hashWithSalt` disassociateAdditionalCodeRepositories
      `Prelude.hashWithSalt` disassociateDefaultCodeRepository
      `Prelude.hashWithSalt` disassociateLifecycleConfig
      `Prelude.hashWithSalt` instanceMetadataServiceConfiguration
      `Prelude.hashWithSalt` instanceType
      `Prelude.hashWithSalt` lifecycleConfigName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` rootAccess
      `Prelude.hashWithSalt` volumeSizeInGB
      `Prelude.hashWithSalt` notebookInstanceName

instance Prelude.NFData UpdateNotebookInstance where
  rnf UpdateNotebookInstance' {..} =
    Prelude.rnf acceleratorTypes
      `Prelude.seq` Prelude.rnf additionalCodeRepositories
      `Prelude.seq` Prelude.rnf defaultCodeRepository
      `Prelude.seq` Prelude.rnf disassociateAcceleratorTypes
      `Prelude.seq` Prelude.rnf disassociateAdditionalCodeRepositories
      `Prelude.seq` Prelude.rnf disassociateDefaultCodeRepository
      `Prelude.seq` Prelude.rnf disassociateLifecycleConfig
      `Prelude.seq` Prelude.rnf instanceMetadataServiceConfiguration
      `Prelude.seq` Prelude.rnf instanceType
      `Prelude.seq` Prelude.rnf lifecycleConfigName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf rootAccess
      `Prelude.seq` Prelude.rnf volumeSizeInGB
      `Prelude.seq` Prelude.rnf notebookInstanceName

instance Data.ToHeaders UpdateNotebookInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.UpdateNotebookInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateNotebookInstance where
  toJSON UpdateNotebookInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceleratorTypes" Data..=)
              Prelude.<$> acceleratorTypes,
            ("AdditionalCodeRepositories" Data..=)
              Prelude.<$> additionalCodeRepositories,
            ("DefaultCodeRepository" Data..=)
              Prelude.<$> defaultCodeRepository,
            ("DisassociateAcceleratorTypes" Data..=)
              Prelude.<$> disassociateAcceleratorTypes,
            ("DisassociateAdditionalCodeRepositories" Data..=)
              Prelude.<$> disassociateAdditionalCodeRepositories,
            ("DisassociateDefaultCodeRepository" Data..=)
              Prelude.<$> disassociateDefaultCodeRepository,
            ("DisassociateLifecycleConfig" Data..=)
              Prelude.<$> disassociateLifecycleConfig,
            ("InstanceMetadataServiceConfiguration" Data..=)
              Prelude.<$> instanceMetadataServiceConfiguration,
            ("InstanceType" Data..=) Prelude.<$> instanceType,
            ("LifecycleConfigName" Data..=)
              Prelude.<$> lifecycleConfigName,
            ("RoleArn" Data..=) Prelude.<$> roleArn,
            ("RootAccess" Data..=) Prelude.<$> rootAccess,
            ("VolumeSizeInGB" Data..=)
              Prelude.<$> volumeSizeInGB,
            Prelude.Just
              ( "NotebookInstanceName"
                  Data..= notebookInstanceName
              )
          ]
      )

instance Data.ToPath UpdateNotebookInstance where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateNotebookInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateNotebookInstanceResponse' smart constructor.
data UpdateNotebookInstanceResponse = UpdateNotebookInstanceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateNotebookInstanceResponse
newUpdateNotebookInstanceResponse pHttpStatus_ =
  UpdateNotebookInstanceResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateNotebookInstanceResponse_httpStatus :: Lens.Lens' UpdateNotebookInstanceResponse Prelude.Int
updateNotebookInstanceResponse_httpStatus = Lens.lens (\UpdateNotebookInstanceResponse' {httpStatus} -> httpStatus) (\s@UpdateNotebookInstanceResponse' {} a -> s {httpStatus = a} :: UpdateNotebookInstanceResponse)

instance
  Prelude.NFData
    UpdateNotebookInstanceResponse
  where
  rnf UpdateNotebookInstanceResponse' {..} =
    Prelude.rnf httpStatus
