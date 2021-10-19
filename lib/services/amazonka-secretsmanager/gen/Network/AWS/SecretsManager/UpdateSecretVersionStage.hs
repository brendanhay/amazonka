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
-- Module      : Network.AWS.SecretsManager.UpdateSecretVersionStage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the staging labels attached to a version of a secret. Staging
-- labels are used to track a version as it progresses through the secret
-- rotation process. You can attach a staging label to only one version of
-- a secret at a time. If a staging label to be added is already attached
-- to another version, then it is moved--removed from the other version
-- first and then attached to this one. For more information about staging
-- labels, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/terms-concepts.html#term_staging-label Staging Labels>
-- in the /Amazon Web Services Secrets Manager User Guide/.
--
-- The staging labels that you specify in the @VersionStage@ parameter are
-- added to the existing list of staging labels--they don\'t replace it.
--
-- You can move the @AWSCURRENT@ staging label to this version by including
-- it in this call.
--
-- Whenever you move @AWSCURRENT@, Secrets Manager automatically moves the
-- label @AWSPREVIOUS@ to the version that @AWSCURRENT@ was removed from.
--
-- If this action results in the last label being removed from a version,
-- then the version is considered to be \'deprecated\' and can be deleted
-- by Secrets Manager.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   secretsmanager:UpdateSecretVersionStage
--
-- __Related operations__
--
-- -   To get the list of staging labels that are currently associated with
--     a version of a secret, use @ DescribeSecret @ and examine the
--     @SecretVersionsToStages@ response value.
module Network.AWS.SecretsManager.UpdateSecretVersionStage
  ( -- * Creating a Request
    UpdateSecretVersionStage (..),
    newUpdateSecretVersionStage,

    -- * Request Lenses
    updateSecretVersionStage_removeFromVersionId,
    updateSecretVersionStage_moveToVersionId,
    updateSecretVersionStage_secretId,
    updateSecretVersionStage_versionStage,

    -- * Destructuring the Response
    UpdateSecretVersionStageResponse (..),
    newUpdateSecretVersionStageResponse,

    -- * Response Lenses
    updateSecretVersionStageResponse_arn,
    updateSecretVersionStageResponse_name,
    updateSecretVersionStageResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newUpdateSecretVersionStage' smart constructor.
data UpdateSecretVersionStage = UpdateSecretVersionStage'
  { -- | Specifies the secret version ID of the version that the staging label is
    -- to be removed from. If the staging label you are trying to attach to one
    -- version is already attached to a different version, then you must
    -- include this parameter and specify the version that the label is to be
    -- removed from. If the label is attached and you either do not specify
    -- this parameter, or the version ID does not match, then the operation
    -- fails.
    removeFromVersionId :: Prelude.Maybe Prelude.Text,
    -- | (Optional) The secret version ID that you want to add the staging label.
    -- If you want to remove a label from a version, then do not specify this
    -- parameter.
    --
    -- If the staging label is already attached to a different version of the
    -- secret, then you must also specify the @RemoveFromVersionId@ parameter.
    moveToVersionId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the secret with the version with the list of staging labels
    -- you want to modify. You can specify either the Amazon Resource Name
    -- (ARN) or the friendly name of the secret.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN.
    secretId :: Prelude.Text,
    -- | The staging label to add to this version.
    versionStage :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecretVersionStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'removeFromVersionId', 'updateSecretVersionStage_removeFromVersionId' - Specifies the secret version ID of the version that the staging label is
-- to be removed from. If the staging label you are trying to attach to one
-- version is already attached to a different version, then you must
-- include this parameter and specify the version that the label is to be
-- removed from. If the label is attached and you either do not specify
-- this parameter, or the version ID does not match, then the operation
-- fails.
--
-- 'moveToVersionId', 'updateSecretVersionStage_moveToVersionId' - (Optional) The secret version ID that you want to add the staging label.
-- If you want to remove a label from a version, then do not specify this
-- parameter.
--
-- If the staging label is already attached to a different version of the
-- secret, then you must also specify the @RemoveFromVersionId@ parameter.
--
-- 'secretId', 'updateSecretVersionStage_secretId' - Specifies the secret with the version with the list of staging labels
-- you want to modify. You can specify either the Amazon Resource Name
-- (ARN) or the friendly name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
--
-- 'versionStage', 'updateSecretVersionStage_versionStage' - The staging label to add to this version.
newUpdateSecretVersionStage ::
  -- | 'secretId'
  Prelude.Text ->
  -- | 'versionStage'
  Prelude.Text ->
  UpdateSecretVersionStage
newUpdateSecretVersionStage pSecretId_ pVersionStage_ =
  UpdateSecretVersionStage'
    { removeFromVersionId =
        Prelude.Nothing,
      moveToVersionId = Prelude.Nothing,
      secretId = pSecretId_,
      versionStage = pVersionStage_
    }

-- | Specifies the secret version ID of the version that the staging label is
-- to be removed from. If the staging label you are trying to attach to one
-- version is already attached to a different version, then you must
-- include this parameter and specify the version that the label is to be
-- removed from. If the label is attached and you either do not specify
-- this parameter, or the version ID does not match, then the operation
-- fails.
updateSecretVersionStage_removeFromVersionId :: Lens.Lens' UpdateSecretVersionStage (Prelude.Maybe Prelude.Text)
updateSecretVersionStage_removeFromVersionId = Lens.lens (\UpdateSecretVersionStage' {removeFromVersionId} -> removeFromVersionId) (\s@UpdateSecretVersionStage' {} a -> s {removeFromVersionId = a} :: UpdateSecretVersionStage)

-- | (Optional) The secret version ID that you want to add the staging label.
-- If you want to remove a label from a version, then do not specify this
-- parameter.
--
-- If the staging label is already attached to a different version of the
-- secret, then you must also specify the @RemoveFromVersionId@ parameter.
updateSecretVersionStage_moveToVersionId :: Lens.Lens' UpdateSecretVersionStage (Prelude.Maybe Prelude.Text)
updateSecretVersionStage_moveToVersionId = Lens.lens (\UpdateSecretVersionStage' {moveToVersionId} -> moveToVersionId) (\s@UpdateSecretVersionStage' {} a -> s {moveToVersionId = a} :: UpdateSecretVersionStage)

-- | Specifies the secret with the version with the list of staging labels
-- you want to modify. You can specify either the Amazon Resource Name
-- (ARN) or the friendly name of the secret.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN.
updateSecretVersionStage_secretId :: Lens.Lens' UpdateSecretVersionStage Prelude.Text
updateSecretVersionStage_secretId = Lens.lens (\UpdateSecretVersionStage' {secretId} -> secretId) (\s@UpdateSecretVersionStage' {} a -> s {secretId = a} :: UpdateSecretVersionStage)

-- | The staging label to add to this version.
updateSecretVersionStage_versionStage :: Lens.Lens' UpdateSecretVersionStage Prelude.Text
updateSecretVersionStage_versionStage = Lens.lens (\UpdateSecretVersionStage' {versionStage} -> versionStage) (\s@UpdateSecretVersionStage' {} a -> s {versionStage = a} :: UpdateSecretVersionStage)

instance Core.AWSRequest UpdateSecretVersionStage where
  type
    AWSResponse UpdateSecretVersionStage =
      UpdateSecretVersionStageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSecretVersionStageResponse'
            Prelude.<$> (x Core..?> "ARN")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSecretVersionStage

instance Prelude.NFData UpdateSecretVersionStage

instance Core.ToHeaders UpdateSecretVersionStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.UpdateSecretVersionStage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSecretVersionStage where
  toJSON UpdateSecretVersionStage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RemoveFromVersionId" Core..=)
              Prelude.<$> removeFromVersionId,
            ("MoveToVersionId" Core..=)
              Prelude.<$> moveToVersionId,
            Prelude.Just ("SecretId" Core..= secretId),
            Prelude.Just ("VersionStage" Core..= versionStage)
          ]
      )

instance Core.ToPath UpdateSecretVersionStage where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateSecretVersionStage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSecretVersionStageResponse' smart constructor.
data UpdateSecretVersionStageResponse = UpdateSecretVersionStageResponse'
  { -- | The ARN of the secret with the modified staging label.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The friendly name of the secret with the modified staging label.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecretVersionStageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updateSecretVersionStageResponse_arn' - The ARN of the secret with the modified staging label.
--
-- 'name', 'updateSecretVersionStageResponse_name' - The friendly name of the secret with the modified staging label.
--
-- 'httpStatus', 'updateSecretVersionStageResponse_httpStatus' - The response's http status code.
newUpdateSecretVersionStageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSecretVersionStageResponse
newUpdateSecretVersionStageResponse pHttpStatus_ =
  UpdateSecretVersionStageResponse'
    { arn =
        Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the secret with the modified staging label.
updateSecretVersionStageResponse_arn :: Lens.Lens' UpdateSecretVersionStageResponse (Prelude.Maybe Prelude.Text)
updateSecretVersionStageResponse_arn = Lens.lens (\UpdateSecretVersionStageResponse' {arn} -> arn) (\s@UpdateSecretVersionStageResponse' {} a -> s {arn = a} :: UpdateSecretVersionStageResponse)

-- | The friendly name of the secret with the modified staging label.
updateSecretVersionStageResponse_name :: Lens.Lens' UpdateSecretVersionStageResponse (Prelude.Maybe Prelude.Text)
updateSecretVersionStageResponse_name = Lens.lens (\UpdateSecretVersionStageResponse' {name} -> name) (\s@UpdateSecretVersionStageResponse' {} a -> s {name = a} :: UpdateSecretVersionStageResponse)

-- | The response's http status code.
updateSecretVersionStageResponse_httpStatus :: Lens.Lens' UpdateSecretVersionStageResponse Prelude.Int
updateSecretVersionStageResponse_httpStatus = Lens.lens (\UpdateSecretVersionStageResponse' {httpStatus} -> httpStatus) (\s@UpdateSecretVersionStageResponse' {} a -> s {httpStatus = a} :: UpdateSecretVersionStageResponse)

instance
  Prelude.NFData
    UpdateSecretVersionStageResponse
