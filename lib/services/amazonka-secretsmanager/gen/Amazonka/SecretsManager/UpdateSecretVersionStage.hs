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
-- Module      : Amazonka.SecretsManager.UpdateSecretVersionStage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the staging labels attached to a version of a secret. Secrets
-- Manager uses staging labels to track a version as it progresses through
-- the secret rotation process. Each staging label can be attached to only
-- one version at a time. To add a staging label to a version when it is
-- already attached to another version, Secrets Manager first removes it
-- from the other version first and then attaches it to this one. For more
-- information about versions and staging labels, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/getting-started.html#term_version Concepts: Version>.
--
-- The staging labels that you specify in the @VersionStage@ parameter are
-- added to the existing list of staging labels for the version.
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
-- Secrets Manager generates a CloudTrail log entry when you call this
-- action. Do not include sensitive information in request parameters
-- because it might be logged. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/retrieve-ct-entries.html Logging Secrets Manager events with CloudTrail>.
--
-- __Required permissions:__ @secretsmanager:UpdateSecretVersionStage@. For
-- more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#reference_iam-permissions_actions IAM policy actions for Secrets Manager>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control in Secrets Manager>.
module Amazonka.SecretsManager.UpdateSecretVersionStage
  ( -- * Creating a Request
    UpdateSecretVersionStage (..),
    newUpdateSecretVersionStage,

    -- * Request Lenses
    updateSecretVersionStage_moveToVersionId,
    updateSecretVersionStage_removeFromVersionId,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newUpdateSecretVersionStage' smart constructor.
data UpdateSecretVersionStage = UpdateSecretVersionStage'
  { -- | The ID of the version to add the staging label to. To remove a label
    -- from a version, then do not specify this parameter.
    --
    -- If the staging label is already attached to a different version of the
    -- secret, then you must also specify the @RemoveFromVersionId@ parameter.
    moveToVersionId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the version that the staging label is to be removed from. If
    -- the staging label you are trying to attach to one version is already
    -- attached to a different version, then you must include this parameter
    -- and specify the version that the label is to be removed from. If the
    -- label is attached and you either do not specify this parameter, or the
    -- version ID does not match, then the operation fails.
    removeFromVersionId :: Prelude.Maybe Prelude.Text,
    -- | The ARN or the name of the secret with the version and staging labelsto
    -- modify.
    --
    -- For an ARN, we recommend that you specify a complete ARN rather than a
    -- partial ARN. See
    -- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
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
-- 'moveToVersionId', 'updateSecretVersionStage_moveToVersionId' - The ID of the version to add the staging label to. To remove a label
-- from a version, then do not specify this parameter.
--
-- If the staging label is already attached to a different version of the
-- secret, then you must also specify the @RemoveFromVersionId@ parameter.
--
-- 'removeFromVersionId', 'updateSecretVersionStage_removeFromVersionId' - The ID of the version that the staging label is to be removed from. If
-- the staging label you are trying to attach to one version is already
-- attached to a different version, then you must include this parameter
-- and specify the version that the label is to be removed from. If the
-- label is attached and you either do not specify this parameter, or the
-- version ID does not match, then the operation fails.
--
-- 'secretId', 'updateSecretVersionStage_secretId' - The ARN or the name of the secret with the version and staging labelsto
-- modify.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
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
    { moveToVersionId =
        Prelude.Nothing,
      removeFromVersionId = Prelude.Nothing,
      secretId = pSecretId_,
      versionStage = pVersionStage_
    }

-- | The ID of the version to add the staging label to. To remove a label
-- from a version, then do not specify this parameter.
--
-- If the staging label is already attached to a different version of the
-- secret, then you must also specify the @RemoveFromVersionId@ parameter.
updateSecretVersionStage_moveToVersionId :: Lens.Lens' UpdateSecretVersionStage (Prelude.Maybe Prelude.Text)
updateSecretVersionStage_moveToVersionId = Lens.lens (\UpdateSecretVersionStage' {moveToVersionId} -> moveToVersionId) (\s@UpdateSecretVersionStage' {} a -> s {moveToVersionId = a} :: UpdateSecretVersionStage)

-- | The ID of the version that the staging label is to be removed from. If
-- the staging label you are trying to attach to one version is already
-- attached to a different version, then you must include this parameter
-- and specify the version that the label is to be removed from. If the
-- label is attached and you either do not specify this parameter, or the
-- version ID does not match, then the operation fails.
updateSecretVersionStage_removeFromVersionId :: Lens.Lens' UpdateSecretVersionStage (Prelude.Maybe Prelude.Text)
updateSecretVersionStage_removeFromVersionId = Lens.lens (\UpdateSecretVersionStage' {removeFromVersionId} -> removeFromVersionId) (\s@UpdateSecretVersionStage' {} a -> s {removeFromVersionId = a} :: UpdateSecretVersionStage)

-- | The ARN or the name of the secret with the version and staging labelsto
-- modify.
--
-- For an ARN, we recommend that you specify a complete ARN rather than a
-- partial ARN. See
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/troubleshoot.html#ARN_secretnamehyphen Finding a secret from a partial ARN>.
updateSecretVersionStage_secretId :: Lens.Lens' UpdateSecretVersionStage Prelude.Text
updateSecretVersionStage_secretId = Lens.lens (\UpdateSecretVersionStage' {secretId} -> secretId) (\s@UpdateSecretVersionStage' {} a -> s {secretId = a} :: UpdateSecretVersionStage)

-- | The staging label to add to this version.
updateSecretVersionStage_versionStage :: Lens.Lens' UpdateSecretVersionStage Prelude.Text
updateSecretVersionStage_versionStage = Lens.lens (\UpdateSecretVersionStage' {versionStage} -> versionStage) (\s@UpdateSecretVersionStage' {} a -> s {versionStage = a} :: UpdateSecretVersionStage)

instance Core.AWSRequest UpdateSecretVersionStage where
  type
    AWSResponse UpdateSecretVersionStage =
      UpdateSecretVersionStageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSecretVersionStageResponse'
            Prelude.<$> (x Data..?> "ARN")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSecretVersionStage where
  hashWithSalt _salt UpdateSecretVersionStage' {..} =
    _salt `Prelude.hashWithSalt` moveToVersionId
      `Prelude.hashWithSalt` removeFromVersionId
      `Prelude.hashWithSalt` secretId
      `Prelude.hashWithSalt` versionStage

instance Prelude.NFData UpdateSecretVersionStage where
  rnf UpdateSecretVersionStage' {..} =
    Prelude.rnf moveToVersionId
      `Prelude.seq` Prelude.rnf removeFromVersionId
      `Prelude.seq` Prelude.rnf secretId
      `Prelude.seq` Prelude.rnf versionStage

instance Data.ToHeaders UpdateSecretVersionStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "secretsmanager.UpdateSecretVersionStage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSecretVersionStage where
  toJSON UpdateSecretVersionStage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MoveToVersionId" Data..=)
              Prelude.<$> moveToVersionId,
            ("RemoveFromVersionId" Data..=)
              Prelude.<$> removeFromVersionId,
            Prelude.Just ("SecretId" Data..= secretId),
            Prelude.Just ("VersionStage" Data..= versionStage)
          ]
      )

instance Data.ToPath UpdateSecretVersionStage where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateSecretVersionStage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSecretVersionStageResponse' smart constructor.
data UpdateSecretVersionStageResponse = UpdateSecretVersionStageResponse'
  { -- | The ARN of the secret that was updated.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the secret that was updated.
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
-- 'arn', 'updateSecretVersionStageResponse_arn' - The ARN of the secret that was updated.
--
-- 'name', 'updateSecretVersionStageResponse_name' - The name of the secret that was updated.
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

-- | The ARN of the secret that was updated.
updateSecretVersionStageResponse_arn :: Lens.Lens' UpdateSecretVersionStageResponse (Prelude.Maybe Prelude.Text)
updateSecretVersionStageResponse_arn = Lens.lens (\UpdateSecretVersionStageResponse' {arn} -> arn) (\s@UpdateSecretVersionStageResponse' {} a -> s {arn = a} :: UpdateSecretVersionStageResponse)

-- | The name of the secret that was updated.
updateSecretVersionStageResponse_name :: Lens.Lens' UpdateSecretVersionStageResponse (Prelude.Maybe Prelude.Text)
updateSecretVersionStageResponse_name = Lens.lens (\UpdateSecretVersionStageResponse' {name} -> name) (\s@UpdateSecretVersionStageResponse' {} a -> s {name = a} :: UpdateSecretVersionStageResponse)

-- | The response's http status code.
updateSecretVersionStageResponse_httpStatus :: Lens.Lens' UpdateSecretVersionStageResponse Prelude.Int
updateSecretVersionStageResponse_httpStatus = Lens.lens (\UpdateSecretVersionStageResponse' {httpStatus} -> httpStatus) (\s@UpdateSecretVersionStageResponse' {} a -> s {httpStatus = a} :: UpdateSecretVersionStageResponse)

instance
  Prelude.NFData
    UpdateSecretVersionStageResponse
  where
  rnf UpdateSecretVersionStageResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
