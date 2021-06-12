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
-- in the /AWS Secrets Manager User Guide/.
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
    removeFromVersionId :: Core.Maybe Core.Text,
    -- | (Optional) The secret version ID that you want to add the staging label.
    -- If you want to remove a label from a version, then do not specify this
    -- parameter.
    --
    -- If the staging label is already attached to a different version of the
    -- secret, then you must also specify the @RemoveFromVersionId@ parameter.
    moveToVersionId :: Core.Maybe Core.Text,
    -- | Specifies the secret with the version with the list of staging labels
    -- you want to modify. You can specify either the Amazon Resource Name
    -- (ARN) or the friendly name of the secret.
    --
    -- If you specify an ARN, we generally recommend that you specify a
    -- complete ARN. You can specify a partial ARN too—for example, if you
    -- don’t include the final hyphen and six random characters that Secrets
    -- Manager adds at the end of the ARN when you created the secret. A
    -- partial ARN match can work as long as it uniquely matches only one
    -- secret. However, if your secret has a name that ends in a hyphen
    -- followed by six characters (before Secrets Manager adds the hyphen and
    -- six characters to the ARN) and you try to use that as a partial ARN,
    -- then those characters cause Secrets Manager to assume that you’re
    -- specifying a complete ARN. This confusion can cause unexpected results.
    -- To avoid this situation, we recommend that you don’t create secret names
    -- ending with a hyphen followed by six characters.
    --
    -- If you specify an incomplete ARN without the random suffix, and instead
    -- provide the \'friendly name\', you /must/ not include the random suffix.
    -- If you do include the random suffix added by Secrets Manager, you
    -- receive either a /ResourceNotFoundException/ or an
    -- /AccessDeniedException/ error, depending on your permissions.
    secretId :: Core.Text,
    -- | The staging label to add to this version.
    versionStage :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- If you specify an ARN, we generally recommend that you specify a
-- complete ARN. You can specify a partial ARN too—for example, if you
-- don’t include the final hyphen and six random characters that Secrets
-- Manager adds at the end of the ARN when you created the secret. A
-- partial ARN match can work as long as it uniquely matches only one
-- secret. However, if your secret has a name that ends in a hyphen
-- followed by six characters (before Secrets Manager adds the hyphen and
-- six characters to the ARN) and you try to use that as a partial ARN,
-- then those characters cause Secrets Manager to assume that you’re
-- specifying a complete ARN. This confusion can cause unexpected results.
-- To avoid this situation, we recommend that you don’t create secret names
-- ending with a hyphen followed by six characters.
--
-- If you specify an incomplete ARN without the random suffix, and instead
-- provide the \'friendly name\', you /must/ not include the random suffix.
-- If you do include the random suffix added by Secrets Manager, you
-- receive either a /ResourceNotFoundException/ or an
-- /AccessDeniedException/ error, depending on your permissions.
--
-- 'versionStage', 'updateSecretVersionStage_versionStage' - The staging label to add to this version.
newUpdateSecretVersionStage ::
  -- | 'secretId'
  Core.Text ->
  -- | 'versionStage'
  Core.Text ->
  UpdateSecretVersionStage
newUpdateSecretVersionStage pSecretId_ pVersionStage_ =
  UpdateSecretVersionStage'
    { removeFromVersionId =
        Core.Nothing,
      moveToVersionId = Core.Nothing,
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
updateSecretVersionStage_removeFromVersionId :: Lens.Lens' UpdateSecretVersionStage (Core.Maybe Core.Text)
updateSecretVersionStage_removeFromVersionId = Lens.lens (\UpdateSecretVersionStage' {removeFromVersionId} -> removeFromVersionId) (\s@UpdateSecretVersionStage' {} a -> s {removeFromVersionId = a} :: UpdateSecretVersionStage)

-- | (Optional) The secret version ID that you want to add the staging label.
-- If you want to remove a label from a version, then do not specify this
-- parameter.
--
-- If the staging label is already attached to a different version of the
-- secret, then you must also specify the @RemoveFromVersionId@ parameter.
updateSecretVersionStage_moveToVersionId :: Lens.Lens' UpdateSecretVersionStage (Core.Maybe Core.Text)
updateSecretVersionStage_moveToVersionId = Lens.lens (\UpdateSecretVersionStage' {moveToVersionId} -> moveToVersionId) (\s@UpdateSecretVersionStage' {} a -> s {moveToVersionId = a} :: UpdateSecretVersionStage)

-- | Specifies the secret with the version with the list of staging labels
-- you want to modify. You can specify either the Amazon Resource Name
-- (ARN) or the friendly name of the secret.
--
-- If you specify an ARN, we generally recommend that you specify a
-- complete ARN. You can specify a partial ARN too—for example, if you
-- don’t include the final hyphen and six random characters that Secrets
-- Manager adds at the end of the ARN when you created the secret. A
-- partial ARN match can work as long as it uniquely matches only one
-- secret. However, if your secret has a name that ends in a hyphen
-- followed by six characters (before Secrets Manager adds the hyphen and
-- six characters to the ARN) and you try to use that as a partial ARN,
-- then those characters cause Secrets Manager to assume that you’re
-- specifying a complete ARN. This confusion can cause unexpected results.
-- To avoid this situation, we recommend that you don’t create secret names
-- ending with a hyphen followed by six characters.
--
-- If you specify an incomplete ARN without the random suffix, and instead
-- provide the \'friendly name\', you /must/ not include the random suffix.
-- If you do include the random suffix added by Secrets Manager, you
-- receive either a /ResourceNotFoundException/ or an
-- /AccessDeniedException/ error, depending on your permissions.
updateSecretVersionStage_secretId :: Lens.Lens' UpdateSecretVersionStage Core.Text
updateSecretVersionStage_secretId = Lens.lens (\UpdateSecretVersionStage' {secretId} -> secretId) (\s@UpdateSecretVersionStage' {} a -> s {secretId = a} :: UpdateSecretVersionStage)

-- | The staging label to add to this version.
updateSecretVersionStage_versionStage :: Lens.Lens' UpdateSecretVersionStage Core.Text
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
            Core.<$> (x Core..?> "ARN")
            Core.<*> (x Core..?> "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateSecretVersionStage

instance Core.NFData UpdateSecretVersionStage

instance Core.ToHeaders UpdateSecretVersionStage where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.UpdateSecretVersionStage" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateSecretVersionStage where
  toJSON UpdateSecretVersionStage' {..} =
    Core.object
      ( Core.catMaybes
          [ ("RemoveFromVersionId" Core..=)
              Core.<$> removeFromVersionId,
            ("MoveToVersionId" Core..=) Core.<$> moveToVersionId,
            Core.Just ("SecretId" Core..= secretId),
            Core.Just ("VersionStage" Core..= versionStage)
          ]
      )

instance Core.ToPath UpdateSecretVersionStage where
  toPath = Core.const "/"

instance Core.ToQuery UpdateSecretVersionStage where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateSecretVersionStageResponse' smart constructor.
data UpdateSecretVersionStageResponse = UpdateSecretVersionStageResponse'
  { -- | The ARN of the secret with the modified staging label.
    arn :: Core.Maybe Core.Text,
    -- | The friendly name of the secret with the modified staging label.
    name :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateSecretVersionStageResponse
newUpdateSecretVersionStageResponse pHttpStatus_ =
  UpdateSecretVersionStageResponse'
    { arn =
        Core.Nothing,
      name = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the secret with the modified staging label.
updateSecretVersionStageResponse_arn :: Lens.Lens' UpdateSecretVersionStageResponse (Core.Maybe Core.Text)
updateSecretVersionStageResponse_arn = Lens.lens (\UpdateSecretVersionStageResponse' {arn} -> arn) (\s@UpdateSecretVersionStageResponse' {} a -> s {arn = a} :: UpdateSecretVersionStageResponse)

-- | The friendly name of the secret with the modified staging label.
updateSecretVersionStageResponse_name :: Lens.Lens' UpdateSecretVersionStageResponse (Core.Maybe Core.Text)
updateSecretVersionStageResponse_name = Lens.lens (\UpdateSecretVersionStageResponse' {name} -> name) (\s@UpdateSecretVersionStageResponse' {} a -> s {name = a} :: UpdateSecretVersionStageResponse)

-- | The response's http status code.
updateSecretVersionStageResponse_httpStatus :: Lens.Lens' UpdateSecretVersionStageResponse Core.Int
updateSecretVersionStageResponse_httpStatus = Lens.lens (\UpdateSecretVersionStageResponse' {httpStatus} -> httpStatus) (\s@UpdateSecretVersionStageResponse' {} a -> s {httpStatus = a} :: UpdateSecretVersionStageResponse)

instance Core.NFData UpdateSecretVersionStageResponse
