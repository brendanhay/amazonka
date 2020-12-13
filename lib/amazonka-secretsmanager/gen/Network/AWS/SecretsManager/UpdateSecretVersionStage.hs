{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.UpdateSecretVersionStage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the staging labels attached to a version of a secret. Staging labels are used to track a version as it progresses through the secret rotation process. You can attach a staging label to only one version of a secret at a time. If a staging label to be added is already attached to another version, then it is moved--removed from the other version first and then attached to this one. For more information about staging labels, see <https://docs.aws.amazon.com/secretsmanager/latest/userguide/terms-concepts.html#term_staging-label Staging Labels> in the /AWS Secrets Manager User Guide/ .
--
-- The staging labels that you specify in the @VersionStage@ parameter are added to the existing list of staging labels--they don't replace it.
-- You can move the @AWSCURRENT@ staging label to this version by including it in this call.
-- If this action results in the last label being removed from a version, then the version is considered to be 'deprecated' and can be deleted by Secrets Manager.
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:UpdateSecretVersionStage
--
--
-- __Related operations__
--
--     * To get the list of staging labels that are currently associated with a version of a secret, use @'DescribeSecret' @ and examine the @SecretVersionsToStages@ response value.
module Network.AWS.SecretsManager.UpdateSecretVersionStage
  ( -- * Creating a request
    UpdateSecretVersionStage (..),
    mkUpdateSecretVersionStage,

    -- ** Request lenses
    usvsRemoveFromVersionId,
    usvsSecretId,
    usvsVersionStage,
    usvsMoveToVersionId,

    -- * Destructuring the response
    UpdateSecretVersionStageResponse (..),
    mkUpdateSecretVersionStageResponse,

    -- ** Response lenses
    usvsrsARN,
    usvsrsName,
    usvsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SecretsManager.Types

-- | /See:/ 'mkUpdateSecretVersionStage' smart constructor.
data UpdateSecretVersionStage = UpdateSecretVersionStage'
  { -- | Specifies the secret version ID of the version that the staging label is to be removed from. If the staging label you are trying to attach to one version is already attached to a different version, then you must include this parameter and specify the version that the label is to be removed from. If the label is attached and you either do not specify this parameter, or the version ID does not match, then the operation fails.
    removeFromVersionId :: Lude.Maybe Lude.Text,
    -- | Specifies the secret with the version with the list of staging labels you want to modify. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
    secretId :: Lude.Text,
    -- | The staging label to add to this version.
    versionStage :: Lude.Text,
    -- | (Optional) The secret version ID that you want to add the staging label. If you want to remove a label from a version, then do not specify this parameter.
    --
    -- If the staging label is already attached to a different version of the secret, then you must also specify the @RemoveFromVersionId@ parameter.
    moveToVersionId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSecretVersionStage' with the minimum fields required to make a request.
--
-- * 'removeFromVersionId' - Specifies the secret version ID of the version that the staging label is to be removed from. If the staging label you are trying to attach to one version is already attached to a different version, then you must include this parameter and specify the version that the label is to be removed from. If the label is attached and you either do not specify this parameter, or the version ID does not match, then the operation fails.
-- * 'secretId' - Specifies the secret with the version with the list of staging labels you want to modify. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
-- * 'versionStage' - The staging label to add to this version.
-- * 'moveToVersionId' - (Optional) The secret version ID that you want to add the staging label. If you want to remove a label from a version, then do not specify this parameter.
--
-- If the staging label is already attached to a different version of the secret, then you must also specify the @RemoveFromVersionId@ parameter.
mkUpdateSecretVersionStage ::
  -- | 'secretId'
  Lude.Text ->
  -- | 'versionStage'
  Lude.Text ->
  UpdateSecretVersionStage
mkUpdateSecretVersionStage pSecretId_ pVersionStage_ =
  UpdateSecretVersionStage'
    { removeFromVersionId = Lude.Nothing,
      secretId = pSecretId_,
      versionStage = pVersionStage_,
      moveToVersionId = Lude.Nothing
    }

-- | Specifies the secret version ID of the version that the staging label is to be removed from. If the staging label you are trying to attach to one version is already attached to a different version, then you must include this parameter and specify the version that the label is to be removed from. If the label is attached and you either do not specify this parameter, or the version ID does not match, then the operation fails.
--
-- /Note:/ Consider using 'removeFromVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usvsRemoveFromVersionId :: Lens.Lens' UpdateSecretVersionStage (Lude.Maybe Lude.Text)
usvsRemoveFromVersionId = Lens.lens (removeFromVersionId :: UpdateSecretVersionStage -> Lude.Maybe Lude.Text) (\s a -> s {removeFromVersionId = a} :: UpdateSecretVersionStage)
{-# DEPRECATED usvsRemoveFromVersionId "Use generic-lens or generic-optics with 'removeFromVersionId' instead." #-}

-- | Specifies the secret with the version with the list of staging labels you want to modify. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usvsSecretId :: Lens.Lens' UpdateSecretVersionStage Lude.Text
usvsSecretId = Lens.lens (secretId :: UpdateSecretVersionStage -> Lude.Text) (\s a -> s {secretId = a} :: UpdateSecretVersionStage)
{-# DEPRECATED usvsSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

-- | The staging label to add to this version.
--
-- /Note:/ Consider using 'versionStage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usvsVersionStage :: Lens.Lens' UpdateSecretVersionStage Lude.Text
usvsVersionStage = Lens.lens (versionStage :: UpdateSecretVersionStage -> Lude.Text) (\s a -> s {versionStage = a} :: UpdateSecretVersionStage)
{-# DEPRECATED usvsVersionStage "Use generic-lens or generic-optics with 'versionStage' instead." #-}

-- | (Optional) The secret version ID that you want to add the staging label. If you want to remove a label from a version, then do not specify this parameter.
--
-- If the staging label is already attached to a different version of the secret, then you must also specify the @RemoveFromVersionId@ parameter.
--
-- /Note:/ Consider using 'moveToVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usvsMoveToVersionId :: Lens.Lens' UpdateSecretVersionStage (Lude.Maybe Lude.Text)
usvsMoveToVersionId = Lens.lens (moveToVersionId :: UpdateSecretVersionStage -> Lude.Maybe Lude.Text) (\s a -> s {moveToVersionId = a} :: UpdateSecretVersionStage)
{-# DEPRECATED usvsMoveToVersionId "Use generic-lens or generic-optics with 'moveToVersionId' instead." #-}

instance Lude.AWSRequest UpdateSecretVersionStage where
  type Rs UpdateSecretVersionStage = UpdateSecretVersionStageResponse
  request = Req.postJSON secretsManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateSecretVersionStageResponse'
            Lude.<$> (x Lude..?> "ARN")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSecretVersionStage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("secretsmanager.UpdateSecretVersionStage" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateSecretVersionStage where
  toJSON UpdateSecretVersionStage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RemoveFromVersionId" Lude..=) Lude.<$> removeFromVersionId,
            Lude.Just ("SecretId" Lude..= secretId),
            Lude.Just ("VersionStage" Lude..= versionStage),
            ("MoveToVersionId" Lude..=) Lude.<$> moveToVersionId
          ]
      )

instance Lude.ToPath UpdateSecretVersionStage where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateSecretVersionStage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateSecretVersionStageResponse' smart constructor.
data UpdateSecretVersionStageResponse = UpdateSecretVersionStageResponse'
  { -- | The ARN of the secret with the modified staging label.
    arn :: Lude.Maybe Lude.Text,
    -- | The friendly name of the secret with the modified staging label.
    name :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSecretVersionStageResponse' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the secret with the modified staging label.
-- * 'name' - The friendly name of the secret with the modified staging label.
-- * 'responseStatus' - The response status code.
mkUpdateSecretVersionStageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSecretVersionStageResponse
mkUpdateSecretVersionStageResponse pResponseStatus_ =
  UpdateSecretVersionStageResponse'
    { arn = Lude.Nothing,
      name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ARN of the secret with the modified staging label.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usvsrsARN :: Lens.Lens' UpdateSecretVersionStageResponse (Lude.Maybe Lude.Text)
usvsrsARN = Lens.lens (arn :: UpdateSecretVersionStageResponse -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: UpdateSecretVersionStageResponse)
{-# DEPRECATED usvsrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the secret with the modified staging label.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usvsrsName :: Lens.Lens' UpdateSecretVersionStageResponse (Lude.Maybe Lude.Text)
usvsrsName = Lens.lens (name :: UpdateSecretVersionStageResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateSecretVersionStageResponse)
{-# DEPRECATED usvsrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usvsrsResponseStatus :: Lens.Lens' UpdateSecretVersionStageResponse Lude.Int
usvsrsResponseStatus = Lens.lens (responseStatus :: UpdateSecretVersionStageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSecretVersionStageResponse)
{-# DEPRECATED usvsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
