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
    usvsSecretId,
    usvsVersionStage,
    usvsMoveToVersionId,
    usvsRemoveFromVersionId,

    -- * Destructuring the response
    UpdateSecretVersionStageResponse (..),
    mkUpdateSecretVersionStageResponse,

    -- ** Response lenses
    usvsrrsARN,
    usvsrrsName,
    usvsrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkUpdateSecretVersionStage' smart constructor.
data UpdateSecretVersionStage = UpdateSecretVersionStage'
  { -- | Specifies the secret with the version with the list of staging labels you want to modify. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
    secretId :: Types.SecretId,
    -- | The staging label to add to this version.
    versionStage :: Types.SecretVersionStageType,
    -- | (Optional) The secret version ID that you want to add the staging label. If you want to remove a label from a version, then do not specify this parameter.
    --
    -- If the staging label is already attached to a different version of the secret, then you must also specify the @RemoveFromVersionId@ parameter.
    moveToVersionId :: Core.Maybe Types.SecretVersionIdType,
    -- | Specifies the secret version ID of the version that the staging label is to be removed from. If the staging label you are trying to attach to one version is already attached to a different version, then you must include this parameter and specify the version that the label is to be removed from. If the label is attached and you either do not specify this parameter, or the version ID does not match, then the operation fails.
    removeFromVersionId :: Core.Maybe Types.SecretVersionIdType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSecretVersionStage' value with any optional fields omitted.
mkUpdateSecretVersionStage ::
  -- | 'secretId'
  Types.SecretId ->
  -- | 'versionStage'
  Types.SecretVersionStageType ->
  UpdateSecretVersionStage
mkUpdateSecretVersionStage secretId versionStage =
  UpdateSecretVersionStage'
    { secretId,
      versionStage,
      moveToVersionId = Core.Nothing,
      removeFromVersionId = Core.Nothing
    }

-- | Specifies the secret with the version with the list of staging labels you want to modify. You can specify either the Amazon Resource Name (ARN) or the friendly name of the secret.
--
-- /Note:/ Consider using 'secretId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usvsSecretId :: Lens.Lens' UpdateSecretVersionStage Types.SecretId
usvsSecretId = Lens.field @"secretId"
{-# DEPRECATED usvsSecretId "Use generic-lens or generic-optics with 'secretId' instead." #-}

-- | The staging label to add to this version.
--
-- /Note:/ Consider using 'versionStage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usvsVersionStage :: Lens.Lens' UpdateSecretVersionStage Types.SecretVersionStageType
usvsVersionStage = Lens.field @"versionStage"
{-# DEPRECATED usvsVersionStage "Use generic-lens or generic-optics with 'versionStage' instead." #-}

-- | (Optional) The secret version ID that you want to add the staging label. If you want to remove a label from a version, then do not specify this parameter.
--
-- If the staging label is already attached to a different version of the secret, then you must also specify the @RemoveFromVersionId@ parameter.
--
-- /Note:/ Consider using 'moveToVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usvsMoveToVersionId :: Lens.Lens' UpdateSecretVersionStage (Core.Maybe Types.SecretVersionIdType)
usvsMoveToVersionId = Lens.field @"moveToVersionId"
{-# DEPRECATED usvsMoveToVersionId "Use generic-lens or generic-optics with 'moveToVersionId' instead." #-}

-- | Specifies the secret version ID of the version that the staging label is to be removed from. If the staging label you are trying to attach to one version is already attached to a different version, then you must include this parameter and specify the version that the label is to be removed from. If the label is attached and you either do not specify this parameter, or the version ID does not match, then the operation fails.
--
-- /Note:/ Consider using 'removeFromVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usvsRemoveFromVersionId :: Lens.Lens' UpdateSecretVersionStage (Core.Maybe Types.SecretVersionIdType)
usvsRemoveFromVersionId = Lens.field @"removeFromVersionId"
{-# DEPRECATED usvsRemoveFromVersionId "Use generic-lens or generic-optics with 'removeFromVersionId' instead." #-}

instance Core.FromJSON UpdateSecretVersionStage where
  toJSON UpdateSecretVersionStage {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("SecretId" Core..= secretId),
            Core.Just ("VersionStage" Core..= versionStage),
            ("MoveToVersionId" Core..=) Core.<$> moveToVersionId,
            ("RemoveFromVersionId" Core..=) Core.<$> removeFromVersionId
          ]
      )

instance Core.AWSRequest UpdateSecretVersionStage where
  type Rs UpdateSecretVersionStage = UpdateSecretVersionStageResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "secretsmanager.UpdateSecretVersionStage")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSecretVersionStageResponse'
            Core.<$> (x Core..:? "ARN")
            Core.<*> (x Core..:? "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateSecretVersionStageResponse' smart constructor.
data UpdateSecretVersionStageResponse = UpdateSecretVersionStageResponse'
  { -- | The ARN of the secret with the modified staging label.
    arn :: Core.Maybe Types.SecretARNType,
    -- | The friendly name of the secret with the modified staging label.
    name :: Core.Maybe Types.Name,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSecretVersionStageResponse' value with any optional fields omitted.
mkUpdateSecretVersionStageResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateSecretVersionStageResponse
mkUpdateSecretVersionStageResponse responseStatus =
  UpdateSecretVersionStageResponse'
    { arn = Core.Nothing,
      name = Core.Nothing,
      responseStatus
    }

-- | The ARN of the secret with the modified staging label.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usvsrrsARN :: Lens.Lens' UpdateSecretVersionStageResponse (Core.Maybe Types.SecretARNType)
usvsrrsARN = Lens.field @"arn"
{-# DEPRECATED usvsrrsARN "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The friendly name of the secret with the modified staging label.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usvsrrsName :: Lens.Lens' UpdateSecretVersionStageResponse (Core.Maybe Types.Name)
usvsrrsName = Lens.field @"name"
{-# DEPRECATED usvsrrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usvsrrsResponseStatus :: Lens.Lens' UpdateSecretVersionStageResponse Core.Int
usvsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED usvsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
