{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.ApplyPendingMaintenanceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a pending maintenance action to a resource (for example, to a replication instance).
module Network.AWS.DMS.ApplyPendingMaintenanceAction
  ( -- * Creating a request
    ApplyPendingMaintenanceAction (..),
    mkApplyPendingMaintenanceAction,

    -- ** Request lenses
    apmaOptInType,
    apmaApplyAction,
    apmaReplicationInstanceARN,

    -- * Destructuring the response
    ApplyPendingMaintenanceActionResponse (..),
    mkApplyPendingMaintenanceActionResponse,

    -- ** Response lenses
    apmarsResourcePendingMaintenanceActions,
    apmarsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkApplyPendingMaintenanceAction' smart constructor.
data ApplyPendingMaintenanceAction = ApplyPendingMaintenanceAction'
  { -- | A value that specifies the type of opt-in request, or undoes an opt-in request. You can't undo an opt-in request of type @immediate@ .
    --
    -- Valid values:
    --
    --     * @immediate@ - Apply the maintenance action immediately.
    --
    --
    --     * @next-maintenance@ - Apply the maintenance action during the next maintenance window for the resource.
    --
    --
    --     * @undo-opt-in@ - Cancel any existing @next-maintenance@ opt-in requests.
    optInType :: Lude.Text,
    -- | The pending maintenance action to apply to this resource.
    applyAction :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the AWS DMS resource that the pending maintenance action applies to.
    replicationInstanceARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplyPendingMaintenanceAction' with the minimum fields required to make a request.
--
-- * 'optInType' - A value that specifies the type of opt-in request, or undoes an opt-in request. You can't undo an opt-in request of type @immediate@ .
--
-- Valid values:
--
--     * @immediate@ - Apply the maintenance action immediately.
--
--
--     * @next-maintenance@ - Apply the maintenance action during the next maintenance window for the resource.
--
--
--     * @undo-opt-in@ - Cancel any existing @next-maintenance@ opt-in requests.
--
--
-- * 'applyAction' - The pending maintenance action to apply to this resource.
-- * 'replicationInstanceARN' - The Amazon Resource Name (ARN) of the AWS DMS resource that the pending maintenance action applies to.
mkApplyPendingMaintenanceAction ::
  -- | 'optInType'
  Lude.Text ->
  -- | 'applyAction'
  Lude.Text ->
  -- | 'replicationInstanceARN'
  Lude.Text ->
  ApplyPendingMaintenanceAction
mkApplyPendingMaintenanceAction
  pOptInType_
  pApplyAction_
  pReplicationInstanceARN_ =
    ApplyPendingMaintenanceAction'
      { optInType = pOptInType_,
        applyAction = pApplyAction_,
        replicationInstanceARN = pReplicationInstanceARN_
      }

-- | A value that specifies the type of opt-in request, or undoes an opt-in request. You can't undo an opt-in request of type @immediate@ .
--
-- Valid values:
--
--     * @immediate@ - Apply the maintenance action immediately.
--
--
--     * @next-maintenance@ - Apply the maintenance action during the next maintenance window for the resource.
--
--
--     * @undo-opt-in@ - Cancel any existing @next-maintenance@ opt-in requests.
--
--
--
-- /Note:/ Consider using 'optInType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apmaOptInType :: Lens.Lens' ApplyPendingMaintenanceAction Lude.Text
apmaOptInType = Lens.lens (optInType :: ApplyPendingMaintenanceAction -> Lude.Text) (\s a -> s {optInType = a} :: ApplyPendingMaintenanceAction)
{-# DEPRECATED apmaOptInType "Use generic-lens or generic-optics with 'optInType' instead." #-}

-- | The pending maintenance action to apply to this resource.
--
-- /Note:/ Consider using 'applyAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apmaApplyAction :: Lens.Lens' ApplyPendingMaintenanceAction Lude.Text
apmaApplyAction = Lens.lens (applyAction :: ApplyPendingMaintenanceAction -> Lude.Text) (\s a -> s {applyAction = a} :: ApplyPendingMaintenanceAction)
{-# DEPRECATED apmaApplyAction "Use generic-lens or generic-optics with 'applyAction' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS DMS resource that the pending maintenance action applies to.
--
-- /Note:/ Consider using 'replicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apmaReplicationInstanceARN :: Lens.Lens' ApplyPendingMaintenanceAction Lude.Text
apmaReplicationInstanceARN = Lens.lens (replicationInstanceARN :: ApplyPendingMaintenanceAction -> Lude.Text) (\s a -> s {replicationInstanceARN = a} :: ApplyPendingMaintenanceAction)
{-# DEPRECATED apmaReplicationInstanceARN "Use generic-lens or generic-optics with 'replicationInstanceARN' instead." #-}

instance Lude.AWSRequest ApplyPendingMaintenanceAction where
  type
    Rs ApplyPendingMaintenanceAction =
      ApplyPendingMaintenanceActionResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ApplyPendingMaintenanceActionResponse'
            Lude.<$> (x Lude..?> "ResourcePendingMaintenanceActions")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ApplyPendingMaintenanceAction where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonDMSv20160101.ApplyPendingMaintenanceAction" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ApplyPendingMaintenanceAction where
  toJSON ApplyPendingMaintenanceAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("OptInType" Lude..= optInType),
            Lude.Just ("ApplyAction" Lude..= applyAction),
            Lude.Just
              ("ReplicationInstanceArn" Lude..= replicationInstanceARN)
          ]
      )

instance Lude.ToPath ApplyPendingMaintenanceAction where
  toPath = Lude.const "/"

instance Lude.ToQuery ApplyPendingMaintenanceAction where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkApplyPendingMaintenanceActionResponse' smart constructor.
data ApplyPendingMaintenanceActionResponse = ApplyPendingMaintenanceActionResponse'
  { -- | The AWS DMS resource that the pending maintenance action will be applied to.
    resourcePendingMaintenanceActions :: Lude.Maybe ResourcePendingMaintenanceActions,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplyPendingMaintenanceActionResponse' with the minimum fields required to make a request.
--
-- * 'resourcePendingMaintenanceActions' - The AWS DMS resource that the pending maintenance action will be applied to.
-- * 'responseStatus' - The response status code.
mkApplyPendingMaintenanceActionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ApplyPendingMaintenanceActionResponse
mkApplyPendingMaintenanceActionResponse pResponseStatus_ =
  ApplyPendingMaintenanceActionResponse'
    { resourcePendingMaintenanceActions =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The AWS DMS resource that the pending maintenance action will be applied to.
--
-- /Note:/ Consider using 'resourcePendingMaintenanceActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apmarsResourcePendingMaintenanceActions :: Lens.Lens' ApplyPendingMaintenanceActionResponse (Lude.Maybe ResourcePendingMaintenanceActions)
apmarsResourcePendingMaintenanceActions = Lens.lens (resourcePendingMaintenanceActions :: ApplyPendingMaintenanceActionResponse -> Lude.Maybe ResourcePendingMaintenanceActions) (\s a -> s {resourcePendingMaintenanceActions = a} :: ApplyPendingMaintenanceActionResponse)
{-# DEPRECATED apmarsResourcePendingMaintenanceActions "Use generic-lens or generic-optics with 'resourcePendingMaintenanceActions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apmarsResponseStatus :: Lens.Lens' ApplyPendingMaintenanceActionResponse Lude.Int
apmarsResponseStatus = Lens.lens (responseStatus :: ApplyPendingMaintenanceActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ApplyPendingMaintenanceActionResponse)
{-# DEPRECATED apmarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
