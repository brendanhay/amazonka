{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.ApplyPendingMaintenanceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a pending maintenance action to a resource (for example, to a DB instance).
module Network.AWS.RDS.ApplyPendingMaintenanceAction
  ( -- * Creating a request
    ApplyPendingMaintenanceAction (..),
    mkApplyPendingMaintenanceAction,

    -- ** Request lenses
    apmaResourceIdentifier,
    apmaApplyAction,
    apmaOptInType,

    -- * Destructuring the response
    ApplyPendingMaintenanceActionResponse (..),
    mkApplyPendingMaintenanceActionResponse,

    -- ** Response lenses
    apmarsResourcePendingMaintenanceActions,
    apmarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkApplyPendingMaintenanceAction' smart constructor.
data ApplyPendingMaintenanceAction = ApplyPendingMaintenanceAction'
  { resourceIdentifier ::
      Lude.Text,
    applyAction :: Lude.Text,
    optInType :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplyPendingMaintenanceAction' with the minimum fields required to make a request.
--
-- * 'applyAction' - The pending maintenance action to apply to this resource.
--
-- Valid values: @system-update@ , @db-upgrade@ , @hardware-maintenance@ , @ca-certificate-rotation@
-- * 'optInType' - A value that specifies the type of opt-in request, or undoes an opt-in request. An opt-in request of type @immediate@ can't be undone.
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
-- * 'resourceIdentifier' - The RDS Amazon Resource Name (ARN) of the resource that the pending maintenance action applies to. For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)> .
mkApplyPendingMaintenanceAction ::
  -- | 'resourceIdentifier'
  Lude.Text ->
  -- | 'applyAction'
  Lude.Text ->
  -- | 'optInType'
  Lude.Text ->
  ApplyPendingMaintenanceAction
mkApplyPendingMaintenanceAction
  pResourceIdentifier_
  pApplyAction_
  pOptInType_ =
    ApplyPendingMaintenanceAction'
      { resourceIdentifier =
          pResourceIdentifier_,
        applyAction = pApplyAction_,
        optInType = pOptInType_
      }

-- | The RDS Amazon Resource Name (ARN) of the resource that the pending maintenance action applies to. For information about creating an ARN, see <https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/USER_Tagging.ARN.html#USER_Tagging.ARN.Constructing Constructing an RDS Amazon Resource Name (ARN)> .
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apmaResourceIdentifier :: Lens.Lens' ApplyPendingMaintenanceAction Lude.Text
apmaResourceIdentifier = Lens.lens (resourceIdentifier :: ApplyPendingMaintenanceAction -> Lude.Text) (\s a -> s {resourceIdentifier = a} :: ApplyPendingMaintenanceAction)
{-# DEPRECATED apmaResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

-- | The pending maintenance action to apply to this resource.
--
-- Valid values: @system-update@ , @db-upgrade@ , @hardware-maintenance@ , @ca-certificate-rotation@
--
-- /Note:/ Consider using 'applyAction' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apmaApplyAction :: Lens.Lens' ApplyPendingMaintenanceAction Lude.Text
apmaApplyAction = Lens.lens (applyAction :: ApplyPendingMaintenanceAction -> Lude.Text) (\s a -> s {applyAction = a} :: ApplyPendingMaintenanceAction)
{-# DEPRECATED apmaApplyAction "Use generic-lens or generic-optics with 'applyAction' instead." #-}

-- | A value that specifies the type of opt-in request, or undoes an opt-in request. An opt-in request of type @immediate@ can't be undone.
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

instance Lude.AWSRequest ApplyPendingMaintenanceAction where
  type
    Rs ApplyPendingMaintenanceAction =
      ApplyPendingMaintenanceActionResponse
  request = Req.postQuery rdsService
  response =
    Res.receiveXMLWrapper
      "ApplyPendingMaintenanceActionResult"
      ( \s h x ->
          ApplyPendingMaintenanceActionResponse'
            Lude.<$> (x Lude..@? "ResourcePendingMaintenanceActions")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ApplyPendingMaintenanceAction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ApplyPendingMaintenanceAction where
  toPath = Lude.const "/"

instance Lude.ToQuery ApplyPendingMaintenanceAction where
  toQuery ApplyPendingMaintenanceAction' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ApplyPendingMaintenanceAction" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "ResourceIdentifier" Lude.=: resourceIdentifier,
        "ApplyAction" Lude.=: applyAction,
        "OptInType" Lude.=: optInType
      ]

-- | /See:/ 'mkApplyPendingMaintenanceActionResponse' smart constructor.
data ApplyPendingMaintenanceActionResponse = ApplyPendingMaintenanceActionResponse'
  { resourcePendingMaintenanceActions ::
      Lude.Maybe
        ResourcePendingMaintenanceActions,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplyPendingMaintenanceActionResponse' with the minimum fields required to make a request.
--
-- * 'resourcePendingMaintenanceActions' - Undocumented field.
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

-- | Undocumented field.
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
