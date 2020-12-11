{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.UpdateMitigationAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the definition for the specified mitigation action.
module Network.AWS.IoT.UpdateMitigationAction
  ( -- * Creating a request
    UpdateMitigationAction (..),
    mkUpdateMitigationAction,

    -- ** Request lenses
    umaActionParams,
    umaRoleARN,
    umaActionName,

    -- * Destructuring the response
    UpdateMitigationActionResponse (..),
    mkUpdateMitigationActionResponse,

    -- ** Response lenses
    umarsActionId,
    umarsActionARN,
    umarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateMitigationAction' smart constructor.
data UpdateMitigationAction = UpdateMitigationAction'
  { actionParams ::
      Lude.Maybe MitigationActionParams,
    roleARN :: Lude.Maybe Lude.Text,
    actionName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMitigationAction' with the minimum fields required to make a request.
--
-- * 'actionName' - The friendly name for the mitigation action. You can't change the name by using @UpdateMitigationAction@ . Instead, you must delete and re-create the mitigation action with the new name.
-- * 'actionParams' - Defines the type of action and the parameters for that action.
-- * 'roleARN' - The ARN of the IAM role that is used to apply the mitigation action.
mkUpdateMitigationAction ::
  -- | 'actionName'
  Lude.Text ->
  UpdateMitigationAction
mkUpdateMitigationAction pActionName_ =
  UpdateMitigationAction'
    { actionParams = Lude.Nothing,
      roleARN = Lude.Nothing,
      actionName = pActionName_
    }

-- | Defines the type of action and the parameters for that action.
--
-- /Note:/ Consider using 'actionParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umaActionParams :: Lens.Lens' UpdateMitigationAction (Lude.Maybe MitigationActionParams)
umaActionParams = Lens.lens (actionParams :: UpdateMitigationAction -> Lude.Maybe MitigationActionParams) (\s a -> s {actionParams = a} :: UpdateMitigationAction)
{-# DEPRECATED umaActionParams "Use generic-lens or generic-optics with 'actionParams' instead." #-}

-- | The ARN of the IAM role that is used to apply the mitigation action.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umaRoleARN :: Lens.Lens' UpdateMitigationAction (Lude.Maybe Lude.Text)
umaRoleARN = Lens.lens (roleARN :: UpdateMitigationAction -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: UpdateMitigationAction)
{-# DEPRECATED umaRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The friendly name for the mitigation action. You can't change the name by using @UpdateMitigationAction@ . Instead, you must delete and re-create the mitigation action with the new name.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umaActionName :: Lens.Lens' UpdateMitigationAction Lude.Text
umaActionName = Lens.lens (actionName :: UpdateMitigationAction -> Lude.Text) (\s a -> s {actionName = a} :: UpdateMitigationAction)
{-# DEPRECATED umaActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

instance Lude.AWSRequest UpdateMitigationAction where
  type Rs UpdateMitigationAction = UpdateMitigationActionResponse
  request = Req.patchJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateMitigationActionResponse'
            Lude.<$> (x Lude..?> "actionId")
            Lude.<*> (x Lude..?> "actionArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateMitigationAction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON UpdateMitigationAction where
  toJSON UpdateMitigationAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("actionParams" Lude..=) Lude.<$> actionParams,
            ("roleArn" Lude..=) Lude.<$> roleARN
          ]
      )

instance Lude.ToPath UpdateMitigationAction where
  toPath UpdateMitigationAction' {..} =
    Lude.mconcat
      ["/mitigationactions/actions/", Lude.toBS actionName]

instance Lude.ToQuery UpdateMitigationAction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateMitigationActionResponse' smart constructor.
data UpdateMitigationActionResponse = UpdateMitigationActionResponse'
  { actionId ::
      Lude.Maybe Lude.Text,
    actionARN ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMitigationActionResponse' with the minimum fields required to make a request.
--
-- * 'actionARN' - The ARN for the new mitigation action.
-- * 'actionId' - A unique identifier for the mitigation action.
-- * 'responseStatus' - The response status code.
mkUpdateMitigationActionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateMitigationActionResponse
mkUpdateMitigationActionResponse pResponseStatus_ =
  UpdateMitigationActionResponse'
    { actionId = Lude.Nothing,
      actionARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique identifier for the mitigation action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umarsActionId :: Lens.Lens' UpdateMitigationActionResponse (Lude.Maybe Lude.Text)
umarsActionId = Lens.lens (actionId :: UpdateMitigationActionResponse -> Lude.Maybe Lude.Text) (\s a -> s {actionId = a} :: UpdateMitigationActionResponse)
{-# DEPRECATED umarsActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | The ARN for the new mitigation action.
--
-- /Note:/ Consider using 'actionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umarsActionARN :: Lens.Lens' UpdateMitigationActionResponse (Lude.Maybe Lude.Text)
umarsActionARN = Lens.lens (actionARN :: UpdateMitigationActionResponse -> Lude.Maybe Lude.Text) (\s a -> s {actionARN = a} :: UpdateMitigationActionResponse)
{-# DEPRECATED umarsActionARN "Use generic-lens or generic-optics with 'actionARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umarsResponseStatus :: Lens.Lens' UpdateMitigationActionResponse Lude.Int
umarsResponseStatus = Lens.lens (responseStatus :: UpdateMitigationActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateMitigationActionResponse)
{-# DEPRECATED umarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
