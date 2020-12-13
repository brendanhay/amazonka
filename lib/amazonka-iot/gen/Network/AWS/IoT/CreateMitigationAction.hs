{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.CreateMitigationAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Defines an action that can be applied to audit findings by using StartAuditMitigationActionsTask. Only certain types of mitigation actions can be applied to specific check names. For more information, see <https://docs.aws.amazon.com/iot/latest/developerguide/device-defender-mitigation-actions.html Mitigation actions> . Each mitigation action can apply only one type of change.
module Network.AWS.IoT.CreateMitigationAction
  ( -- * Creating a request
    CreateMitigationAction (..),
    mkCreateMitigationAction,

    -- ** Request lenses
    cActionParams,
    cActionName,
    cTags,
    cRoleARN,

    -- * Destructuring the response
    CreateMitigationActionResponse (..),
    mkCreateMitigationActionResponse,

    -- ** Response lenses
    cmarsActionId,
    cmarsActionARN,
    cmarsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateMitigationAction' smart constructor.
data CreateMitigationAction = CreateMitigationAction'
  { -- | Defines the type of action and the parameters for that action.
    actionParams :: MitigationActionParams,
    -- | A friendly name for the action. Choose a friendly name that accurately describes the action (for example, @EnableLoggingAction@ ).
    actionName :: Lude.Text,
    -- | Metadata that can be used to manage the mitigation action.
    tags :: Lude.Maybe [Tag],
    -- | The ARN of the IAM role that is used to apply the mitigation action.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMitigationAction' with the minimum fields required to make a request.
--
-- * 'actionParams' - Defines the type of action and the parameters for that action.
-- * 'actionName' - A friendly name for the action. Choose a friendly name that accurately describes the action (for example, @EnableLoggingAction@ ).
-- * 'tags' - Metadata that can be used to manage the mitigation action.
-- * 'roleARN' - The ARN of the IAM role that is used to apply the mitigation action.
mkCreateMitigationAction ::
  -- | 'actionParams'
  MitigationActionParams ->
  -- | 'actionName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  CreateMitigationAction
mkCreateMitigationAction pActionParams_ pActionName_ pRoleARN_ =
  CreateMitigationAction'
    { actionParams = pActionParams_,
      actionName = pActionName_,
      tags = Lude.Nothing,
      roleARN = pRoleARN_
    }

-- | Defines the type of action and the parameters for that action.
--
-- /Note:/ Consider using 'actionParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cActionParams :: Lens.Lens' CreateMitigationAction MitigationActionParams
cActionParams = Lens.lens (actionParams :: CreateMitigationAction -> MitigationActionParams) (\s a -> s {actionParams = a} :: CreateMitigationAction)
{-# DEPRECATED cActionParams "Use generic-lens or generic-optics with 'actionParams' instead." #-}

-- | A friendly name for the action. Choose a friendly name that accurately describes the action (for example, @EnableLoggingAction@ ).
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cActionName :: Lens.Lens' CreateMitigationAction Lude.Text
cActionName = Lens.lens (actionName :: CreateMitigationAction -> Lude.Text) (\s a -> s {actionName = a} :: CreateMitigationAction)
{-# DEPRECATED cActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | Metadata that can be used to manage the mitigation action.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTags :: Lens.Lens' CreateMitigationAction (Lude.Maybe [Tag])
cTags = Lens.lens (tags :: CreateMitigationAction -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateMitigationAction)
{-# DEPRECATED cTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The ARN of the IAM role that is used to apply the mitigation action.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRoleARN :: Lens.Lens' CreateMitigationAction Lude.Text
cRoleARN = Lens.lens (roleARN :: CreateMitigationAction -> Lude.Text) (\s a -> s {roleARN = a} :: CreateMitigationAction)
{-# DEPRECATED cRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest CreateMitigationAction where
  type Rs CreateMitigationAction = CreateMitigationActionResponse
  request = Req.postJSON ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateMitigationActionResponse'
            Lude.<$> (x Lude..?> "actionId")
            Lude.<*> (x Lude..?> "actionArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateMitigationAction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON CreateMitigationAction where
  toJSON CreateMitigationAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("actionParams" Lude..= actionParams),
            ("tags" Lude..=) Lude.<$> tags,
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )

instance Lude.ToPath CreateMitigationAction where
  toPath CreateMitigationAction' {..} =
    Lude.mconcat
      ["/mitigationactions/actions/", Lude.toBS actionName]

instance Lude.ToQuery CreateMitigationAction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateMitigationActionResponse' smart constructor.
data CreateMitigationActionResponse = CreateMitigationActionResponse'
  { -- | A unique identifier for the new mitigation action.
    actionId :: Lude.Maybe Lude.Text,
    -- | The ARN for the new mitigation action.
    actionARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateMitigationActionResponse' with the minimum fields required to make a request.
--
-- * 'actionId' - A unique identifier for the new mitigation action.
-- * 'actionARN' - The ARN for the new mitigation action.
-- * 'responseStatus' - The response status code.
mkCreateMitigationActionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateMitigationActionResponse
mkCreateMitigationActionResponse pResponseStatus_ =
  CreateMitigationActionResponse'
    { actionId = Lude.Nothing,
      actionARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A unique identifier for the new mitigation action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmarsActionId :: Lens.Lens' CreateMitigationActionResponse (Lude.Maybe Lude.Text)
cmarsActionId = Lens.lens (actionId :: CreateMitigationActionResponse -> Lude.Maybe Lude.Text) (\s a -> s {actionId = a} :: CreateMitigationActionResponse)
{-# DEPRECATED cmarsActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | The ARN for the new mitigation action.
--
-- /Note:/ Consider using 'actionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmarsActionARN :: Lens.Lens' CreateMitigationActionResponse (Lude.Maybe Lude.Text)
cmarsActionARN = Lens.lens (actionARN :: CreateMitigationActionResponse -> Lude.Maybe Lude.Text) (\s a -> s {actionARN = a} :: CreateMitigationActionResponse)
{-# DEPRECATED cmarsActionARN "Use generic-lens or generic-optics with 'actionARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cmarsResponseStatus :: Lens.Lens' CreateMitigationActionResponse Lude.Int
cmarsResponseStatus = Lens.lens (responseStatus :: CreateMitigationActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateMitigationActionResponse)
{-# DEPRECATED cmarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
