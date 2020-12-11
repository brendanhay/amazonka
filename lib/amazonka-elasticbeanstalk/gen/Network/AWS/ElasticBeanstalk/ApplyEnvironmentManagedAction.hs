{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.ApplyEnvironmentManagedAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a scheduled managed action immediately. A managed action can be applied only if its status is @Scheduled@ . Get the status and action ID of a managed action with 'DescribeEnvironmentManagedActions' .
module Network.AWS.ElasticBeanstalk.ApplyEnvironmentManagedAction
  ( -- * Creating a request
    ApplyEnvironmentManagedAction (..),
    mkApplyEnvironmentManagedAction,

    -- ** Request lenses
    aemaEnvironmentName,
    aemaEnvironmentId,
    aemaActionId,

    -- * Destructuring the response
    ApplyEnvironmentManagedActionResponse (..),
    mkApplyEnvironmentManagedActionResponse,

    -- ** Response lenses
    aemarsStatus,
    aemarsActionId,
    aemarsActionDescription,
    aemarsActionType,
    aemarsResponseStatus,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to execute a scheduled managed action immediately.
--
-- /See:/ 'mkApplyEnvironmentManagedAction' smart constructor.
data ApplyEnvironmentManagedAction = ApplyEnvironmentManagedAction'
  { environmentName ::
      Lude.Maybe Lude.Text,
    environmentId ::
      Lude.Maybe Lude.Text,
    actionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ApplyEnvironmentManagedAction' with the minimum fields required to make a request.
--
-- * 'actionId' - The action ID of the scheduled managed action to execute.
-- * 'environmentId' - The environment ID of the target environment.
-- * 'environmentName' - The name of the target environment.
mkApplyEnvironmentManagedAction ::
  -- | 'actionId'
  Lude.Text ->
  ApplyEnvironmentManagedAction
mkApplyEnvironmentManagedAction pActionId_ =
  ApplyEnvironmentManagedAction'
    { environmentName = Lude.Nothing,
      environmentId = Lude.Nothing,
      actionId = pActionId_
    }

-- | The name of the target environment.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemaEnvironmentName :: Lens.Lens' ApplyEnvironmentManagedAction (Lude.Maybe Lude.Text)
aemaEnvironmentName = Lens.lens (environmentName :: ApplyEnvironmentManagedAction -> Lude.Maybe Lude.Text) (\s a -> s {environmentName = a} :: ApplyEnvironmentManagedAction)
{-# DEPRECATED aemaEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The environment ID of the target environment.
--
-- /Note:/ Consider using 'environmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemaEnvironmentId :: Lens.Lens' ApplyEnvironmentManagedAction (Lude.Maybe Lude.Text)
aemaEnvironmentId = Lens.lens (environmentId :: ApplyEnvironmentManagedAction -> Lude.Maybe Lude.Text) (\s a -> s {environmentId = a} :: ApplyEnvironmentManagedAction)
{-# DEPRECATED aemaEnvironmentId "Use generic-lens or generic-optics with 'environmentId' instead." #-}

-- | The action ID of the scheduled managed action to execute.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemaActionId :: Lens.Lens' ApplyEnvironmentManagedAction Lude.Text
aemaActionId = Lens.lens (actionId :: ApplyEnvironmentManagedAction -> Lude.Text) (\s a -> s {actionId = a} :: ApplyEnvironmentManagedAction)
{-# DEPRECATED aemaActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

instance Lude.AWSRequest ApplyEnvironmentManagedAction where
  type
    Rs ApplyEnvironmentManagedAction =
      ApplyEnvironmentManagedActionResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveXMLWrapper
      "ApplyEnvironmentManagedActionResult"
      ( \s h x ->
          ApplyEnvironmentManagedActionResponse'
            Lude.<$> (x Lude..@? "Status")
            Lude.<*> (x Lude..@? "ActionId")
            Lude.<*> (x Lude..@? "ActionDescription")
            Lude.<*> (x Lude..@? "ActionType")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ApplyEnvironmentManagedAction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ApplyEnvironmentManagedAction where
  toPath = Lude.const "/"

instance Lude.ToQuery ApplyEnvironmentManagedAction where
  toQuery ApplyEnvironmentManagedAction' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("ApplyEnvironmentManagedAction" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "EnvironmentName" Lude.=: environmentName,
        "EnvironmentId" Lude.=: environmentId,
        "ActionId" Lude.=: actionId
      ]

-- | The result message containing information about the managed action.
--
-- /See:/ 'mkApplyEnvironmentManagedActionResponse' smart constructor.
data ApplyEnvironmentManagedActionResponse = ApplyEnvironmentManagedActionResponse'
  { status ::
      Lude.Maybe
        Lude.Text,
    actionId ::
      Lude.Maybe
        Lude.Text,
    actionDescription ::
      Lude.Maybe
        Lude.Text,
    actionType ::
      Lude.Maybe
        ActionType,
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

-- | Creates a value of 'ApplyEnvironmentManagedActionResponse' with the minimum fields required to make a request.
--
-- * 'actionDescription' - A description of the managed action.
-- * 'actionId' - The action ID of the managed action.
-- * 'actionType' - The type of managed action.
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of the managed action.
mkApplyEnvironmentManagedActionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ApplyEnvironmentManagedActionResponse
mkApplyEnvironmentManagedActionResponse pResponseStatus_ =
  ApplyEnvironmentManagedActionResponse'
    { status = Lude.Nothing,
      actionId = Lude.Nothing,
      actionDescription = Lude.Nothing,
      actionType = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the managed action.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemarsStatus :: Lens.Lens' ApplyEnvironmentManagedActionResponse (Lude.Maybe Lude.Text)
aemarsStatus = Lens.lens (status :: ApplyEnvironmentManagedActionResponse -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ApplyEnvironmentManagedActionResponse)
{-# DEPRECATED aemarsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The action ID of the managed action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemarsActionId :: Lens.Lens' ApplyEnvironmentManagedActionResponse (Lude.Maybe Lude.Text)
aemarsActionId = Lens.lens (actionId :: ApplyEnvironmentManagedActionResponse -> Lude.Maybe Lude.Text) (\s a -> s {actionId = a} :: ApplyEnvironmentManagedActionResponse)
{-# DEPRECATED aemarsActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | A description of the managed action.
--
-- /Note:/ Consider using 'actionDescription' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemarsActionDescription :: Lens.Lens' ApplyEnvironmentManagedActionResponse (Lude.Maybe Lude.Text)
aemarsActionDescription = Lens.lens (actionDescription :: ApplyEnvironmentManagedActionResponse -> Lude.Maybe Lude.Text) (\s a -> s {actionDescription = a} :: ApplyEnvironmentManagedActionResponse)
{-# DEPRECATED aemarsActionDescription "Use generic-lens or generic-optics with 'actionDescription' instead." #-}

-- | The type of managed action.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemarsActionType :: Lens.Lens' ApplyEnvironmentManagedActionResponse (Lude.Maybe ActionType)
aemarsActionType = Lens.lens (actionType :: ApplyEnvironmentManagedActionResponse -> Lude.Maybe ActionType) (\s a -> s {actionType = a} :: ApplyEnvironmentManagedActionResponse)
{-# DEPRECATED aemarsActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aemarsResponseStatus :: Lens.Lens' ApplyEnvironmentManagedActionResponse Lude.Int
aemarsResponseStatus = Lens.lens (responseStatus :: ApplyEnvironmentManagedActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ApplyEnvironmentManagedActionResponse)
{-# DEPRECATED aemarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
