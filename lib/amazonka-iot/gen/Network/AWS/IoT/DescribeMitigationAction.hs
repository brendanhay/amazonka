{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.DescribeMitigationAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a mitigation action.
module Network.AWS.IoT.DescribeMitigationAction
  ( -- * Creating a request
    DescribeMitigationAction (..),
    mkDescribeMitigationAction,

    -- ** Request lenses
    dActionName,

    -- * Destructuring the response
    DescribeMitigationActionResponse (..),
    mkDescribeMitigationActionResponse,

    -- ** Response lenses
    desrsLastModifiedDate,
    desrsActionParams,
    desrsActionId,
    desrsActionName,
    desrsCreationDate,
    desrsActionARN,
    desrsActionType,
    desrsRoleARN,
    desrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeMitigationAction' smart constructor.
newtype DescribeMitigationAction = DescribeMitigationAction'
  { actionName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMitigationAction' with the minimum fields required to make a request.
--
-- * 'actionName' - The friendly name that uniquely identifies the mitigation action.
mkDescribeMitigationAction ::
  -- | 'actionName'
  Lude.Text ->
  DescribeMitigationAction
mkDescribeMitigationAction pActionName_ =
  DescribeMitigationAction' {actionName = pActionName_}

-- | The friendly name that uniquely identifies the mitigation action.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dActionName :: Lens.Lens' DescribeMitigationAction Lude.Text
dActionName = Lens.lens (actionName :: DescribeMitigationAction -> Lude.Text) (\s a -> s {actionName = a} :: DescribeMitigationAction)
{-# DEPRECATED dActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

instance Lude.AWSRequest DescribeMitigationAction where
  type Rs DescribeMitigationAction = DescribeMitigationActionResponse
  request = Req.get ioTService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeMitigationActionResponse'
            Lude.<$> (x Lude..?> "lastModifiedDate")
            Lude.<*> (x Lude..?> "actionParams")
            Lude.<*> (x Lude..?> "actionId")
            Lude.<*> (x Lude..?> "actionName")
            Lude.<*> (x Lude..?> "creationDate")
            Lude.<*> (x Lude..?> "actionArn")
            Lude.<*> (x Lude..?> "actionType")
            Lude.<*> (x Lude..?> "roleArn")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeMitigationAction where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DescribeMitigationAction where
  toPath DescribeMitigationAction' {..} =
    Lude.mconcat
      ["/mitigationactions/actions/", Lude.toBS actionName]

instance Lude.ToQuery DescribeMitigationAction where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeMitigationActionResponse' smart constructor.
data DescribeMitigationActionResponse = DescribeMitigationActionResponse'
  { lastModifiedDate ::
      Lude.Maybe Lude.Timestamp,
    actionParams ::
      Lude.Maybe
        MitigationActionParams,
    actionId ::
      Lude.Maybe Lude.Text,
    actionName ::
      Lude.Maybe Lude.Text,
    creationDate ::
      Lude.Maybe Lude.Timestamp,
    actionARN ::
      Lude.Maybe Lude.Text,
    actionType ::
      Lude.Maybe
        MitigationActionType,
    roleARN ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeMitigationActionResponse' with the minimum fields required to make a request.
--
-- * 'actionARN' - The ARN that identifies this migration action.
-- * 'actionId' - A unique identifier for this action.
-- * 'actionName' - The friendly name that uniquely identifies the mitigation action.
-- * 'actionParams' - Parameters that control how the mitigation action is applied, specific to the type of mitigation action.
-- * 'actionType' - The type of mitigation action.
-- * 'creationDate' - The date and time when the mitigation action was added to your AWS account.
-- * 'lastModifiedDate' - The date and time when the mitigation action was last changed.
-- * 'responseStatus' - The response status code.
-- * 'roleARN' - The ARN of the IAM role used to apply this action.
mkDescribeMitigationActionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeMitigationActionResponse
mkDescribeMitigationActionResponse pResponseStatus_ =
  DescribeMitigationActionResponse'
    { lastModifiedDate =
        Lude.Nothing,
      actionParams = Lude.Nothing,
      actionId = Lude.Nothing,
      actionName = Lude.Nothing,
      creationDate = Lude.Nothing,
      actionARN = Lude.Nothing,
      actionType = Lude.Nothing,
      roleARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The date and time when the mitigation action was last changed.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsLastModifiedDate :: Lens.Lens' DescribeMitigationActionResponse (Lude.Maybe Lude.Timestamp)
desrsLastModifiedDate = Lens.lens (lastModifiedDate :: DescribeMitigationActionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: DescribeMitigationActionResponse)
{-# DEPRECATED desrsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Parameters that control how the mitigation action is applied, specific to the type of mitigation action.
--
-- /Note:/ Consider using 'actionParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsActionParams :: Lens.Lens' DescribeMitigationActionResponse (Lude.Maybe MitigationActionParams)
desrsActionParams = Lens.lens (actionParams :: DescribeMitigationActionResponse -> Lude.Maybe MitigationActionParams) (\s a -> s {actionParams = a} :: DescribeMitigationActionResponse)
{-# DEPRECATED desrsActionParams "Use generic-lens or generic-optics with 'actionParams' instead." #-}

-- | A unique identifier for this action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsActionId :: Lens.Lens' DescribeMitigationActionResponse (Lude.Maybe Lude.Text)
desrsActionId = Lens.lens (actionId :: DescribeMitigationActionResponse -> Lude.Maybe Lude.Text) (\s a -> s {actionId = a} :: DescribeMitigationActionResponse)
{-# DEPRECATED desrsActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | The friendly name that uniquely identifies the mitigation action.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsActionName :: Lens.Lens' DescribeMitigationActionResponse (Lude.Maybe Lude.Text)
desrsActionName = Lens.lens (actionName :: DescribeMitigationActionResponse -> Lude.Maybe Lude.Text) (\s a -> s {actionName = a} :: DescribeMitigationActionResponse)
{-# DEPRECATED desrsActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | The date and time when the mitigation action was added to your AWS account.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsCreationDate :: Lens.Lens' DescribeMitigationActionResponse (Lude.Maybe Lude.Timestamp)
desrsCreationDate = Lens.lens (creationDate :: DescribeMitigationActionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: DescribeMitigationActionResponse)
{-# DEPRECATED desrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The ARN that identifies this migration action.
--
-- /Note:/ Consider using 'actionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsActionARN :: Lens.Lens' DescribeMitigationActionResponse (Lude.Maybe Lude.Text)
desrsActionARN = Lens.lens (actionARN :: DescribeMitigationActionResponse -> Lude.Maybe Lude.Text) (\s a -> s {actionARN = a} :: DescribeMitigationActionResponse)
{-# DEPRECATED desrsActionARN "Use generic-lens or generic-optics with 'actionARN' instead." #-}

-- | The type of mitigation action.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsActionType :: Lens.Lens' DescribeMitigationActionResponse (Lude.Maybe MitigationActionType)
desrsActionType = Lens.lens (actionType :: DescribeMitigationActionResponse -> Lude.Maybe MitigationActionType) (\s a -> s {actionType = a} :: DescribeMitigationActionResponse)
{-# DEPRECATED desrsActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

-- | The ARN of the IAM role used to apply this action.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsRoleARN :: Lens.Lens' DescribeMitigationActionResponse (Lude.Maybe Lude.Text)
desrsRoleARN = Lens.lens (roleARN :: DescribeMitigationActionResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DescribeMitigationActionResponse)
{-# DEPRECATED desrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desrsResponseStatus :: Lens.Lens' DescribeMitigationActionResponse Lude.Int
desrsResponseStatus = Lens.lens (responseStatus :: DescribeMitigationActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMitigationActionResponse)
{-# DEPRECATED desrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
