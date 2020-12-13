{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    dmafrsLastModifiedDate,
    dmafrsActionParams,
    dmafrsActionId,
    dmafrsActionName,
    dmafrsCreationDate,
    dmafrsActionARN,
    dmafrsActionType,
    dmafrsRoleARN,
    dmafrsResponseStatus,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeMitigationAction' smart constructor.
newtype DescribeMitigationAction = DescribeMitigationAction'
  { -- | The friendly name that uniquely identifies the mitigation action.
    actionName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
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
  { -- | The date and time when the mitigation action was last changed.
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    -- | Parameters that control how the mitigation action is applied, specific to the type of mitigation action.
    actionParams :: Lude.Maybe MitigationActionParams,
    -- | A unique identifier for this action.
    actionId :: Lude.Maybe Lude.Text,
    -- | The friendly name that uniquely identifies the mitigation action.
    actionName :: Lude.Maybe Lude.Text,
    -- | The date and time when the mitigation action was added to your AWS account.
    creationDate :: Lude.Maybe Lude.Timestamp,
    -- | The ARN that identifies this migration action.
    actionARN :: Lude.Maybe Lude.Text,
    -- | The type of mitigation action.
    actionType :: Lude.Maybe MitigationActionType,
    -- | The ARN of the IAM role used to apply this action.
    roleARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeMitigationActionResponse' with the minimum fields required to make a request.
--
-- * 'lastModifiedDate' - The date and time when the mitigation action was last changed.
-- * 'actionParams' - Parameters that control how the mitigation action is applied, specific to the type of mitigation action.
-- * 'actionId' - A unique identifier for this action.
-- * 'actionName' - The friendly name that uniquely identifies the mitigation action.
-- * 'creationDate' - The date and time when the mitigation action was added to your AWS account.
-- * 'actionARN' - The ARN that identifies this migration action.
-- * 'actionType' - The type of mitigation action.
-- * 'roleARN' - The ARN of the IAM role used to apply this action.
-- * 'responseStatus' - The response status code.
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
dmafrsLastModifiedDate :: Lens.Lens' DescribeMitigationActionResponse (Lude.Maybe Lude.Timestamp)
dmafrsLastModifiedDate = Lens.lens (lastModifiedDate :: DescribeMitigationActionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: DescribeMitigationActionResponse)
{-# DEPRECATED dmafrsLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | Parameters that control how the mitigation action is applied, specific to the type of mitigation action.
--
-- /Note:/ Consider using 'actionParams' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmafrsActionParams :: Lens.Lens' DescribeMitigationActionResponse (Lude.Maybe MitigationActionParams)
dmafrsActionParams = Lens.lens (actionParams :: DescribeMitigationActionResponse -> Lude.Maybe MitigationActionParams) (\s a -> s {actionParams = a} :: DescribeMitigationActionResponse)
{-# DEPRECATED dmafrsActionParams "Use generic-lens or generic-optics with 'actionParams' instead." #-}

-- | A unique identifier for this action.
--
-- /Note:/ Consider using 'actionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmafrsActionId :: Lens.Lens' DescribeMitigationActionResponse (Lude.Maybe Lude.Text)
dmafrsActionId = Lens.lens (actionId :: DescribeMitigationActionResponse -> Lude.Maybe Lude.Text) (\s a -> s {actionId = a} :: DescribeMitigationActionResponse)
{-# DEPRECATED dmafrsActionId "Use generic-lens or generic-optics with 'actionId' instead." #-}

-- | The friendly name that uniquely identifies the mitigation action.
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmafrsActionName :: Lens.Lens' DescribeMitigationActionResponse (Lude.Maybe Lude.Text)
dmafrsActionName = Lens.lens (actionName :: DescribeMitigationActionResponse -> Lude.Maybe Lude.Text) (\s a -> s {actionName = a} :: DescribeMitigationActionResponse)
{-# DEPRECATED dmafrsActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | The date and time when the mitigation action was added to your AWS account.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmafrsCreationDate :: Lens.Lens' DescribeMitigationActionResponse (Lude.Maybe Lude.Timestamp)
dmafrsCreationDate = Lens.lens (creationDate :: DescribeMitigationActionResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: DescribeMitigationActionResponse)
{-# DEPRECATED dmafrsCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The ARN that identifies this migration action.
--
-- /Note:/ Consider using 'actionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmafrsActionARN :: Lens.Lens' DescribeMitigationActionResponse (Lude.Maybe Lude.Text)
dmafrsActionARN = Lens.lens (actionARN :: DescribeMitigationActionResponse -> Lude.Maybe Lude.Text) (\s a -> s {actionARN = a} :: DescribeMitigationActionResponse)
{-# DEPRECATED dmafrsActionARN "Use generic-lens or generic-optics with 'actionARN' instead." #-}

-- | The type of mitigation action.
--
-- /Note:/ Consider using 'actionType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmafrsActionType :: Lens.Lens' DescribeMitigationActionResponse (Lude.Maybe MitigationActionType)
dmafrsActionType = Lens.lens (actionType :: DescribeMitigationActionResponse -> Lude.Maybe MitigationActionType) (\s a -> s {actionType = a} :: DescribeMitigationActionResponse)
{-# DEPRECATED dmafrsActionType "Use generic-lens or generic-optics with 'actionType' instead." #-}

-- | The ARN of the IAM role used to apply this action.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmafrsRoleARN :: Lens.Lens' DescribeMitigationActionResponse (Lude.Maybe Lude.Text)
dmafrsRoleARN = Lens.lens (roleARN :: DescribeMitigationActionResponse -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DescribeMitigationActionResponse)
{-# DEPRECATED dmafrsRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmafrsResponseStatus :: Lens.Lens' DescribeMitigationActionResponse Lude.Int
dmafrsResponseStatus = Lens.lens (responseStatus :: DescribeMitigationActionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeMitigationActionResponse)
{-# DEPRECATED dmafrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
