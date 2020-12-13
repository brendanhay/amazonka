{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.AddPermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a statement to a topic's access control policy, granting access for the specified AWS accounts to the specified actions.
module Network.AWS.SNS.AddPermission
  ( -- * Creating a request
    AddPermission (..),
    mkAddPermission,

    -- ** Request lenses
    apAWSAccountId,
    apActionName,
    apTopicARN,
    apLabel,

    -- * Destructuring the response
    AddPermissionResponse (..),
    mkAddPermissionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | /See:/ 'mkAddPermission' smart constructor.
data AddPermission = AddPermission'
  { -- | The AWS account IDs of the users (principals) who will be given access to the specified actions. The users must have AWS accounts, but do not need to be signed up for this service.
    awsAccountId :: [Lude.Text],
    -- | The action you want to allow for the specified principal(s).
    --
    -- Valid values: Any Amazon SNS action name, for example @Publish@ .
    actionName :: [Lude.Text],
    -- | The ARN of the topic whose access control policy you wish to modify.
    topicARN :: Lude.Text,
    -- | A unique identifier for the new policy statement.
    label :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddPermission' with the minimum fields required to make a request.
--
-- * 'awsAccountId' - The AWS account IDs of the users (principals) who will be given access to the specified actions. The users must have AWS accounts, but do not need to be signed up for this service.
-- * 'actionName' - The action you want to allow for the specified principal(s).
--
-- Valid values: Any Amazon SNS action name, for example @Publish@ .
-- * 'topicARN' - The ARN of the topic whose access control policy you wish to modify.
-- * 'label' - A unique identifier for the new policy statement.
mkAddPermission ::
  -- | 'topicARN'
  Lude.Text ->
  -- | 'label'
  Lude.Text ->
  AddPermission
mkAddPermission pTopicARN_ pLabel_ =
  AddPermission'
    { awsAccountId = Lude.mempty,
      actionName = Lude.mempty,
      topicARN = pTopicARN_,
      label = pLabel_
    }

-- | The AWS account IDs of the users (principals) who will be given access to the specified actions. The users must have AWS accounts, but do not need to be signed up for this service.
--
-- /Note:/ Consider using 'awsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apAWSAccountId :: Lens.Lens' AddPermission [Lude.Text]
apAWSAccountId = Lens.lens (awsAccountId :: AddPermission -> [Lude.Text]) (\s a -> s {awsAccountId = a} :: AddPermission)
{-# DEPRECATED apAWSAccountId "Use generic-lens or generic-optics with 'awsAccountId' instead." #-}

-- | The action you want to allow for the specified principal(s).
--
-- Valid values: Any Amazon SNS action name, for example @Publish@ .
--
-- /Note:/ Consider using 'actionName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apActionName :: Lens.Lens' AddPermission [Lude.Text]
apActionName = Lens.lens (actionName :: AddPermission -> [Lude.Text]) (\s a -> s {actionName = a} :: AddPermission)
{-# DEPRECATED apActionName "Use generic-lens or generic-optics with 'actionName' instead." #-}

-- | The ARN of the topic whose access control policy you wish to modify.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apTopicARN :: Lens.Lens' AddPermission Lude.Text
apTopicARN = Lens.lens (topicARN :: AddPermission -> Lude.Text) (\s a -> s {topicARN = a} :: AddPermission)
{-# DEPRECATED apTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | A unique identifier for the new policy statement.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
apLabel :: Lens.Lens' AddPermission Lude.Text
apLabel = Lens.lens (label :: AddPermission -> Lude.Text) (\s a -> s {label = a} :: AddPermission)
{-# DEPRECATED apLabel "Use generic-lens or generic-optics with 'label' instead." #-}

instance Lude.AWSRequest AddPermission where
  type Rs AddPermission = AddPermissionResponse
  request = Req.postQuery snsService
  response = Res.receiveNull AddPermissionResponse'

instance Lude.ToHeaders AddPermission where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AddPermission where
  toPath = Lude.const "/"

instance Lude.ToQuery AddPermission where
  toQuery AddPermission' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AddPermission" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "AWSAccountId" Lude.=: Lude.toQueryList "member" awsAccountId,
        "ActionName" Lude.=: Lude.toQueryList "member" actionName,
        "TopicArn" Lude.=: topicARN,
        "Label" Lude.=: label
      ]

-- | /See:/ 'mkAddPermissionResponse' smart constructor.
data AddPermissionResponse = AddPermissionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddPermissionResponse' with the minimum fields required to make a request.
mkAddPermissionResponse ::
  AddPermissionResponse
mkAddPermissionResponse = AddPermissionResponse'
