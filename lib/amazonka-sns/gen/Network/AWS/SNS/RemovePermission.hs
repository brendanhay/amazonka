{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.RemovePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a statement from a topic's access control policy.
module Network.AWS.SNS.RemovePermission
  ( -- * Creating a request
    RemovePermission (..),
    mkRemovePermission,

    -- ** Request lenses
    rpTopicARN,
    rpLabel,

    -- * Destructuring the response
    RemovePermissionResponse (..),
    mkRemovePermissionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SNS.Types

-- | Input for RemovePermission action.
--
-- /See:/ 'mkRemovePermission' smart constructor.
data RemovePermission = RemovePermission'
  { -- | The ARN of the topic whose access control policy you wish to modify.
    topicARN :: Lude.Text,
    -- | The unique label of the statement you want to remove.
    label :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemovePermission' with the minimum fields required to make a request.
--
-- * 'topicARN' - The ARN of the topic whose access control policy you wish to modify.
-- * 'label' - The unique label of the statement you want to remove.
mkRemovePermission ::
  -- | 'topicARN'
  Lude.Text ->
  -- | 'label'
  Lude.Text ->
  RemovePermission
mkRemovePermission pTopicARN_ pLabel_ =
  RemovePermission' {topicARN = pTopicARN_, label = pLabel_}

-- | The ARN of the topic whose access control policy you wish to modify.
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpTopicARN :: Lens.Lens' RemovePermission Lude.Text
rpTopicARN = Lens.lens (topicARN :: RemovePermission -> Lude.Text) (\s a -> s {topicARN = a} :: RemovePermission)
{-# DEPRECATED rpTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | The unique label of the statement you want to remove.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpLabel :: Lens.Lens' RemovePermission Lude.Text
rpLabel = Lens.lens (label :: RemovePermission -> Lude.Text) (\s a -> s {label = a} :: RemovePermission)
{-# DEPRECATED rpLabel "Use generic-lens or generic-optics with 'label' instead." #-}

instance Lude.AWSRequest RemovePermission where
  type Rs RemovePermission = RemovePermissionResponse
  request = Req.postQuery snsService
  response = Res.receiveNull RemovePermissionResponse'

instance Lude.ToHeaders RemovePermission where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RemovePermission where
  toPath = Lude.const "/"

instance Lude.ToQuery RemovePermission where
  toQuery RemovePermission' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RemovePermission" :: Lude.ByteString),
        "Version" Lude.=: ("2010-03-31" :: Lude.ByteString),
        "TopicArn" Lude.=: topicARN,
        "Label" Lude.=: label
      ]

-- | /See:/ 'mkRemovePermissionResponse' smart constructor.
data RemovePermissionResponse = RemovePermissionResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemovePermissionResponse' with the minimum fields required to make a request.
mkRemovePermissionResponse ::
  RemovePermissionResponse
mkRemovePermissionResponse = RemovePermissionResponse'
