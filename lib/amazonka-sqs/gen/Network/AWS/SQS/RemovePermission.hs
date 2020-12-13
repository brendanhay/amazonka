{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.RemovePermission
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Revokes any permissions in the queue policy that matches the specified @Label@ parameter.
module Network.AWS.SQS.RemovePermission
  ( -- * Creating a request
    RemovePermission (..),
    mkRemovePermission,

    -- ** Request lenses
    rpQueueURL,
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
import Network.AWS.SQS.Types

-- |
--
-- /See:/ 'mkRemovePermission' smart constructor.
data RemovePermission = RemovePermission'
  { -- | The URL of the Amazon SQS queue from which permissions are removed.
    --
    -- Queue URLs and names are case-sensitive.
    queueURL :: Lude.Text,
    -- | The identification of the permission to remove. This is the label added using the @'AddPermission' @ action.
    label :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemovePermission' with the minimum fields required to make a request.
--
-- * 'queueURL' - The URL of the Amazon SQS queue from which permissions are removed.
--
-- Queue URLs and names are case-sensitive.
-- * 'label' - The identification of the permission to remove. This is the label added using the @'AddPermission' @ action.
mkRemovePermission ::
  -- | 'queueURL'
  Lude.Text ->
  -- | 'label'
  Lude.Text ->
  RemovePermission
mkRemovePermission pQueueURL_ pLabel_ =
  RemovePermission' {queueURL = pQueueURL_, label = pLabel_}

-- | The URL of the Amazon SQS queue from which permissions are removed.
--
-- Queue URLs and names are case-sensitive.
--
-- /Note:/ Consider using 'queueURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpQueueURL :: Lens.Lens' RemovePermission Lude.Text
rpQueueURL = Lens.lens (queueURL :: RemovePermission -> Lude.Text) (\s a -> s {queueURL = a} :: RemovePermission)
{-# DEPRECATED rpQueueURL "Use generic-lens or generic-optics with 'queueURL' instead." #-}

-- | The identification of the permission to remove. This is the label added using the @'AddPermission' @ action.
--
-- /Note:/ Consider using 'label' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rpLabel :: Lens.Lens' RemovePermission Lude.Text
rpLabel = Lens.lens (label :: RemovePermission -> Lude.Text) (\s a -> s {label = a} :: RemovePermission)
{-# DEPRECATED rpLabel "Use generic-lens or generic-optics with 'label' instead." #-}

instance Lude.AWSRequest RemovePermission where
  type Rs RemovePermission = RemovePermissionResponse
  request = Req.postQuery sqsService
  response = Res.receiveNull RemovePermissionResponse'

instance Lude.ToHeaders RemovePermission where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RemovePermission where
  toPath = Lude.const "/"

instance Lude.ToQuery RemovePermission where
  toQuery RemovePermission' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RemovePermission" :: Lude.ByteString),
        "Version" Lude.=: ("2012-11-05" :: Lude.ByteString),
        "QueueUrl" Lude.=: queueURL,
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
