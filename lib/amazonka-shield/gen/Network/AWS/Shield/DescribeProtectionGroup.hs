{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DescribeProtectionGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the specification for the specified protection group.
module Network.AWS.Shield.DescribeProtectionGroup
  ( -- * Creating a request
    DescribeProtectionGroup (..),
    mkDescribeProtectionGroup,

    -- ** Request lenses
    dProtectionGroupId,

    -- * Destructuring the response
    DescribeProtectionGroupResponse (..),
    mkDescribeProtectionGroupResponse,

    -- ** Response lenses
    drsResponseStatus,
    drsProtectionGroup,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkDescribeProtectionGroup' smart constructor.
newtype DescribeProtectionGroup = DescribeProtectionGroup'
  { protectionGroupId ::
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

-- | Creates a value of 'DescribeProtectionGroup' with the minimum fields required to make a request.
--
-- * 'protectionGroupId' - The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
mkDescribeProtectionGroup ::
  -- | 'protectionGroupId'
  Lude.Text ->
  DescribeProtectionGroup
mkDescribeProtectionGroup pProtectionGroupId_ =
  DescribeProtectionGroup' {protectionGroupId = pProtectionGroupId_}

-- | The name of the protection group. You use this to identify the protection group in lists and to manage the protection group, for example to update, delete, or describe it.
--
-- /Note:/ Consider using 'protectionGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dProtectionGroupId :: Lens.Lens' DescribeProtectionGroup Lude.Text
dProtectionGroupId = Lens.lens (protectionGroupId :: DescribeProtectionGroup -> Lude.Text) (\s a -> s {protectionGroupId = a} :: DescribeProtectionGroup)
{-# DEPRECATED dProtectionGroupId "Use generic-lens or generic-optics with 'protectionGroupId' instead." #-}

instance Lude.AWSRequest DescribeProtectionGroup where
  type Rs DescribeProtectionGroup = DescribeProtectionGroupResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeProtectionGroupResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "ProtectionGroup")
      )

instance Lude.ToHeaders DescribeProtectionGroup where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.DescribeProtectionGroup" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeProtectionGroup where
  toJSON DescribeProtectionGroup' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ProtectionGroupId" Lude..= protectionGroupId)]
      )

instance Lude.ToPath DescribeProtectionGroup where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeProtectionGroup where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeProtectionGroupResponse' smart constructor.
data DescribeProtectionGroupResponse = DescribeProtectionGroupResponse'
  { responseStatus ::
      Lude.Int,
    protectionGroup ::
      ProtectionGroup
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeProtectionGroupResponse' with the minimum fields required to make a request.
--
-- * 'protectionGroup' - A grouping of protected resources that you and AWS Shield Advanced can monitor as a collective. This resource grouping improves the accuracy of detection and reduces false positives.
-- * 'responseStatus' - The response status code.
mkDescribeProtectionGroupResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'protectionGroup'
  ProtectionGroup ->
  DescribeProtectionGroupResponse
mkDescribeProtectionGroupResponse
  pResponseStatus_
  pProtectionGroup_ =
    DescribeProtectionGroupResponse'
      { responseStatus =
          pResponseStatus_,
        protectionGroup = pProtectionGroup_
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeProtectionGroupResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeProtectionGroupResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeProtectionGroupResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A grouping of protected resources that you and AWS Shield Advanced can monitor as a collective. This resource grouping improves the accuracy of detection and reduces false positives.
--
-- /Note:/ Consider using 'protectionGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsProtectionGroup :: Lens.Lens' DescribeProtectionGroupResponse ProtectionGroup
drsProtectionGroup = Lens.lens (protectionGroup :: DescribeProtectionGroupResponse -> ProtectionGroup) (\s a -> s {protectionGroup = a} :: DescribeProtectionGroupResponse)
{-# DEPRECATED drsProtectionGroup "Use generic-lens or generic-optics with 'protectionGroup' instead." #-}
