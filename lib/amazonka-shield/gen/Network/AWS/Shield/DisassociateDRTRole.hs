{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DisassociateDRTRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the DDoS Response Team's (DRT) access to your AWS account.
--
-- To make a @DisassociateDRTRole@ request, you must be subscribed to the <https://aws.amazon.com/premiumsupport/business-support/ Business Support plan> or the <https://aws.amazon.com/premiumsupport/enterprise-support/ Enterprise Support plan> . However, if you are not subscribed to one of these support plans, but had been previously and had granted the DRT access to your account, you can submit a @DisassociateDRTRole@ request to remove this access.
module Network.AWS.Shield.DisassociateDRTRole
  ( -- * Creating a request
    DisassociateDRTRole (..),
    mkDisassociateDRTRole,

    -- * Destructuring the response
    DisassociateDRTRoleResponse (..),
    mkDisassociateDRTRoleResponse,

    -- ** Response lenses
    ddrtrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkDisassociateDRTRole' smart constructor.
data DisassociateDRTRole = DisassociateDRTRole'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateDRTRole' with the minimum fields required to make a request.
mkDisassociateDRTRole ::
  DisassociateDRTRole
mkDisassociateDRTRole = DisassociateDRTRole'

instance Lude.AWSRequest DisassociateDRTRole where
  type Rs DisassociateDRTRole = DisassociateDRTRoleResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisassociateDRTRoleResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisassociateDRTRole where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSShield_20160616.DisassociateDRTRole" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisassociateDRTRole where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DisassociateDRTRole where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateDRTRole where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisassociateDRTRoleResponse' smart constructor.
newtype DisassociateDRTRoleResponse = DisassociateDRTRoleResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateDRTRoleResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisassociateDRTRoleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisassociateDRTRoleResponse
mkDisassociateDRTRoleResponse pResponseStatus_ =
  DisassociateDRTRoleResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrtrrsResponseStatus :: Lens.Lens' DisassociateDRTRoleResponse Lude.Int
ddrtrrsResponseStatus = Lens.lens (responseStatus :: DisassociateDRTRoleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisassociateDRTRoleResponse)
{-# DEPRECATED ddrtrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
