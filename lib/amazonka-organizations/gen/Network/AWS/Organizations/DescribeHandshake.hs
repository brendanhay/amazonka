{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Organizations.DescribeHandshake
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a previously requested handshake. The handshake ID comes from the response to the original 'InviteAccountToOrganization' operation that generated the handshake.
--
-- You can access handshakes that are @ACCEPTED@ , @DECLINED@ , or @CANCELED@ for only 30 days after they change to that state. They're then deleted and no longer accessible.
-- This operation can be called from any account in the organization.
module Network.AWS.Organizations.DescribeHandshake
  ( -- * Creating a request
    DescribeHandshake (..),
    mkDescribeHandshake,

    -- ** Request lenses
    dHandshakeId,

    -- * Destructuring the response
    DescribeHandshakeResponse (..),
    mkDescribeHandshakeResponse,

    -- ** Response lenses
    dhrsHandshake,
    dhrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Organizations.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeHandshake' smart constructor.
newtype DescribeHandshake = DescribeHandshake'
  { -- | The unique identifier (ID) of the handshake that you want information about. You can get the ID from the original call to 'InviteAccountToOrganization' , or from a call to 'ListHandshakesForAccount' or 'ListHandshakesForOrganization' .
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
    handshakeId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHandshake' with the minimum fields required to make a request.
--
-- * 'handshakeId' - The unique identifier (ID) of the handshake that you want information about. You can get the ID from the original call to 'InviteAccountToOrganization' , or from a call to 'ListHandshakesForAccount' or 'ListHandshakesForOrganization' .
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
mkDescribeHandshake ::
  -- | 'handshakeId'
  Lude.Text ->
  DescribeHandshake
mkDescribeHandshake pHandshakeId_ =
  DescribeHandshake' {handshakeId = pHandshakeId_}

-- | The unique identifier (ID) of the handshake that you want information about. You can get the ID from the original call to 'InviteAccountToOrganization' , or from a call to 'ListHandshakesForAccount' or 'ListHandshakesForOrganization' .
--
-- The <http://wikipedia.org/wiki/regex regex pattern> for handshake ID string requires "h-" followed by from 8 to 32 lowercase letters or digits.
--
-- /Note:/ Consider using 'handshakeId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHandshakeId :: Lens.Lens' DescribeHandshake Lude.Text
dHandshakeId = Lens.lens (handshakeId :: DescribeHandshake -> Lude.Text) (\s a -> s {handshakeId = a} :: DescribeHandshake)
{-# DEPRECATED dHandshakeId "Use generic-lens or generic-optics with 'handshakeId' instead." #-}

instance Lude.AWSRequest DescribeHandshake where
  type Rs DescribeHandshake = DescribeHandshakeResponse
  request = Req.postJSON organizationsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeHandshakeResponse'
            Lude.<$> (x Lude..?> "Handshake") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeHandshake where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSOrganizationsV20161128.DescribeHandshake" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeHandshake where
  toJSON DescribeHandshake' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("HandshakeId" Lude..= handshakeId)])

instance Lude.ToPath DescribeHandshake where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeHandshake where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeHandshakeResponse' smart constructor.
data DescribeHandshakeResponse = DescribeHandshakeResponse'
  { -- | A structure that contains information about the specified handshake.
    handshake :: Lude.Maybe Handshake,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeHandshakeResponse' with the minimum fields required to make a request.
--
-- * 'handshake' - A structure that contains information about the specified handshake.
-- * 'responseStatus' - The response status code.
mkDescribeHandshakeResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeHandshakeResponse
mkDescribeHandshakeResponse pResponseStatus_ =
  DescribeHandshakeResponse'
    { handshake = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A structure that contains information about the specified handshake.
--
-- /Note:/ Consider using 'handshake' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrsHandshake :: Lens.Lens' DescribeHandshakeResponse (Lude.Maybe Handshake)
dhrsHandshake = Lens.lens (handshake :: DescribeHandshakeResponse -> Lude.Maybe Handshake) (\s a -> s {handshake = a} :: DescribeHandshakeResponse)
{-# DEPRECATED dhrsHandshake "Use generic-lens or generic-optics with 'handshake' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dhrsResponseStatus :: Lens.Lens' DescribeHandshakeResponse Lude.Int
dhrsResponseStatus = Lens.lens (responseStatus :: DescribeHandshakeResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeHandshakeResponse)
{-# DEPRECATED dhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
