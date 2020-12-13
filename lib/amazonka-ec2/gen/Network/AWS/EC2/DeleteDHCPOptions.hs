{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteDHCPOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified set of DHCP options. You must disassociate the set of DHCP options before you can delete it. You can disassociate the set of DHCP options by associating either a new set of options or the default set of options with the VPC.
module Network.AWS.EC2.DeleteDHCPOptions
  ( -- * Creating a request
    DeleteDHCPOptions (..),
    mkDeleteDHCPOptions,

    -- ** Request lenses
    ddoDHCPOptionsId,
    ddoDryRun,

    -- * Destructuring the response
    DeleteDHCPOptionsResponse (..),
    mkDeleteDHCPOptionsResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDHCPOptions' smart constructor.
data DeleteDHCPOptions = DeleteDHCPOptions'
  { -- | The ID of the DHCP options set.
    dhcpOptionsId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDHCPOptions' with the minimum fields required to make a request.
--
-- * 'dhcpOptionsId' - The ID of the DHCP options set.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeleteDHCPOptions ::
  -- | 'dhcpOptionsId'
  Lude.Text ->
  DeleteDHCPOptions
mkDeleteDHCPOptions pDHCPOptionsId_ =
  DeleteDHCPOptions'
    { dhcpOptionsId = pDHCPOptionsId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the DHCP options set.
--
-- /Note:/ Consider using 'dhcpOptionsId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddoDHCPOptionsId :: Lens.Lens' DeleteDHCPOptions Lude.Text
ddoDHCPOptionsId = Lens.lens (dhcpOptionsId :: DeleteDHCPOptions -> Lude.Text) (\s a -> s {dhcpOptionsId = a} :: DeleteDHCPOptions)
{-# DEPRECATED ddoDHCPOptionsId "Use generic-lens or generic-optics with 'dhcpOptionsId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddoDryRun :: Lens.Lens' DeleteDHCPOptions (Lude.Maybe Lude.Bool)
ddoDryRun = Lens.lens (dryRun :: DeleteDHCPOptions -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteDHCPOptions)
{-# DEPRECATED ddoDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DeleteDHCPOptions where
  type Rs DeleteDHCPOptions = DeleteDHCPOptionsResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull DeleteDHCPOptionsResponse'

instance Lude.ToHeaders DeleteDHCPOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDHCPOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDHCPOptions where
  toQuery DeleteDHCPOptions' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteDhcpOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DhcpOptionsId" Lude.=: dhcpOptionsId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDeleteDHCPOptionsResponse' smart constructor.
data DeleteDHCPOptionsResponse = DeleteDHCPOptionsResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDHCPOptionsResponse' with the minimum fields required to make a request.
mkDeleteDHCPOptionsResponse ::
  DeleteDHCPOptionsResponse
mkDeleteDHCPOptionsResponse = DeleteDHCPOptionsResponse'
