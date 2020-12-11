{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.AssociateDHCPOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates a set of DHCP options (that you've previously created) with the specified VPC, or associates no DHCP options with the VPC.
--
-- After you associate the options with the VPC, any existing instances and all new instances that you launch in that VPC use the options. You don't need to restart or relaunch the instances. They automatically pick up the changes within a few hours, depending on how frequently the instance renews its DHCP lease. You can explicitly renew the lease using the operating system on the instance.
-- For more information, see <https://docs.aws.amazon.com/vpc/latest/userguide/VPC_DHCP_Options.html DHCP Options Sets> in the /Amazon Virtual Private Cloud User Guide/ .
module Network.AWS.EC2.AssociateDHCPOptions
  ( -- * Creating a request
    AssociateDHCPOptions (..),
    mkAssociateDHCPOptions,

    -- ** Request lenses
    adoDryRun,
    adoDHCPOptionsId,
    adoVPCId,

    -- * Destructuring the response
    AssociateDHCPOptionsResponse (..),
    mkAssociateDHCPOptionsResponse,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAssociateDHCPOptions' smart constructor.
data AssociateDHCPOptions = AssociateDHCPOptions'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    dhcpOptionsId :: Lude.Text,
    vpcId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateDHCPOptions' with the minimum fields required to make a request.
--
-- * 'dhcpOptionsId' - The ID of the DHCP options set, or @default@ to associate no DHCP options with the VPC.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'vpcId' - The ID of the VPC.
mkAssociateDHCPOptions ::
  -- | 'dhcpOptionsId'
  Lude.Text ->
  -- | 'vpcId'
  Lude.Text ->
  AssociateDHCPOptions
mkAssociateDHCPOptions pDHCPOptionsId_ pVPCId_ =
  AssociateDHCPOptions'
    { dryRun = Lude.Nothing,
      dhcpOptionsId = pDHCPOptionsId_,
      vpcId = pVPCId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adoDryRun :: Lens.Lens' AssociateDHCPOptions (Lude.Maybe Lude.Bool)
adoDryRun = Lens.lens (dryRun :: AssociateDHCPOptions -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: AssociateDHCPOptions)
{-# DEPRECATED adoDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the DHCP options set, or @default@ to associate no DHCP options with the VPC.
--
-- /Note:/ Consider using 'dhcpOptionsId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adoDHCPOptionsId :: Lens.Lens' AssociateDHCPOptions Lude.Text
adoDHCPOptionsId = Lens.lens (dhcpOptionsId :: AssociateDHCPOptions -> Lude.Text) (\s a -> s {dhcpOptionsId = a} :: AssociateDHCPOptions)
{-# DEPRECATED adoDHCPOptionsId "Use generic-lens or generic-optics with 'dhcpOptionsId' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
adoVPCId :: Lens.Lens' AssociateDHCPOptions Lude.Text
adoVPCId = Lens.lens (vpcId :: AssociateDHCPOptions -> Lude.Text) (\s a -> s {vpcId = a} :: AssociateDHCPOptions)
{-# DEPRECATED adoVPCId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Lude.AWSRequest AssociateDHCPOptions where
  type Rs AssociateDHCPOptions = AssociateDHCPOptionsResponse
  request = Req.postQuery ec2Service
  response = Res.receiveNull AssociateDHCPOptionsResponse'

instance Lude.ToHeaders AssociateDHCPOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AssociateDHCPOptions where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateDHCPOptions where
  toQuery AssociateDHCPOptions' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AssociateDhcpOptions" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "DhcpOptionsId" Lude.=: dhcpOptionsId,
        "VpcId" Lude.=: vpcId
      ]

-- | /See:/ 'mkAssociateDHCPOptionsResponse' smart constructor.
data AssociateDHCPOptionsResponse = AssociateDHCPOptionsResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateDHCPOptionsResponse' with the minimum fields required to make a request.
mkAssociateDHCPOptionsResponse ::
  AssociateDHCPOptionsResponse
mkAssociateDHCPOptionsResponse = AssociateDHCPOptionsResponse'
