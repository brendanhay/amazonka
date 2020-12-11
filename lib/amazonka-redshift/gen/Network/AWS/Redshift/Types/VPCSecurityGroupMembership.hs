-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.VPCSecurityGroupMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.VPCSecurityGroupMembership
  ( VPCSecurityGroupMembership (..),

    -- * Smart constructor
    mkVPCSecurityGroupMembership,

    -- * Lenses
    vsgmStatus,
    vsgmVPCSecurityGroupId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes the members of a VPC security group.
--
-- /See:/ 'mkVPCSecurityGroupMembership' smart constructor.
data VPCSecurityGroupMembership = VPCSecurityGroupMembership'
  { status ::
      Lude.Maybe Lude.Text,
    vpcSecurityGroupId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCSecurityGroupMembership' with the minimum fields required to make a request.
--
-- * 'status' - The status of the VPC security group.
-- * 'vpcSecurityGroupId' - The identifier of the VPC security group.
mkVPCSecurityGroupMembership ::
  VPCSecurityGroupMembership
mkVPCSecurityGroupMembership =
  VPCSecurityGroupMembership'
    { status = Lude.Nothing,
      vpcSecurityGroupId = Lude.Nothing
    }

-- | The status of the VPC security group.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsgmStatus :: Lens.Lens' VPCSecurityGroupMembership (Lude.Maybe Lude.Text)
vsgmStatus = Lens.lens (status :: VPCSecurityGroupMembership -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: VPCSecurityGroupMembership)
{-# DEPRECATED vsgmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The identifier of the VPC security group.
--
-- /Note:/ Consider using 'vpcSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsgmVPCSecurityGroupId :: Lens.Lens' VPCSecurityGroupMembership (Lude.Maybe Lude.Text)
vsgmVPCSecurityGroupId = Lens.lens (vpcSecurityGroupId :: VPCSecurityGroupMembership -> Lude.Maybe Lude.Text) (\s a -> s {vpcSecurityGroupId = a} :: VPCSecurityGroupMembership)
{-# DEPRECATED vsgmVPCSecurityGroupId "Use generic-lens or generic-optics with 'vpcSecurityGroupId' instead." #-}

instance Lude.FromXML VPCSecurityGroupMembership where
  parseXML x =
    VPCSecurityGroupMembership'
      Lude.<$> (x Lude..@? "Status") Lude.<*> (x Lude..@? "VpcSecurityGroupId")
