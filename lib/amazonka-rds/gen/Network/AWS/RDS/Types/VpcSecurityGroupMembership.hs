{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.VpcSecurityGroupMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.RDS.Types.VpcSecurityGroupMembership
  ( VpcSecurityGroupMembership (..)
  -- * Smart constructor
  , mkVpcSecurityGroupMembership
  -- * Lenses
  , vsgmStatus
  , vsgmVpcSecurityGroupId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | This data type is used as a response element for queries on VPC security group membership.
--
-- /See:/ 'mkVpcSecurityGroupMembership' smart constructor.
data VpcSecurityGroupMembership = VpcSecurityGroupMembership'
  { status :: Core.Maybe Core.Text
    -- ^ The status of the VPC security group.
  , vpcSecurityGroupId :: Core.Maybe Core.Text
    -- ^ The name of the VPC security group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpcSecurityGroupMembership' value with any optional fields omitted.
mkVpcSecurityGroupMembership
    :: VpcSecurityGroupMembership
mkVpcSecurityGroupMembership
  = VpcSecurityGroupMembership'{status = Core.Nothing,
                                vpcSecurityGroupId = Core.Nothing}

-- | The status of the VPC security group.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsgmStatus :: Lens.Lens' VpcSecurityGroupMembership (Core.Maybe Core.Text)
vsgmStatus = Lens.field @"status"
{-# INLINEABLE vsgmStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The name of the VPC security group.
--
-- /Note:/ Consider using 'vpcSecurityGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vsgmVpcSecurityGroupId :: Lens.Lens' VpcSecurityGroupMembership (Core.Maybe Core.Text)
vsgmVpcSecurityGroupId = Lens.field @"vpcSecurityGroupId"
{-# INLINEABLE vsgmVpcSecurityGroupId #-}
{-# DEPRECATED vpcSecurityGroupId "Use generic-lens or generic-optics with 'vpcSecurityGroupId' instead"  #-}

instance Core.FromXML VpcSecurityGroupMembership where
        parseXML x
          = VpcSecurityGroupMembership' Core.<$>
              (x Core..@? "Status") Core.<*> x Core..@? "VpcSecurityGroupId"
