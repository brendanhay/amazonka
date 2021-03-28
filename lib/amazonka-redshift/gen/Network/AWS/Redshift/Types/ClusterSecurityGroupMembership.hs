{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterSecurityGroupMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.ClusterSecurityGroupMembership
  ( ClusterSecurityGroupMembership (..)
  -- * Smart constructor
  , mkClusterSecurityGroupMembership
  -- * Lenses
  , csgmClusterSecurityGroupName
  , csgmStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

-- | Describes a cluster security group.
--
-- /See:/ 'mkClusterSecurityGroupMembership' smart constructor.
data ClusterSecurityGroupMembership = ClusterSecurityGroupMembership'
  { clusterSecurityGroupName :: Core.Maybe Core.Text
    -- ^ The name of the cluster security group.
  , status :: Core.Maybe Core.Text
    -- ^ The status of the cluster security group.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClusterSecurityGroupMembership' value with any optional fields omitted.
mkClusterSecurityGroupMembership
    :: ClusterSecurityGroupMembership
mkClusterSecurityGroupMembership
  = ClusterSecurityGroupMembership'{clusterSecurityGroupName =
                                      Core.Nothing,
                                    status = Core.Nothing}

-- | The name of the cluster security group.
--
-- /Note:/ Consider using 'clusterSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgmClusterSecurityGroupName :: Lens.Lens' ClusterSecurityGroupMembership (Core.Maybe Core.Text)
csgmClusterSecurityGroupName = Lens.field @"clusterSecurityGroupName"
{-# INLINEABLE csgmClusterSecurityGroupName #-}
{-# DEPRECATED clusterSecurityGroupName "Use generic-lens or generic-optics with 'clusterSecurityGroupName' instead"  #-}

-- | The status of the cluster security group.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgmStatus :: Lens.Lens' ClusterSecurityGroupMembership (Core.Maybe Core.Text)
csgmStatus = Lens.field @"status"
{-# INLINEABLE csgmStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

instance Core.FromXML ClusterSecurityGroupMembership where
        parseXML x
          = ClusterSecurityGroupMembership' Core.<$>
              (x Core..@? "ClusterSecurityGroupName") Core.<*>
                x Core..@? "Status"
