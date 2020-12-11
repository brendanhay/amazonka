-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterSecurityGroupMembership
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterSecurityGroupMembership
  ( ClusterSecurityGroupMembership (..),

    -- * Smart constructor
    mkClusterSecurityGroupMembership,

    -- * Lenses
    csgmStatus,
    csgmClusterSecurityGroupName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | Describes a cluster security group.
--
-- /See:/ 'mkClusterSecurityGroupMembership' smart constructor.
data ClusterSecurityGroupMembership = ClusterSecurityGroupMembership'
  { status ::
      Lude.Maybe Lude.Text,
    clusterSecurityGroupName ::
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

-- | Creates a value of 'ClusterSecurityGroupMembership' with the minimum fields required to make a request.
--
-- * 'clusterSecurityGroupName' - The name of the cluster security group.
-- * 'status' - The status of the cluster security group.
mkClusterSecurityGroupMembership ::
  ClusterSecurityGroupMembership
mkClusterSecurityGroupMembership =
  ClusterSecurityGroupMembership'
    { status = Lude.Nothing,
      clusterSecurityGroupName = Lude.Nothing
    }

-- | The status of the cluster security group.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgmStatus :: Lens.Lens' ClusterSecurityGroupMembership (Lude.Maybe Lude.Text)
csgmStatus = Lens.lens (status :: ClusterSecurityGroupMembership -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: ClusterSecurityGroupMembership)
{-# DEPRECATED csgmStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the cluster security group.
--
-- /Note:/ Consider using 'clusterSecurityGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csgmClusterSecurityGroupName :: Lens.Lens' ClusterSecurityGroupMembership (Lude.Maybe Lude.Text)
csgmClusterSecurityGroupName = Lens.lens (clusterSecurityGroupName :: ClusterSecurityGroupMembership -> Lude.Maybe Lude.Text) (\s a -> s {clusterSecurityGroupName = a} :: ClusterSecurityGroupMembership)
{-# DEPRECATED csgmClusterSecurityGroupName "Use generic-lens or generic-optics with 'clusterSecurityGroupName' instead." #-}

instance Lude.FromXML ClusterSecurityGroupMembership where
  parseXML x =
    ClusterSecurityGroupMembership'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "ClusterSecurityGroupName")
