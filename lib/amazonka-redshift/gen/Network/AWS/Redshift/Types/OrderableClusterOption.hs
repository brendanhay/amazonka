{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.OrderableClusterOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.OrderableClusterOption
  ( OrderableClusterOption (..),

    -- * Smart constructor
    mkOrderableClusterOption,

    -- * Lenses
    ocoAvailabilityZones,
    ocoClusterType,
    ocoClusterVersion,
    ocoNodeType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.AvailabilityZone

-- | Describes an orderable cluster option.
--
-- /See:/ 'mkOrderableClusterOption' smart constructor.
data OrderableClusterOption = OrderableClusterOption'
  { -- | A list of availability zones for the orderable cluster.
    availabilityZones :: Lude.Maybe [AvailabilityZone],
    -- | The cluster type, for example @multi-node@ .
    clusterType :: Lude.Maybe Lude.Text,
    -- | The version of the orderable cluster.
    clusterVersion :: Lude.Maybe Lude.Text,
    -- | The node type for the orderable cluster.
    nodeType :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OrderableClusterOption' with the minimum fields required to make a request.
--
-- * 'availabilityZones' - A list of availability zones for the orderable cluster.
-- * 'clusterType' - The cluster type, for example @multi-node@ .
-- * 'clusterVersion' - The version of the orderable cluster.
-- * 'nodeType' - The node type for the orderable cluster.
mkOrderableClusterOption ::
  OrderableClusterOption
mkOrderableClusterOption =
  OrderableClusterOption'
    { availabilityZones = Lude.Nothing,
      clusterType = Lude.Nothing,
      clusterVersion = Lude.Nothing,
      nodeType = Lude.Nothing
    }

-- | A list of availability zones for the orderable cluster.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocoAvailabilityZones :: Lens.Lens' OrderableClusterOption (Lude.Maybe [AvailabilityZone])
ocoAvailabilityZones = Lens.lens (availabilityZones :: OrderableClusterOption -> Lude.Maybe [AvailabilityZone]) (\s a -> s {availabilityZones = a} :: OrderableClusterOption)
{-# DEPRECATED ocoAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The cluster type, for example @multi-node@ .
--
-- /Note:/ Consider using 'clusterType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocoClusterType :: Lens.Lens' OrderableClusterOption (Lude.Maybe Lude.Text)
ocoClusterType = Lens.lens (clusterType :: OrderableClusterOption -> Lude.Maybe Lude.Text) (\s a -> s {clusterType = a} :: OrderableClusterOption)
{-# DEPRECATED ocoClusterType "Use generic-lens or generic-optics with 'clusterType' instead." #-}

-- | The version of the orderable cluster.
--
-- /Note:/ Consider using 'clusterVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocoClusterVersion :: Lens.Lens' OrderableClusterOption (Lude.Maybe Lude.Text)
ocoClusterVersion = Lens.lens (clusterVersion :: OrderableClusterOption -> Lude.Maybe Lude.Text) (\s a -> s {clusterVersion = a} :: OrderableClusterOption)
{-# DEPRECATED ocoClusterVersion "Use generic-lens or generic-optics with 'clusterVersion' instead." #-}

-- | The node type for the orderable cluster.
--
-- /Note:/ Consider using 'nodeType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocoNodeType :: Lens.Lens' OrderableClusterOption (Lude.Maybe Lude.Text)
ocoNodeType = Lens.lens (nodeType :: OrderableClusterOption -> Lude.Maybe Lude.Text) (\s a -> s {nodeType = a} :: OrderableClusterOption)
{-# DEPRECATED ocoNodeType "Use generic-lens or generic-optics with 'nodeType' instead." #-}

instance Lude.FromXML OrderableClusterOption where
  parseXML x =
    OrderableClusterOption'
      Lude.<$> ( x Lude..@? "AvailabilityZones" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "AvailabilityZone")
               )
      Lude.<*> (x Lude..@? "ClusterType")
      Lude.<*> (x Lude..@? "ClusterVersion")
      Lude.<*> (x Lude..@? "NodeType")
