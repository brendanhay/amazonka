{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ClusterListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Snowball.Types.ClusterListEntry
  ( ClusterListEntry (..)
  -- * Smart constructor
  , mkClusterListEntry
  -- * Lenses
  , cleClusterId
  , cleClusterState
  , cleCreationDate
  , cleDescription
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.ClusterState as Types

-- | Contains a cluster's state, a cluster's ID, and other important information.
--
-- /See:/ 'mkClusterListEntry' smart constructor.
data ClusterListEntry = ClusterListEntry'
  { clusterId :: Core.Maybe Core.Text
    -- ^ The 39-character ID for the cluster that you want to list, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
  , clusterState :: Core.Maybe Types.ClusterState
    -- ^ The current state of this cluster. For information about the state of a specific node, see 'JobListEntry$JobState' .
  , creationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The creation date for this cluster.
  , description :: Core.Maybe Core.Text
    -- ^ Defines an optional description of the cluster, for example @Environmental Data Cluster-01@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ClusterListEntry' value with any optional fields omitted.
mkClusterListEntry
    :: ClusterListEntry
mkClusterListEntry
  = ClusterListEntry'{clusterId = Core.Nothing,
                      clusterState = Core.Nothing, creationDate = Core.Nothing,
                      description = Core.Nothing}

-- | The 39-character ID for the cluster that you want to list, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cleClusterId :: Lens.Lens' ClusterListEntry (Core.Maybe Core.Text)
cleClusterId = Lens.field @"clusterId"
{-# INLINEABLE cleClusterId #-}
{-# DEPRECATED clusterId "Use generic-lens or generic-optics with 'clusterId' instead"  #-}

-- | The current state of this cluster. For information about the state of a specific node, see 'JobListEntry$JobState' .
--
-- /Note:/ Consider using 'clusterState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cleClusterState :: Lens.Lens' ClusterListEntry (Core.Maybe Types.ClusterState)
cleClusterState = Lens.field @"clusterState"
{-# INLINEABLE cleClusterState #-}
{-# DEPRECATED clusterState "Use generic-lens or generic-optics with 'clusterState' instead"  #-}

-- | The creation date for this cluster.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cleCreationDate :: Lens.Lens' ClusterListEntry (Core.Maybe Core.NominalDiffTime)
cleCreationDate = Lens.field @"creationDate"
{-# INLINEABLE cleCreationDate #-}
{-# DEPRECATED creationDate "Use generic-lens or generic-optics with 'creationDate' instead"  #-}

-- | Defines an optional description of the cluster, for example @Environmental Data Cluster-01@ .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cleDescription :: Lens.Lens' ClusterListEntry (Core.Maybe Core.Text)
cleDescription = Lens.field @"description"
{-# INLINEABLE cleDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

instance Core.FromJSON ClusterListEntry where
        parseJSON
          = Core.withObject "ClusterListEntry" Core.$
              \ x ->
                ClusterListEntry' Core.<$>
                  (x Core..:? "ClusterId") Core.<*> x Core..:? "ClusterState"
                    Core.<*> x Core..:? "CreationDate"
                    Core.<*> x Core..:? "Description"
