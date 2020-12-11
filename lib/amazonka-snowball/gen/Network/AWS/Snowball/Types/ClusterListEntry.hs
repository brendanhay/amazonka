-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.ClusterListEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.ClusterListEntry
  ( ClusterListEntry (..),

    -- * Smart constructor
    mkClusterListEntry,

    -- * Lenses
    cleClusterState,
    cleClusterId,
    cleCreationDate,
    cleDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Snowball.Types.ClusterState

-- | Contains a cluster's state, a cluster's ID, and other important information.
--
-- /See:/ 'mkClusterListEntry' smart constructor.
data ClusterListEntry = ClusterListEntry'
  { clusterState ::
      Lude.Maybe ClusterState,
    clusterId :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Timestamp,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterListEntry' with the minimum fields required to make a request.
--
-- * 'clusterId' - The 39-character ID for the cluster that you want to list, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
-- * 'clusterState' - The current state of this cluster. For information about the state of a specific node, see 'JobListEntry$JobState' .
-- * 'creationDate' - The creation date for this cluster.
-- * 'description' - Defines an optional description of the cluster, for example @Environmental Data Cluster-01@ .
mkClusterListEntry ::
  ClusterListEntry
mkClusterListEntry =
  ClusterListEntry'
    { clusterState = Lude.Nothing,
      clusterId = Lude.Nothing,
      creationDate = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The current state of this cluster. For information about the state of a specific node, see 'JobListEntry$JobState' .
--
-- /Note:/ Consider using 'clusterState' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cleClusterState :: Lens.Lens' ClusterListEntry (Lude.Maybe ClusterState)
cleClusterState = Lens.lens (clusterState :: ClusterListEntry -> Lude.Maybe ClusterState) (\s a -> s {clusterState = a} :: ClusterListEntry)
{-# DEPRECATED cleClusterState "Use generic-lens or generic-optics with 'clusterState' instead." #-}

-- | The 39-character ID for the cluster that you want to list, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cleClusterId :: Lens.Lens' ClusterListEntry (Lude.Maybe Lude.Text)
cleClusterId = Lens.lens (clusterId :: ClusterListEntry -> Lude.Maybe Lude.Text) (\s a -> s {clusterId = a} :: ClusterListEntry)
{-# DEPRECATED cleClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The creation date for this cluster.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cleCreationDate :: Lens.Lens' ClusterListEntry (Lude.Maybe Lude.Timestamp)
cleCreationDate = Lens.lens (creationDate :: ClusterListEntry -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: ClusterListEntry)
{-# DEPRECATED cleCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | Defines an optional description of the cluster, for example @Environmental Data Cluster-01@ .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cleDescription :: Lens.Lens' ClusterListEntry (Lude.Maybe Lude.Text)
cleDescription = Lens.lens (description :: ClusterListEntry -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: ClusterListEntry)
{-# DEPRECATED cleDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON ClusterListEntry where
  parseJSON =
    Lude.withObject
      "ClusterListEntry"
      ( \x ->
          ClusterListEntry'
            Lude.<$> (x Lude..:? "ClusterState")
            Lude.<*> (x Lude..:? "ClusterId")
            Lude.<*> (x Lude..:? "CreationDate")
            Lude.<*> (x Lude..:? "Description")
      )
