{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterDbRevision
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.ClusterDbRevision
  ( ClusterDbRevision (..)
  -- * Smart constructor
  , mkClusterDbRevision
  -- * Lenses
  , cdrClusterIdentifier
  , cdrCurrentDatabaseRevision
  , cdrDatabaseRevisionReleaseDate
  , cdrRevisionTargets
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.RevisionTarget as Types

-- | Describes a @ClusterDbRevision@ .
--
-- /See:/ 'mkClusterDbRevision' smart constructor.
data ClusterDbRevision = ClusterDbRevision'
  { clusterIdentifier :: Core.Maybe Core.Text
    -- ^ The unique identifier of the cluster.
  , currentDatabaseRevision :: Core.Maybe Core.Text
    -- ^ A string representing the current cluster version.
  , databaseRevisionReleaseDate :: Core.Maybe Core.UTCTime
    -- ^ The date on which the database revision was released.
  , revisionTargets :: Core.Maybe [Types.RevisionTarget]
    -- ^ A list of @RevisionTarget@ objects, where each object describes the database revision that a cluster can be updated to.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ClusterDbRevision' value with any optional fields omitted.
mkClusterDbRevision
    :: ClusterDbRevision
mkClusterDbRevision
  = ClusterDbRevision'{clusterIdentifier = Core.Nothing,
                       currentDatabaseRevision = Core.Nothing,
                       databaseRevisionReleaseDate = Core.Nothing,
                       revisionTargets = Core.Nothing}

-- | The unique identifier of the cluster.
--
-- /Note:/ Consider using 'clusterIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrClusterIdentifier :: Lens.Lens' ClusterDbRevision (Core.Maybe Core.Text)
cdrClusterIdentifier = Lens.field @"clusterIdentifier"
{-# INLINEABLE cdrClusterIdentifier #-}
{-# DEPRECATED clusterIdentifier "Use generic-lens or generic-optics with 'clusterIdentifier' instead"  #-}

-- | A string representing the current cluster version.
--
-- /Note:/ Consider using 'currentDatabaseRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrCurrentDatabaseRevision :: Lens.Lens' ClusterDbRevision (Core.Maybe Core.Text)
cdrCurrentDatabaseRevision = Lens.field @"currentDatabaseRevision"
{-# INLINEABLE cdrCurrentDatabaseRevision #-}
{-# DEPRECATED currentDatabaseRevision "Use generic-lens or generic-optics with 'currentDatabaseRevision' instead"  #-}

-- | The date on which the database revision was released.
--
-- /Note:/ Consider using 'databaseRevisionReleaseDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrDatabaseRevisionReleaseDate :: Lens.Lens' ClusterDbRevision (Core.Maybe Core.UTCTime)
cdrDatabaseRevisionReleaseDate = Lens.field @"databaseRevisionReleaseDate"
{-# INLINEABLE cdrDatabaseRevisionReleaseDate #-}
{-# DEPRECATED databaseRevisionReleaseDate "Use generic-lens or generic-optics with 'databaseRevisionReleaseDate' instead"  #-}

-- | A list of @RevisionTarget@ objects, where each object describes the database revision that a cluster can be updated to.
--
-- /Note:/ Consider using 'revisionTargets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdrRevisionTargets :: Lens.Lens' ClusterDbRevision (Core.Maybe [Types.RevisionTarget])
cdrRevisionTargets = Lens.field @"revisionTargets"
{-# INLINEABLE cdrRevisionTargets #-}
{-# DEPRECATED revisionTargets "Use generic-lens or generic-optics with 'revisionTargets' instead"  #-}

instance Core.FromXML ClusterDbRevision where
        parseXML x
          = ClusterDbRevision' Core.<$>
              (x Core..@? "ClusterIdentifier") Core.<*>
                x Core..@? "CurrentDatabaseRevision"
                Core.<*> x Core..@? "DatabaseRevisionReleaseDate"
                Core.<*>
                x Core..@? "RevisionTargets" Core..<@>
                  Core.parseXMLList "RevisionTarget"
