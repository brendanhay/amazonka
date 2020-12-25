{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.RevisionTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.RevisionTarget
  ( RevisionTarget (..),

    -- * Smart constructor
    mkRevisionTarget,

    -- * Lenses
    rtDatabaseRevision,
    rtDatabaseRevisionReleaseDate,
    rtDescription,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.DatabaseRevision as Types
import qualified Network.AWS.Redshift.Types.Description as Types

-- | Describes a @RevisionTarget@ .
--
-- /See:/ 'mkRevisionTarget' smart constructor.
data RevisionTarget = RevisionTarget'
  { -- | A unique string that identifies the version to update the cluster to. You can use this value in 'ModifyClusterDbRevision' .
    databaseRevision :: Core.Maybe Types.DatabaseRevision,
    -- | The date on which the database revision was released.
    databaseRevisionReleaseDate :: Core.Maybe Core.UTCTime,
    -- | A string that describes the changes and features that will be applied to the cluster when it is updated to the corresponding 'ClusterDbRevision' .
    description :: Core.Maybe Types.Description
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'RevisionTarget' value with any optional fields omitted.
mkRevisionTarget ::
  RevisionTarget
mkRevisionTarget =
  RevisionTarget'
    { databaseRevision = Core.Nothing,
      databaseRevisionReleaseDate = Core.Nothing,
      description = Core.Nothing
    }

-- | A unique string that identifies the version to update the cluster to. You can use this value in 'ModifyClusterDbRevision' .
--
-- /Note:/ Consider using 'databaseRevision' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtDatabaseRevision :: Lens.Lens' RevisionTarget (Core.Maybe Types.DatabaseRevision)
rtDatabaseRevision = Lens.field @"databaseRevision"
{-# DEPRECATED rtDatabaseRevision "Use generic-lens or generic-optics with 'databaseRevision' instead." #-}

-- | The date on which the database revision was released.
--
-- /Note:/ Consider using 'databaseRevisionReleaseDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtDatabaseRevisionReleaseDate :: Lens.Lens' RevisionTarget (Core.Maybe Core.UTCTime)
rtDatabaseRevisionReleaseDate = Lens.field @"databaseRevisionReleaseDate"
{-# DEPRECATED rtDatabaseRevisionReleaseDate "Use generic-lens or generic-optics with 'databaseRevisionReleaseDate' instead." #-}

-- | A string that describes the changes and features that will be applied to the cluster when it is updated to the corresponding 'ClusterDbRevision' .
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtDescription :: Lens.Lens' RevisionTarget (Core.Maybe Types.Description)
rtDescription = Lens.field @"description"
{-# DEPRECATED rtDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Core.FromXML RevisionTarget where
  parseXML x =
    RevisionTarget'
      Core.<$> (x Core..@? "DatabaseRevision")
      Core.<*> (x Core..@? "DatabaseRevisionReleaseDate")
      Core.<*> (x Core..@? "Description")
