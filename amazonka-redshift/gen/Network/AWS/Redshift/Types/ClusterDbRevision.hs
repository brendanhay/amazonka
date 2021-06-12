{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterDbRevision
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterDbRevision where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.RevisionTarget

-- | Describes a @ClusterDbRevision@.
--
-- /See:/ 'newClusterDbRevision' smart constructor.
data ClusterDbRevision = ClusterDbRevision'
  { -- | A string representing the current cluster version.
    currentDatabaseRevision :: Core.Maybe Core.Text,
    -- | A list of @RevisionTarget@ objects, where each object describes the
    -- database revision that a cluster can be updated to.
    revisionTargets :: Core.Maybe [RevisionTarget],
    -- | The unique identifier of the cluster.
    clusterIdentifier :: Core.Maybe Core.Text,
    -- | The date on which the database revision was released.
    databaseRevisionReleaseDate :: Core.Maybe Core.ISO8601
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ClusterDbRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentDatabaseRevision', 'clusterDbRevision_currentDatabaseRevision' - A string representing the current cluster version.
--
-- 'revisionTargets', 'clusterDbRevision_revisionTargets' - A list of @RevisionTarget@ objects, where each object describes the
-- database revision that a cluster can be updated to.
--
-- 'clusterIdentifier', 'clusterDbRevision_clusterIdentifier' - The unique identifier of the cluster.
--
-- 'databaseRevisionReleaseDate', 'clusterDbRevision_databaseRevisionReleaseDate' - The date on which the database revision was released.
newClusterDbRevision ::
  ClusterDbRevision
newClusterDbRevision =
  ClusterDbRevision'
    { currentDatabaseRevision =
        Core.Nothing,
      revisionTargets = Core.Nothing,
      clusterIdentifier = Core.Nothing,
      databaseRevisionReleaseDate = Core.Nothing
    }

-- | A string representing the current cluster version.
clusterDbRevision_currentDatabaseRevision :: Lens.Lens' ClusterDbRevision (Core.Maybe Core.Text)
clusterDbRevision_currentDatabaseRevision = Lens.lens (\ClusterDbRevision' {currentDatabaseRevision} -> currentDatabaseRevision) (\s@ClusterDbRevision' {} a -> s {currentDatabaseRevision = a} :: ClusterDbRevision)

-- | A list of @RevisionTarget@ objects, where each object describes the
-- database revision that a cluster can be updated to.
clusterDbRevision_revisionTargets :: Lens.Lens' ClusterDbRevision (Core.Maybe [RevisionTarget])
clusterDbRevision_revisionTargets = Lens.lens (\ClusterDbRevision' {revisionTargets} -> revisionTargets) (\s@ClusterDbRevision' {} a -> s {revisionTargets = a} :: ClusterDbRevision) Core.. Lens.mapping Lens._Coerce

-- | The unique identifier of the cluster.
clusterDbRevision_clusterIdentifier :: Lens.Lens' ClusterDbRevision (Core.Maybe Core.Text)
clusterDbRevision_clusterIdentifier = Lens.lens (\ClusterDbRevision' {clusterIdentifier} -> clusterIdentifier) (\s@ClusterDbRevision' {} a -> s {clusterIdentifier = a} :: ClusterDbRevision)

-- | The date on which the database revision was released.
clusterDbRevision_databaseRevisionReleaseDate :: Lens.Lens' ClusterDbRevision (Core.Maybe Core.UTCTime)
clusterDbRevision_databaseRevisionReleaseDate = Lens.lens (\ClusterDbRevision' {databaseRevisionReleaseDate} -> databaseRevisionReleaseDate) (\s@ClusterDbRevision' {} a -> s {databaseRevisionReleaseDate = a} :: ClusterDbRevision) Core.. Lens.mapping Core._Time

instance Core.FromXML ClusterDbRevision where
  parseXML x =
    ClusterDbRevision'
      Core.<$> (x Core..@? "CurrentDatabaseRevision")
      Core.<*> ( x Core..@? "RevisionTargets" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "RevisionTarget")
               )
      Core.<*> (x Core..@? "ClusterIdentifier")
      Core.<*> (x Core..@? "DatabaseRevisionReleaseDate")

instance Core.Hashable ClusterDbRevision

instance Core.NFData ClusterDbRevision
