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
-- Module      : Amazonka.Redshift.Types.ClusterDbRevision
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.ClusterDbRevision where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.RevisionTarget

-- | Describes a @ClusterDbRevision@.
--
-- /See:/ 'newClusterDbRevision' smart constructor.
data ClusterDbRevision = ClusterDbRevision'
  { -- | The unique identifier of the cluster.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A string representing the current cluster version.
    currentDatabaseRevision :: Prelude.Maybe Prelude.Text,
    -- | The date on which the database revision was released.
    databaseRevisionReleaseDate :: Prelude.Maybe Data.ISO8601,
    -- | A list of @RevisionTarget@ objects, where each object describes the
    -- database revision that a cluster can be updated to.
    revisionTargets :: Prelude.Maybe [RevisionTarget]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterDbRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterIdentifier', 'clusterDbRevision_clusterIdentifier' - The unique identifier of the cluster.
--
-- 'currentDatabaseRevision', 'clusterDbRevision_currentDatabaseRevision' - A string representing the current cluster version.
--
-- 'databaseRevisionReleaseDate', 'clusterDbRevision_databaseRevisionReleaseDate' - The date on which the database revision was released.
--
-- 'revisionTargets', 'clusterDbRevision_revisionTargets' - A list of @RevisionTarget@ objects, where each object describes the
-- database revision that a cluster can be updated to.
newClusterDbRevision ::
  ClusterDbRevision
newClusterDbRevision =
  ClusterDbRevision'
    { clusterIdentifier =
        Prelude.Nothing,
      currentDatabaseRevision = Prelude.Nothing,
      databaseRevisionReleaseDate = Prelude.Nothing,
      revisionTargets = Prelude.Nothing
    }

-- | The unique identifier of the cluster.
clusterDbRevision_clusterIdentifier :: Lens.Lens' ClusterDbRevision (Prelude.Maybe Prelude.Text)
clusterDbRevision_clusterIdentifier = Lens.lens (\ClusterDbRevision' {clusterIdentifier} -> clusterIdentifier) (\s@ClusterDbRevision' {} a -> s {clusterIdentifier = a} :: ClusterDbRevision)

-- | A string representing the current cluster version.
clusterDbRevision_currentDatabaseRevision :: Lens.Lens' ClusterDbRevision (Prelude.Maybe Prelude.Text)
clusterDbRevision_currentDatabaseRevision = Lens.lens (\ClusterDbRevision' {currentDatabaseRevision} -> currentDatabaseRevision) (\s@ClusterDbRevision' {} a -> s {currentDatabaseRevision = a} :: ClusterDbRevision)

-- | The date on which the database revision was released.
clusterDbRevision_databaseRevisionReleaseDate :: Lens.Lens' ClusterDbRevision (Prelude.Maybe Prelude.UTCTime)
clusterDbRevision_databaseRevisionReleaseDate = Lens.lens (\ClusterDbRevision' {databaseRevisionReleaseDate} -> databaseRevisionReleaseDate) (\s@ClusterDbRevision' {} a -> s {databaseRevisionReleaseDate = a} :: ClusterDbRevision) Prelude.. Lens.mapping Data._Time

-- | A list of @RevisionTarget@ objects, where each object describes the
-- database revision that a cluster can be updated to.
clusterDbRevision_revisionTargets :: Lens.Lens' ClusterDbRevision (Prelude.Maybe [RevisionTarget])
clusterDbRevision_revisionTargets = Lens.lens (\ClusterDbRevision' {revisionTargets} -> revisionTargets) (\s@ClusterDbRevision' {} a -> s {revisionTargets = a} :: ClusterDbRevision) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML ClusterDbRevision where
  parseXML x =
    ClusterDbRevision'
      Prelude.<$> (x Data..@? "ClusterIdentifier")
      Prelude.<*> (x Data..@? "CurrentDatabaseRevision")
      Prelude.<*> (x Data..@? "DatabaseRevisionReleaseDate")
      Prelude.<*> ( x Data..@? "RevisionTargets" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "RevisionTarget")
                  )

instance Prelude.Hashable ClusterDbRevision where
  hashWithSalt _salt ClusterDbRevision' {..} =
    _salt `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` currentDatabaseRevision
      `Prelude.hashWithSalt` databaseRevisionReleaseDate
      `Prelude.hashWithSalt` revisionTargets

instance Prelude.NFData ClusterDbRevision where
  rnf ClusterDbRevision' {..} =
    Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf currentDatabaseRevision
      `Prelude.seq` Prelude.rnf databaseRevisionReleaseDate
      `Prelude.seq` Prelude.rnf revisionTargets
