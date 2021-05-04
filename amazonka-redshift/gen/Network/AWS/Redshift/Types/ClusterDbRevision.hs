{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.RevisionTarget

-- | Describes a @ClusterDbRevision@.
--
-- /See:/ 'newClusterDbRevision' smart constructor.
data ClusterDbRevision = ClusterDbRevision'
  { -- | A string representing the current cluster version.
    currentDatabaseRevision :: Prelude.Maybe Prelude.Text,
    -- | A list of @RevisionTarget@ objects, where each object describes the
    -- database revision that a cluster can be updated to.
    revisionTargets :: Prelude.Maybe [RevisionTarget],
    -- | The unique identifier of the cluster.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The date on which the database revision was released.
    databaseRevisionReleaseDate :: Prelude.Maybe Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      revisionTargets = Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      databaseRevisionReleaseDate = Prelude.Nothing
    }

-- | A string representing the current cluster version.
clusterDbRevision_currentDatabaseRevision :: Lens.Lens' ClusterDbRevision (Prelude.Maybe Prelude.Text)
clusterDbRevision_currentDatabaseRevision = Lens.lens (\ClusterDbRevision' {currentDatabaseRevision} -> currentDatabaseRevision) (\s@ClusterDbRevision' {} a -> s {currentDatabaseRevision = a} :: ClusterDbRevision)

-- | A list of @RevisionTarget@ objects, where each object describes the
-- database revision that a cluster can be updated to.
clusterDbRevision_revisionTargets :: Lens.Lens' ClusterDbRevision (Prelude.Maybe [RevisionTarget])
clusterDbRevision_revisionTargets = Lens.lens (\ClusterDbRevision' {revisionTargets} -> revisionTargets) (\s@ClusterDbRevision' {} a -> s {revisionTargets = a} :: ClusterDbRevision) Prelude.. Lens.mapping Prelude._Coerce

-- | The unique identifier of the cluster.
clusterDbRevision_clusterIdentifier :: Lens.Lens' ClusterDbRevision (Prelude.Maybe Prelude.Text)
clusterDbRevision_clusterIdentifier = Lens.lens (\ClusterDbRevision' {clusterIdentifier} -> clusterIdentifier) (\s@ClusterDbRevision' {} a -> s {clusterIdentifier = a} :: ClusterDbRevision)

-- | The date on which the database revision was released.
clusterDbRevision_databaseRevisionReleaseDate :: Lens.Lens' ClusterDbRevision (Prelude.Maybe Prelude.UTCTime)
clusterDbRevision_databaseRevisionReleaseDate = Lens.lens (\ClusterDbRevision' {databaseRevisionReleaseDate} -> databaseRevisionReleaseDate) (\s@ClusterDbRevision' {} a -> s {databaseRevisionReleaseDate = a} :: ClusterDbRevision) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromXML ClusterDbRevision where
  parseXML x =
    ClusterDbRevision'
      Prelude.<$> (x Prelude..@? "CurrentDatabaseRevision")
      Prelude.<*> ( x Prelude..@? "RevisionTargets"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "RevisionTarget")
                  )
      Prelude.<*> (x Prelude..@? "ClusterIdentifier")
      Prelude.<*> (x Prelude..@? "DatabaseRevisionReleaseDate")

instance Prelude.Hashable ClusterDbRevision

instance Prelude.NFData ClusterDbRevision
