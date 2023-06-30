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
-- Module      : Amazonka.Redshift.Types.RevisionTarget
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.RevisionTarget where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal

-- | Describes a @RevisionTarget@.
--
-- /See:/ 'newRevisionTarget' smart constructor.
data RevisionTarget = RevisionTarget'
  { -- | A unique string that identifies the version to update the cluster to.
    -- You can use this value in ModifyClusterDbRevision.
    databaseRevision :: Prelude.Maybe Prelude.Text,
    -- | The date on which the database revision was released.
    databaseRevisionReleaseDate :: Prelude.Maybe Data.ISO8601,
    -- | A string that describes the changes and features that will be applied to
    -- the cluster when it is updated to the corresponding ClusterDbRevision.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RevisionTarget' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'databaseRevision', 'revisionTarget_databaseRevision' - A unique string that identifies the version to update the cluster to.
-- You can use this value in ModifyClusterDbRevision.
--
-- 'databaseRevisionReleaseDate', 'revisionTarget_databaseRevisionReleaseDate' - The date on which the database revision was released.
--
-- 'description', 'revisionTarget_description' - A string that describes the changes and features that will be applied to
-- the cluster when it is updated to the corresponding ClusterDbRevision.
newRevisionTarget ::
  RevisionTarget
newRevisionTarget =
  RevisionTarget'
    { databaseRevision = Prelude.Nothing,
      databaseRevisionReleaseDate = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | A unique string that identifies the version to update the cluster to.
-- You can use this value in ModifyClusterDbRevision.
revisionTarget_databaseRevision :: Lens.Lens' RevisionTarget (Prelude.Maybe Prelude.Text)
revisionTarget_databaseRevision = Lens.lens (\RevisionTarget' {databaseRevision} -> databaseRevision) (\s@RevisionTarget' {} a -> s {databaseRevision = a} :: RevisionTarget)

-- | The date on which the database revision was released.
revisionTarget_databaseRevisionReleaseDate :: Lens.Lens' RevisionTarget (Prelude.Maybe Prelude.UTCTime)
revisionTarget_databaseRevisionReleaseDate = Lens.lens (\RevisionTarget' {databaseRevisionReleaseDate} -> databaseRevisionReleaseDate) (\s@RevisionTarget' {} a -> s {databaseRevisionReleaseDate = a} :: RevisionTarget) Prelude.. Lens.mapping Data._Time

-- | A string that describes the changes and features that will be applied to
-- the cluster when it is updated to the corresponding ClusterDbRevision.
revisionTarget_description :: Lens.Lens' RevisionTarget (Prelude.Maybe Prelude.Text)
revisionTarget_description = Lens.lens (\RevisionTarget' {description} -> description) (\s@RevisionTarget' {} a -> s {description = a} :: RevisionTarget)

instance Data.FromXML RevisionTarget where
  parseXML x =
    RevisionTarget'
      Prelude.<$> (x Data..@? "DatabaseRevision")
      Prelude.<*> (x Data..@? "DatabaseRevisionReleaseDate")
      Prelude.<*> (x Data..@? "Description")

instance Prelude.Hashable RevisionTarget where
  hashWithSalt _salt RevisionTarget' {..} =
    _salt
      `Prelude.hashWithSalt` databaseRevision
      `Prelude.hashWithSalt` databaseRevisionReleaseDate
      `Prelude.hashWithSalt` description

instance Prelude.NFData RevisionTarget where
  rnf RevisionTarget' {..} =
    Prelude.rnf databaseRevision
      `Prelude.seq` Prelude.rnf databaseRevisionReleaseDate
      `Prelude.seq` Prelude.rnf description
