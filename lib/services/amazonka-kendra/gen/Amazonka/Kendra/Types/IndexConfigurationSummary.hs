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
-- Module      : Amazonka.Kendra.Types.IndexConfigurationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.IndexConfigurationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.IndexEdition
import Amazonka.Kendra.Types.IndexStatus
import qualified Amazonka.Prelude as Prelude

-- | Summary information on the configuration of an index.
--
-- /See:/ 'newIndexConfigurationSummary' smart constructor.
data IndexConfigurationSummary = IndexConfigurationSummary'
  { -- | Indicates whether the index is a Enterprise Edition index or a Developer
    -- Edition index.
    edition :: Prelude.Maybe IndexEdition,
    -- | A identifier for the index. Use this to identify the index when you are
    -- using APIs such as @Query@, @DescribeIndex@, @UpdateIndex@, and
    -- @DeleteIndex@.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the index.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp when the index was created.
    createdAt :: Data.POSIX,
    -- | The Unix timestamp when the index was last updated.
    updatedAt :: Data.POSIX,
    -- | The current status of the index. When the status is @ACTIVE@, the index
    -- is ready to search.
    status :: IndexStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IndexConfigurationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'edition', 'indexConfigurationSummary_edition' - Indicates whether the index is a Enterprise Edition index or a Developer
-- Edition index.
--
-- 'id', 'indexConfigurationSummary_id' - A identifier for the index. Use this to identify the index when you are
-- using APIs such as @Query@, @DescribeIndex@, @UpdateIndex@, and
-- @DeleteIndex@.
--
-- 'name', 'indexConfigurationSummary_name' - The name of the index.
--
-- 'createdAt', 'indexConfigurationSummary_createdAt' - The Unix timestamp when the index was created.
--
-- 'updatedAt', 'indexConfigurationSummary_updatedAt' - The Unix timestamp when the index was last updated.
--
-- 'status', 'indexConfigurationSummary_status' - The current status of the index. When the status is @ACTIVE@, the index
-- is ready to search.
newIndexConfigurationSummary ::
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'updatedAt'
  Prelude.UTCTime ->
  -- | 'status'
  IndexStatus ->
  IndexConfigurationSummary
newIndexConfigurationSummary
  pCreatedAt_
  pUpdatedAt_
  pStatus_ =
    IndexConfigurationSummary'
      { edition =
          Prelude.Nothing,
        id = Prelude.Nothing,
        name = Prelude.Nothing,
        createdAt = Data._Time Lens.# pCreatedAt_,
        updatedAt = Data._Time Lens.# pUpdatedAt_,
        status = pStatus_
      }

-- | Indicates whether the index is a Enterprise Edition index or a Developer
-- Edition index.
indexConfigurationSummary_edition :: Lens.Lens' IndexConfigurationSummary (Prelude.Maybe IndexEdition)
indexConfigurationSummary_edition = Lens.lens (\IndexConfigurationSummary' {edition} -> edition) (\s@IndexConfigurationSummary' {} a -> s {edition = a} :: IndexConfigurationSummary)

-- | A identifier for the index. Use this to identify the index when you are
-- using APIs such as @Query@, @DescribeIndex@, @UpdateIndex@, and
-- @DeleteIndex@.
indexConfigurationSummary_id :: Lens.Lens' IndexConfigurationSummary (Prelude.Maybe Prelude.Text)
indexConfigurationSummary_id = Lens.lens (\IndexConfigurationSummary' {id} -> id) (\s@IndexConfigurationSummary' {} a -> s {id = a} :: IndexConfigurationSummary)

-- | The name of the index.
indexConfigurationSummary_name :: Lens.Lens' IndexConfigurationSummary (Prelude.Maybe Prelude.Text)
indexConfigurationSummary_name = Lens.lens (\IndexConfigurationSummary' {name} -> name) (\s@IndexConfigurationSummary' {} a -> s {name = a} :: IndexConfigurationSummary)

-- | The Unix timestamp when the index was created.
indexConfigurationSummary_createdAt :: Lens.Lens' IndexConfigurationSummary Prelude.UTCTime
indexConfigurationSummary_createdAt = Lens.lens (\IndexConfigurationSummary' {createdAt} -> createdAt) (\s@IndexConfigurationSummary' {} a -> s {createdAt = a} :: IndexConfigurationSummary) Prelude.. Data._Time

-- | The Unix timestamp when the index was last updated.
indexConfigurationSummary_updatedAt :: Lens.Lens' IndexConfigurationSummary Prelude.UTCTime
indexConfigurationSummary_updatedAt = Lens.lens (\IndexConfigurationSummary' {updatedAt} -> updatedAt) (\s@IndexConfigurationSummary' {} a -> s {updatedAt = a} :: IndexConfigurationSummary) Prelude.. Data._Time

-- | The current status of the index. When the status is @ACTIVE@, the index
-- is ready to search.
indexConfigurationSummary_status :: Lens.Lens' IndexConfigurationSummary IndexStatus
indexConfigurationSummary_status = Lens.lens (\IndexConfigurationSummary' {status} -> status) (\s@IndexConfigurationSummary' {} a -> s {status = a} :: IndexConfigurationSummary)

instance Data.FromJSON IndexConfigurationSummary where
  parseJSON =
    Data.withObject
      "IndexConfigurationSummary"
      ( \x ->
          IndexConfigurationSummary'
            Prelude.<$> (x Data..:? "Edition")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..: "CreatedAt")
            Prelude.<*> (x Data..: "UpdatedAt")
            Prelude.<*> (x Data..: "Status")
      )

instance Prelude.Hashable IndexConfigurationSummary where
  hashWithSalt _salt IndexConfigurationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` edition
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` status

instance Prelude.NFData IndexConfigurationSummary where
  rnf IndexConfigurationSummary' {..} =
    Prelude.rnf edition
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf status
