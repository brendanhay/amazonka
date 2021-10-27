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
-- Module      : Network.AWS.Kendra.Types.IndexConfigurationSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.IndexConfigurationSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.IndexEdition
import Network.AWS.Kendra.Types.IndexStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A summary of information about an index.
--
-- /See:/ 'newIndexConfigurationSummary' smart constructor.
data IndexConfigurationSummary = IndexConfigurationSummary'
  { -- | Indicates whether the index is a enterprise edition index or a developer
    -- edition index.
    edition :: Prelude.Maybe IndexEdition,
    -- | The name of the index.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the index. Use this to identify the index when
    -- you are using operations such as @Query@, @DescribeIndex@,
    -- @UpdateIndex@, and @DeleteIndex@.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Unix timestamp when the index was created.
    createdAt :: Core.POSIX,
    -- | The Unix timestamp when the index was last updated by the @UpdateIndex@
    -- operation.
    updatedAt :: Core.POSIX,
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
-- 'edition', 'indexConfigurationSummary_edition' - Indicates whether the index is a enterprise edition index or a developer
-- edition index.
--
-- 'name', 'indexConfigurationSummary_name' - The name of the index.
--
-- 'id', 'indexConfigurationSummary_id' - A unique identifier for the index. Use this to identify the index when
-- you are using operations such as @Query@, @DescribeIndex@,
-- @UpdateIndex@, and @DeleteIndex@.
--
-- 'createdAt', 'indexConfigurationSummary_createdAt' - The Unix timestamp when the index was created.
--
-- 'updatedAt', 'indexConfigurationSummary_updatedAt' - The Unix timestamp when the index was last updated by the @UpdateIndex@
-- operation.
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
        name = Prelude.Nothing,
        id = Prelude.Nothing,
        createdAt = Core._Time Lens.# pCreatedAt_,
        updatedAt = Core._Time Lens.# pUpdatedAt_,
        status = pStatus_
      }

-- | Indicates whether the index is a enterprise edition index or a developer
-- edition index.
indexConfigurationSummary_edition :: Lens.Lens' IndexConfigurationSummary (Prelude.Maybe IndexEdition)
indexConfigurationSummary_edition = Lens.lens (\IndexConfigurationSummary' {edition} -> edition) (\s@IndexConfigurationSummary' {} a -> s {edition = a} :: IndexConfigurationSummary)

-- | The name of the index.
indexConfigurationSummary_name :: Lens.Lens' IndexConfigurationSummary (Prelude.Maybe Prelude.Text)
indexConfigurationSummary_name = Lens.lens (\IndexConfigurationSummary' {name} -> name) (\s@IndexConfigurationSummary' {} a -> s {name = a} :: IndexConfigurationSummary)

-- | A unique identifier for the index. Use this to identify the index when
-- you are using operations such as @Query@, @DescribeIndex@,
-- @UpdateIndex@, and @DeleteIndex@.
indexConfigurationSummary_id :: Lens.Lens' IndexConfigurationSummary (Prelude.Maybe Prelude.Text)
indexConfigurationSummary_id = Lens.lens (\IndexConfigurationSummary' {id} -> id) (\s@IndexConfigurationSummary' {} a -> s {id = a} :: IndexConfigurationSummary)

-- | The Unix timestamp when the index was created.
indexConfigurationSummary_createdAt :: Lens.Lens' IndexConfigurationSummary Prelude.UTCTime
indexConfigurationSummary_createdAt = Lens.lens (\IndexConfigurationSummary' {createdAt} -> createdAt) (\s@IndexConfigurationSummary' {} a -> s {createdAt = a} :: IndexConfigurationSummary) Prelude.. Core._Time

-- | The Unix timestamp when the index was last updated by the @UpdateIndex@
-- operation.
indexConfigurationSummary_updatedAt :: Lens.Lens' IndexConfigurationSummary Prelude.UTCTime
indexConfigurationSummary_updatedAt = Lens.lens (\IndexConfigurationSummary' {updatedAt} -> updatedAt) (\s@IndexConfigurationSummary' {} a -> s {updatedAt = a} :: IndexConfigurationSummary) Prelude.. Core._Time

-- | The current status of the index. When the status is @ACTIVE@, the index
-- is ready to search.
indexConfigurationSummary_status :: Lens.Lens' IndexConfigurationSummary IndexStatus
indexConfigurationSummary_status = Lens.lens (\IndexConfigurationSummary' {status} -> status) (\s@IndexConfigurationSummary' {} a -> s {status = a} :: IndexConfigurationSummary)

instance Core.FromJSON IndexConfigurationSummary where
  parseJSON =
    Core.withObject
      "IndexConfigurationSummary"
      ( \x ->
          IndexConfigurationSummary'
            Prelude.<$> (x Core..:? "Edition")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..: "CreatedAt")
            Prelude.<*> (x Core..: "UpdatedAt")
            Prelude.<*> (x Core..: "Status")
      )

instance Prelude.Hashable IndexConfigurationSummary

instance Prelude.NFData IndexConfigurationSummary
