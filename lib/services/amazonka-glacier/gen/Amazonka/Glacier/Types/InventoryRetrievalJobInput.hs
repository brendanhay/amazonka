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
-- Module      : Amazonka.Glacier.Types.InventoryRetrievalJobInput
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.InventoryRetrievalJobInput where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides options for specifying a range inventory retrieval job.
--
-- /See:/ 'newInventoryRetrievalJobInput' smart constructor.
data InventoryRetrievalJobInput = InventoryRetrievalJobInput'
  { -- | An opaque string that represents where to continue pagination of the
    -- vault inventory retrieval results. You use the marker in a new
    -- __InitiateJob__ request to obtain additional inventory items. If there
    -- are no more inventory items, this value is @null@.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The end of the date range in UTC for vault inventory retrieval that
    -- includes archives created before this date. This value should be a
    -- string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@.
    endDate :: Prelude.Maybe Prelude.Text,
    -- | Specifies the maximum number of inventory items returned per vault
    -- inventory retrieval request. Valid values are greater than or equal to
    -- 1.
    limit :: Prelude.Maybe Prelude.Text,
    -- | The start of the date range in UTC for vault inventory retrieval that
    -- includes archives created on or after this date. This value should be a
    -- string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@.
    startDate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InventoryRetrievalJobInput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'marker', 'inventoryRetrievalJobInput_marker' - An opaque string that represents where to continue pagination of the
-- vault inventory retrieval results. You use the marker in a new
-- __InitiateJob__ request to obtain additional inventory items. If there
-- are no more inventory items, this value is @null@.
--
-- 'endDate', 'inventoryRetrievalJobInput_endDate' - The end of the date range in UTC for vault inventory retrieval that
-- includes archives created before this date. This value should be a
-- string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@.
--
-- 'limit', 'inventoryRetrievalJobInput_limit' - Specifies the maximum number of inventory items returned per vault
-- inventory retrieval request. Valid values are greater than or equal to
-- 1.
--
-- 'startDate', 'inventoryRetrievalJobInput_startDate' - The start of the date range in UTC for vault inventory retrieval that
-- includes archives created on or after this date. This value should be a
-- string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@.
newInventoryRetrievalJobInput ::
  InventoryRetrievalJobInput
newInventoryRetrievalJobInput =
  InventoryRetrievalJobInput'
    { marker =
        Prelude.Nothing,
      endDate = Prelude.Nothing,
      limit = Prelude.Nothing,
      startDate = Prelude.Nothing
    }

-- | An opaque string that represents where to continue pagination of the
-- vault inventory retrieval results. You use the marker in a new
-- __InitiateJob__ request to obtain additional inventory items. If there
-- are no more inventory items, this value is @null@.
inventoryRetrievalJobInput_marker :: Lens.Lens' InventoryRetrievalJobInput (Prelude.Maybe Prelude.Text)
inventoryRetrievalJobInput_marker = Lens.lens (\InventoryRetrievalJobInput' {marker} -> marker) (\s@InventoryRetrievalJobInput' {} a -> s {marker = a} :: InventoryRetrievalJobInput)

-- | The end of the date range in UTC for vault inventory retrieval that
-- includes archives created before this date. This value should be a
-- string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@.
inventoryRetrievalJobInput_endDate :: Lens.Lens' InventoryRetrievalJobInput (Prelude.Maybe Prelude.Text)
inventoryRetrievalJobInput_endDate = Lens.lens (\InventoryRetrievalJobInput' {endDate} -> endDate) (\s@InventoryRetrievalJobInput' {} a -> s {endDate = a} :: InventoryRetrievalJobInput)

-- | Specifies the maximum number of inventory items returned per vault
-- inventory retrieval request. Valid values are greater than or equal to
-- 1.
inventoryRetrievalJobInput_limit :: Lens.Lens' InventoryRetrievalJobInput (Prelude.Maybe Prelude.Text)
inventoryRetrievalJobInput_limit = Lens.lens (\InventoryRetrievalJobInput' {limit} -> limit) (\s@InventoryRetrievalJobInput' {} a -> s {limit = a} :: InventoryRetrievalJobInput)

-- | The start of the date range in UTC for vault inventory retrieval that
-- includes archives created on or after this date. This value should be a
-- string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@.
inventoryRetrievalJobInput_startDate :: Lens.Lens' InventoryRetrievalJobInput (Prelude.Maybe Prelude.Text)
inventoryRetrievalJobInput_startDate = Lens.lens (\InventoryRetrievalJobInput' {startDate} -> startDate) (\s@InventoryRetrievalJobInput' {} a -> s {startDate = a} :: InventoryRetrievalJobInput)

instance Prelude.Hashable InventoryRetrievalJobInput where
  hashWithSalt _salt InventoryRetrievalJobInput' {..} =
    _salt `Prelude.hashWithSalt` marker
      `Prelude.hashWithSalt` endDate
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` startDate

instance Prelude.NFData InventoryRetrievalJobInput where
  rnf InventoryRetrievalJobInput' {..} =
    Prelude.rnf marker
      `Prelude.seq` Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf startDate

instance Data.ToJSON InventoryRetrievalJobInput where
  toJSON InventoryRetrievalJobInput' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Marker" Data..=) Prelude.<$> marker,
            ("EndDate" Data..=) Prelude.<$> endDate,
            ("Limit" Data..=) Prelude.<$> limit,
            ("StartDate" Data..=) Prelude.<$> startDate
          ]
      )
