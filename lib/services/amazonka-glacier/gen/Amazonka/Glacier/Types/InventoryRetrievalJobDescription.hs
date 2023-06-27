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
-- Module      : Amazonka.Glacier.Types.InventoryRetrievalJobDescription
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glacier.Types.InventoryRetrievalJobDescription where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the options for a range inventory retrieval job.
--
-- /See:/ 'newInventoryRetrievalJobDescription' smart constructor.
data InventoryRetrievalJobDescription = InventoryRetrievalJobDescription'
  { -- | The end of the date range in UTC for vault inventory retrieval that
    -- includes archives created before this date. This value should be a
    -- string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@.
    endDate :: Prelude.Maybe Prelude.Text,
    -- | The output format for the vault inventory list, which is set by the
    -- __InitiateJob__ request when initiating a job to retrieve a vault
    -- inventory. Valid values are @CSV@ and @JSON@.
    format :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of inventory items returned per vault inventory
    -- retrieval request. This limit is set when initiating the job with the a
    -- __InitiateJob__ request.
    limit :: Prelude.Maybe Prelude.Text,
    -- | An opaque string that represents where to continue pagination of the
    -- vault inventory retrieval results. You use the marker in a new
    -- __InitiateJob__ request to obtain additional inventory items. If there
    -- are no more inventory items, this value is @null@. For more information,
    -- see
    -- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html#api-initiate-job-post-vault-inventory-list-filtering Range Inventory Retrieval>.
    marker :: Prelude.Maybe Prelude.Text,
    -- | The start of the date range in Universal Coordinated Time (UTC) for
    -- vault inventory retrieval that includes archives created on or after
    -- this date. This value should be a string in the ISO 8601 date format,
    -- for example @2013-03-20T17:03:43Z@.
    startDate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InventoryRetrievalJobDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endDate', 'inventoryRetrievalJobDescription_endDate' - The end of the date range in UTC for vault inventory retrieval that
-- includes archives created before this date. This value should be a
-- string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@.
--
-- 'format', 'inventoryRetrievalJobDescription_format' - The output format for the vault inventory list, which is set by the
-- __InitiateJob__ request when initiating a job to retrieve a vault
-- inventory. Valid values are @CSV@ and @JSON@.
--
-- 'limit', 'inventoryRetrievalJobDescription_limit' - The maximum number of inventory items returned per vault inventory
-- retrieval request. This limit is set when initiating the job with the a
-- __InitiateJob__ request.
--
-- 'marker', 'inventoryRetrievalJobDescription_marker' - An opaque string that represents where to continue pagination of the
-- vault inventory retrieval results. You use the marker in a new
-- __InitiateJob__ request to obtain additional inventory items. If there
-- are no more inventory items, this value is @null@. For more information,
-- see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html#api-initiate-job-post-vault-inventory-list-filtering Range Inventory Retrieval>.
--
-- 'startDate', 'inventoryRetrievalJobDescription_startDate' - The start of the date range in Universal Coordinated Time (UTC) for
-- vault inventory retrieval that includes archives created on or after
-- this date. This value should be a string in the ISO 8601 date format,
-- for example @2013-03-20T17:03:43Z@.
newInventoryRetrievalJobDescription ::
  InventoryRetrievalJobDescription
newInventoryRetrievalJobDescription =
  InventoryRetrievalJobDescription'
    { endDate =
        Prelude.Nothing,
      format = Prelude.Nothing,
      limit = Prelude.Nothing,
      marker = Prelude.Nothing,
      startDate = Prelude.Nothing
    }

-- | The end of the date range in UTC for vault inventory retrieval that
-- includes archives created before this date. This value should be a
-- string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@.
inventoryRetrievalJobDescription_endDate :: Lens.Lens' InventoryRetrievalJobDescription (Prelude.Maybe Prelude.Text)
inventoryRetrievalJobDescription_endDate = Lens.lens (\InventoryRetrievalJobDescription' {endDate} -> endDate) (\s@InventoryRetrievalJobDescription' {} a -> s {endDate = a} :: InventoryRetrievalJobDescription)

-- | The output format for the vault inventory list, which is set by the
-- __InitiateJob__ request when initiating a job to retrieve a vault
-- inventory. Valid values are @CSV@ and @JSON@.
inventoryRetrievalJobDescription_format :: Lens.Lens' InventoryRetrievalJobDescription (Prelude.Maybe Prelude.Text)
inventoryRetrievalJobDescription_format = Lens.lens (\InventoryRetrievalJobDescription' {format} -> format) (\s@InventoryRetrievalJobDescription' {} a -> s {format = a} :: InventoryRetrievalJobDescription)

-- | The maximum number of inventory items returned per vault inventory
-- retrieval request. This limit is set when initiating the job with the a
-- __InitiateJob__ request.
inventoryRetrievalJobDescription_limit :: Lens.Lens' InventoryRetrievalJobDescription (Prelude.Maybe Prelude.Text)
inventoryRetrievalJobDescription_limit = Lens.lens (\InventoryRetrievalJobDescription' {limit} -> limit) (\s@InventoryRetrievalJobDescription' {} a -> s {limit = a} :: InventoryRetrievalJobDescription)

-- | An opaque string that represents where to continue pagination of the
-- vault inventory retrieval results. You use the marker in a new
-- __InitiateJob__ request to obtain additional inventory items. If there
-- are no more inventory items, this value is @null@. For more information,
-- see
-- <https://docs.aws.amazon.com/amazonglacier/latest/dev/api-initiate-job-post.html#api-initiate-job-post-vault-inventory-list-filtering Range Inventory Retrieval>.
inventoryRetrievalJobDescription_marker :: Lens.Lens' InventoryRetrievalJobDescription (Prelude.Maybe Prelude.Text)
inventoryRetrievalJobDescription_marker = Lens.lens (\InventoryRetrievalJobDescription' {marker} -> marker) (\s@InventoryRetrievalJobDescription' {} a -> s {marker = a} :: InventoryRetrievalJobDescription)

-- | The start of the date range in Universal Coordinated Time (UTC) for
-- vault inventory retrieval that includes archives created on or after
-- this date. This value should be a string in the ISO 8601 date format,
-- for example @2013-03-20T17:03:43Z@.
inventoryRetrievalJobDescription_startDate :: Lens.Lens' InventoryRetrievalJobDescription (Prelude.Maybe Prelude.Text)
inventoryRetrievalJobDescription_startDate = Lens.lens (\InventoryRetrievalJobDescription' {startDate} -> startDate) (\s@InventoryRetrievalJobDescription' {} a -> s {startDate = a} :: InventoryRetrievalJobDescription)

instance
  Data.FromJSON
    InventoryRetrievalJobDescription
  where
  parseJSON =
    Data.withObject
      "InventoryRetrievalJobDescription"
      ( \x ->
          InventoryRetrievalJobDescription'
            Prelude.<$> (x Data..:? "EndDate")
            Prelude.<*> (x Data..:? "Format")
            Prelude.<*> (x Data..:? "Limit")
            Prelude.<*> (x Data..:? "Marker")
            Prelude.<*> (x Data..:? "StartDate")
      )

instance
  Prelude.Hashable
    InventoryRetrievalJobDescription
  where
  hashWithSalt
    _salt
    InventoryRetrievalJobDescription' {..} =
      _salt
        `Prelude.hashWithSalt` endDate
        `Prelude.hashWithSalt` format
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` marker
        `Prelude.hashWithSalt` startDate

instance
  Prelude.NFData
    InventoryRetrievalJobDescription
  where
  rnf InventoryRetrievalJobDescription' {..} =
    Prelude.rnf endDate
      `Prelude.seq` Prelude.rnf format
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf marker
      `Prelude.seq` Prelude.rnf startDate
