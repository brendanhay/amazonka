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
-- Module      : Network.AWS.Glacier.Types.InventoryRetrievalJobDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glacier.Types.InventoryRetrievalJobDescription where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the options for a range inventory retrieval job.
--
-- /See:/ 'newInventoryRetrievalJobDescription' smart constructor.
data InventoryRetrievalJobDescription = InventoryRetrievalJobDescription'
  { -- | The start of the date range in Universal Coordinated Time (UTC) for
    -- vault inventory retrieval that includes archives created on or after
    -- this date. This value should be a string in the ISO 8601 date format,
    -- for example @2013-03-20T17:03:43Z@.
    startDate :: Prelude.Maybe Prelude.Text,
    -- | The output format for the vault inventory list, which is set by the
    -- __InitiateJob__ request when initiating a job to retrieve a vault
    -- inventory. Valid values are @CSV@ and @JSON@.
    format :: Prelude.Maybe Prelude.Text,
    -- | The end of the date range in UTC for vault inventory retrieval that
    -- includes archives created before this date. This value should be a
    -- string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@.
    endDate :: Prelude.Maybe Prelude.Text,
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
    marker :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InventoryRetrievalJobDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'startDate', 'inventoryRetrievalJobDescription_startDate' - The start of the date range in Universal Coordinated Time (UTC) for
-- vault inventory retrieval that includes archives created on or after
-- this date. This value should be a string in the ISO 8601 date format,
-- for example @2013-03-20T17:03:43Z@.
--
-- 'format', 'inventoryRetrievalJobDescription_format' - The output format for the vault inventory list, which is set by the
-- __InitiateJob__ request when initiating a job to retrieve a vault
-- inventory. Valid values are @CSV@ and @JSON@.
--
-- 'endDate', 'inventoryRetrievalJobDescription_endDate' - The end of the date range in UTC for vault inventory retrieval that
-- includes archives created before this date. This value should be a
-- string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@.
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
newInventoryRetrievalJobDescription ::
  InventoryRetrievalJobDescription
newInventoryRetrievalJobDescription =
  InventoryRetrievalJobDescription'
    { startDate =
        Prelude.Nothing,
      format = Prelude.Nothing,
      endDate = Prelude.Nothing,
      limit = Prelude.Nothing,
      marker = Prelude.Nothing
    }

-- | The start of the date range in Universal Coordinated Time (UTC) for
-- vault inventory retrieval that includes archives created on or after
-- this date. This value should be a string in the ISO 8601 date format,
-- for example @2013-03-20T17:03:43Z@.
inventoryRetrievalJobDescription_startDate :: Lens.Lens' InventoryRetrievalJobDescription (Prelude.Maybe Prelude.Text)
inventoryRetrievalJobDescription_startDate = Lens.lens (\InventoryRetrievalJobDescription' {startDate} -> startDate) (\s@InventoryRetrievalJobDescription' {} a -> s {startDate = a} :: InventoryRetrievalJobDescription)

-- | The output format for the vault inventory list, which is set by the
-- __InitiateJob__ request when initiating a job to retrieve a vault
-- inventory. Valid values are @CSV@ and @JSON@.
inventoryRetrievalJobDescription_format :: Lens.Lens' InventoryRetrievalJobDescription (Prelude.Maybe Prelude.Text)
inventoryRetrievalJobDescription_format = Lens.lens (\InventoryRetrievalJobDescription' {format} -> format) (\s@InventoryRetrievalJobDescription' {} a -> s {format = a} :: InventoryRetrievalJobDescription)

-- | The end of the date range in UTC for vault inventory retrieval that
-- includes archives created before this date. This value should be a
-- string in the ISO 8601 date format, for example @2013-03-20T17:03:43Z@.
inventoryRetrievalJobDescription_endDate :: Lens.Lens' InventoryRetrievalJobDescription (Prelude.Maybe Prelude.Text)
inventoryRetrievalJobDescription_endDate = Lens.lens (\InventoryRetrievalJobDescription' {endDate} -> endDate) (\s@InventoryRetrievalJobDescription' {} a -> s {endDate = a} :: InventoryRetrievalJobDescription)

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

instance
  Prelude.FromJSON
    InventoryRetrievalJobDescription
  where
  parseJSON =
    Prelude.withObject
      "InventoryRetrievalJobDescription"
      ( \x ->
          InventoryRetrievalJobDescription'
            Prelude.<$> (x Prelude..:? "StartDate")
            Prelude.<*> (x Prelude..:? "Format")
            Prelude.<*> (x Prelude..:? "EndDate")
            Prelude.<*> (x Prelude..:? "Limit")
            Prelude.<*> (x Prelude..:? "Marker")
      )

instance
  Prelude.Hashable
    InventoryRetrievalJobDescription

instance
  Prelude.NFData
    InventoryRetrievalJobDescription
