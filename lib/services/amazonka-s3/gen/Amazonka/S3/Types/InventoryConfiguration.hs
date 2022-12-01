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
-- Module      : Amazonka.S3.Types.InventoryConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.InventoryConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.InventoryDestination
import Amazonka.S3.Types.InventoryFilter
import Amazonka.S3.Types.InventoryIncludedObjectVersions
import Amazonka.S3.Types.InventoryOptionalField
import Amazonka.S3.Types.InventorySchedule

-- | Specifies the inventory configuration for an Amazon S3 bucket. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketGETInventoryConfig.html GET Bucket inventory>
-- in the /Amazon S3 API Reference/.
--
-- /See:/ 'newInventoryConfiguration' smart constructor.
data InventoryConfiguration = InventoryConfiguration'
  { -- | Contains the optional fields that are included in the inventory results.
    optionalFields :: Prelude.Maybe [InventoryOptionalField],
    -- | Specifies an inventory filter. The inventory only includes objects that
    -- meet the filter\'s criteria.
    filter' :: Prelude.Maybe InventoryFilter,
    -- | Contains information about where to publish the inventory results.
    destination :: InventoryDestination,
    -- | Specifies whether the inventory is enabled or disabled. If set to
    -- @True@, an inventory list is generated. If set to @False@, no inventory
    -- list is generated.
    isEnabled :: Prelude.Bool,
    -- | The ID used to identify the inventory configuration.
    id :: Prelude.Text,
    -- | Object versions to include in the inventory list. If set to @All@, the
    -- list includes all the object versions, which adds the version-related
    -- fields @VersionId@, @IsLatest@, and @DeleteMarker@ to the list. If set
    -- to @Current@, the list does not contain these version-related fields.
    includedObjectVersions :: InventoryIncludedObjectVersions,
    -- | Specifies the schedule for generating inventory results.
    schedule :: InventorySchedule
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InventoryConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'optionalFields', 'inventoryConfiguration_optionalFields' - Contains the optional fields that are included in the inventory results.
--
-- 'filter'', 'inventoryConfiguration_filter' - Specifies an inventory filter. The inventory only includes objects that
-- meet the filter\'s criteria.
--
-- 'destination', 'inventoryConfiguration_destination' - Contains information about where to publish the inventory results.
--
-- 'isEnabled', 'inventoryConfiguration_isEnabled' - Specifies whether the inventory is enabled or disabled. If set to
-- @True@, an inventory list is generated. If set to @False@, no inventory
-- list is generated.
--
-- 'id', 'inventoryConfiguration_id' - The ID used to identify the inventory configuration.
--
-- 'includedObjectVersions', 'inventoryConfiguration_includedObjectVersions' - Object versions to include in the inventory list. If set to @All@, the
-- list includes all the object versions, which adds the version-related
-- fields @VersionId@, @IsLatest@, and @DeleteMarker@ to the list. If set
-- to @Current@, the list does not contain these version-related fields.
--
-- 'schedule', 'inventoryConfiguration_schedule' - Specifies the schedule for generating inventory results.
newInventoryConfiguration ::
  -- | 'destination'
  InventoryDestination ->
  -- | 'isEnabled'
  Prelude.Bool ->
  -- | 'id'
  Prelude.Text ->
  -- | 'includedObjectVersions'
  InventoryIncludedObjectVersions ->
  -- | 'schedule'
  InventorySchedule ->
  InventoryConfiguration
newInventoryConfiguration
  pDestination_
  pIsEnabled_
  pId_
  pIncludedObjectVersions_
  pSchedule_ =
    InventoryConfiguration'
      { optionalFields =
          Prelude.Nothing,
        filter' = Prelude.Nothing,
        destination = pDestination_,
        isEnabled = pIsEnabled_,
        id = pId_,
        includedObjectVersions = pIncludedObjectVersions_,
        schedule = pSchedule_
      }

-- | Contains the optional fields that are included in the inventory results.
inventoryConfiguration_optionalFields :: Lens.Lens' InventoryConfiguration (Prelude.Maybe [InventoryOptionalField])
inventoryConfiguration_optionalFields = Lens.lens (\InventoryConfiguration' {optionalFields} -> optionalFields) (\s@InventoryConfiguration' {} a -> s {optionalFields = a} :: InventoryConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Specifies an inventory filter. The inventory only includes objects that
-- meet the filter\'s criteria.
inventoryConfiguration_filter :: Lens.Lens' InventoryConfiguration (Prelude.Maybe InventoryFilter)
inventoryConfiguration_filter = Lens.lens (\InventoryConfiguration' {filter'} -> filter') (\s@InventoryConfiguration' {} a -> s {filter' = a} :: InventoryConfiguration)

-- | Contains information about where to publish the inventory results.
inventoryConfiguration_destination :: Lens.Lens' InventoryConfiguration InventoryDestination
inventoryConfiguration_destination = Lens.lens (\InventoryConfiguration' {destination} -> destination) (\s@InventoryConfiguration' {} a -> s {destination = a} :: InventoryConfiguration)

-- | Specifies whether the inventory is enabled or disabled. If set to
-- @True@, an inventory list is generated. If set to @False@, no inventory
-- list is generated.
inventoryConfiguration_isEnabled :: Lens.Lens' InventoryConfiguration Prelude.Bool
inventoryConfiguration_isEnabled = Lens.lens (\InventoryConfiguration' {isEnabled} -> isEnabled) (\s@InventoryConfiguration' {} a -> s {isEnabled = a} :: InventoryConfiguration)

-- | The ID used to identify the inventory configuration.
inventoryConfiguration_id :: Lens.Lens' InventoryConfiguration Prelude.Text
inventoryConfiguration_id = Lens.lens (\InventoryConfiguration' {id} -> id) (\s@InventoryConfiguration' {} a -> s {id = a} :: InventoryConfiguration)

-- | Object versions to include in the inventory list. If set to @All@, the
-- list includes all the object versions, which adds the version-related
-- fields @VersionId@, @IsLatest@, and @DeleteMarker@ to the list. If set
-- to @Current@, the list does not contain these version-related fields.
inventoryConfiguration_includedObjectVersions :: Lens.Lens' InventoryConfiguration InventoryIncludedObjectVersions
inventoryConfiguration_includedObjectVersions = Lens.lens (\InventoryConfiguration' {includedObjectVersions} -> includedObjectVersions) (\s@InventoryConfiguration' {} a -> s {includedObjectVersions = a} :: InventoryConfiguration)

-- | Specifies the schedule for generating inventory results.
inventoryConfiguration_schedule :: Lens.Lens' InventoryConfiguration InventorySchedule
inventoryConfiguration_schedule = Lens.lens (\InventoryConfiguration' {schedule} -> schedule) (\s@InventoryConfiguration' {} a -> s {schedule = a} :: InventoryConfiguration)

instance Core.FromXML InventoryConfiguration where
  parseXML x =
    InventoryConfiguration'
      Prelude.<$> ( x Core..@? "OptionalFields" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "Field")
                  )
      Prelude.<*> (x Core..@? "Filter")
      Prelude.<*> (x Core..@ "Destination")
      Prelude.<*> (x Core..@ "IsEnabled")
      Prelude.<*> (x Core..@ "Id")
      Prelude.<*> (x Core..@ "IncludedObjectVersions")
      Prelude.<*> (x Core..@ "Schedule")

instance Prelude.Hashable InventoryConfiguration where
  hashWithSalt _salt InventoryConfiguration' {..} =
    _salt `Prelude.hashWithSalt` optionalFields
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` destination
      `Prelude.hashWithSalt` isEnabled
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` includedObjectVersions
      `Prelude.hashWithSalt` schedule

instance Prelude.NFData InventoryConfiguration where
  rnf InventoryConfiguration' {..} =
    Prelude.rnf optionalFields
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf destination
      `Prelude.seq` Prelude.rnf isEnabled
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf includedObjectVersions
      `Prelude.seq` Prelude.rnf schedule

instance Core.ToXML InventoryConfiguration where
  toXML InventoryConfiguration' {..} =
    Prelude.mconcat
      [ "OptionalFields"
          Core.@= Core.toXML
            (Core.toXMLList "Field" Prelude.<$> optionalFields),
        "Filter" Core.@= filter',
        "Destination" Core.@= destination,
        "IsEnabled" Core.@= isEnabled,
        "Id" Core.@= id,
        "IncludedObjectVersions"
          Core.@= includedObjectVersions,
        "Schedule" Core.@= schedule
      ]
