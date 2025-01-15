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
-- Module      : Amazonka.IoT.Types.ThingIndexingConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ThingIndexingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.DeviceDefenderIndexingMode
import Amazonka.IoT.Types.Field
import Amazonka.IoT.Types.IndexingFilter
import Amazonka.IoT.Types.NamedShadowIndexingMode
import Amazonka.IoT.Types.ThingConnectivityIndexingMode
import Amazonka.IoT.Types.ThingIndexingMode
import qualified Amazonka.Prelude as Prelude

-- | The thing indexing configuration. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/managing-index.html Managing Thing Indexing>.
--
-- /See:/ 'newThingIndexingConfiguration' smart constructor.
data ThingIndexingConfiguration = ThingIndexingConfiguration'
  { -- | Contains custom field names and their data type.
    customFields :: Prelude.Maybe [Field],
    -- | Device Defender indexing mode. Valid values are:
    --
    -- -   VIOLATIONS – Your thing index contains Device Defender violations.
    --     To enable Device Defender indexing, /deviceDefenderIndexingMode/
    --     must not be set to OFF.
    --
    -- -   OFF - Device Defender indexing is disabled.
    --
    -- For more information about Device Defender violations, see
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/device-defender-detect.html Device Defender Detect.>
    deviceDefenderIndexingMode :: Prelude.Maybe DeviceDefenderIndexingMode,
    -- | Provides additional filters for specific data sources. Named shadow is
    -- the only data source that currently supports and requires a filter. To
    -- add named shadows to your fleet indexing configuration, set
    -- @namedShadowIndexingMode@ to be @ON@ and specify your shadow names in
    -- @filter@.
    filter' :: Prelude.Maybe IndexingFilter,
    -- | Contains fields that are indexed and whose types are already known by
    -- the Fleet Indexing service.
    managedFields :: Prelude.Maybe [Field],
    -- | Named shadow indexing mode. Valid values are:
    --
    -- -   ON – Your thing index contains named shadow. To enable thing named
    --     shadow indexing, /namedShadowIndexingMode/ must not be set to OFF.
    --
    -- -   OFF - Named shadow indexing is disabled.
    --
    -- For more information about Shadows, see
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-device-shadows.html IoT Device Shadow service.>
    namedShadowIndexingMode :: Prelude.Maybe NamedShadowIndexingMode,
    -- | Thing connectivity indexing mode. Valid values are:
    --
    -- -   STATUS – Your thing index contains connectivity status. To enable
    --     thing connectivity indexing, /thingIndexMode/ must not be set to
    --     OFF.
    --
    -- -   OFF - Thing connectivity status indexing is disabled.
    thingConnectivityIndexingMode :: Prelude.Maybe ThingConnectivityIndexingMode,
    -- | Thing indexing mode. Valid values are:
    --
    -- -   REGISTRY – Your thing index contains registry data only.
    --
    -- -   REGISTRY_AND_SHADOW - Your thing index contains registry and shadow
    --     data.
    --
    -- -   OFF - Thing indexing is disabled.
    thingIndexingMode :: ThingIndexingMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThingIndexingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'customFields', 'thingIndexingConfiguration_customFields' - Contains custom field names and their data type.
--
-- 'deviceDefenderIndexingMode', 'thingIndexingConfiguration_deviceDefenderIndexingMode' - Device Defender indexing mode. Valid values are:
--
-- -   VIOLATIONS – Your thing index contains Device Defender violations.
--     To enable Device Defender indexing, /deviceDefenderIndexingMode/
--     must not be set to OFF.
--
-- -   OFF - Device Defender indexing is disabled.
--
-- For more information about Device Defender violations, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/device-defender-detect.html Device Defender Detect.>
--
-- 'filter'', 'thingIndexingConfiguration_filter' - Provides additional filters for specific data sources. Named shadow is
-- the only data source that currently supports and requires a filter. To
-- add named shadows to your fleet indexing configuration, set
-- @namedShadowIndexingMode@ to be @ON@ and specify your shadow names in
-- @filter@.
--
-- 'managedFields', 'thingIndexingConfiguration_managedFields' - Contains fields that are indexed and whose types are already known by
-- the Fleet Indexing service.
--
-- 'namedShadowIndexingMode', 'thingIndexingConfiguration_namedShadowIndexingMode' - Named shadow indexing mode. Valid values are:
--
-- -   ON – Your thing index contains named shadow. To enable thing named
--     shadow indexing, /namedShadowIndexingMode/ must not be set to OFF.
--
-- -   OFF - Named shadow indexing is disabled.
--
-- For more information about Shadows, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-device-shadows.html IoT Device Shadow service.>
--
-- 'thingConnectivityIndexingMode', 'thingIndexingConfiguration_thingConnectivityIndexingMode' - Thing connectivity indexing mode. Valid values are:
--
-- -   STATUS – Your thing index contains connectivity status. To enable
--     thing connectivity indexing, /thingIndexMode/ must not be set to
--     OFF.
--
-- -   OFF - Thing connectivity status indexing is disabled.
--
-- 'thingIndexingMode', 'thingIndexingConfiguration_thingIndexingMode' - Thing indexing mode. Valid values are:
--
-- -   REGISTRY – Your thing index contains registry data only.
--
-- -   REGISTRY_AND_SHADOW - Your thing index contains registry and shadow
--     data.
--
-- -   OFF - Thing indexing is disabled.
newThingIndexingConfiguration ::
  -- | 'thingIndexingMode'
  ThingIndexingMode ->
  ThingIndexingConfiguration
newThingIndexingConfiguration pThingIndexingMode_ =
  ThingIndexingConfiguration'
    { customFields =
        Prelude.Nothing,
      deviceDefenderIndexingMode = Prelude.Nothing,
      filter' = Prelude.Nothing,
      managedFields = Prelude.Nothing,
      namedShadowIndexingMode = Prelude.Nothing,
      thingConnectivityIndexingMode = Prelude.Nothing,
      thingIndexingMode = pThingIndexingMode_
    }

-- | Contains custom field names and their data type.
thingIndexingConfiguration_customFields :: Lens.Lens' ThingIndexingConfiguration (Prelude.Maybe [Field])
thingIndexingConfiguration_customFields = Lens.lens (\ThingIndexingConfiguration' {customFields} -> customFields) (\s@ThingIndexingConfiguration' {} a -> s {customFields = a} :: ThingIndexingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Device Defender indexing mode. Valid values are:
--
-- -   VIOLATIONS – Your thing index contains Device Defender violations.
--     To enable Device Defender indexing, /deviceDefenderIndexingMode/
--     must not be set to OFF.
--
-- -   OFF - Device Defender indexing is disabled.
--
-- For more information about Device Defender violations, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/device-defender-detect.html Device Defender Detect.>
thingIndexingConfiguration_deviceDefenderIndexingMode :: Lens.Lens' ThingIndexingConfiguration (Prelude.Maybe DeviceDefenderIndexingMode)
thingIndexingConfiguration_deviceDefenderIndexingMode = Lens.lens (\ThingIndexingConfiguration' {deviceDefenderIndexingMode} -> deviceDefenderIndexingMode) (\s@ThingIndexingConfiguration' {} a -> s {deviceDefenderIndexingMode = a} :: ThingIndexingConfiguration)

-- | Provides additional filters for specific data sources. Named shadow is
-- the only data source that currently supports and requires a filter. To
-- add named shadows to your fleet indexing configuration, set
-- @namedShadowIndexingMode@ to be @ON@ and specify your shadow names in
-- @filter@.
thingIndexingConfiguration_filter :: Lens.Lens' ThingIndexingConfiguration (Prelude.Maybe IndexingFilter)
thingIndexingConfiguration_filter = Lens.lens (\ThingIndexingConfiguration' {filter'} -> filter') (\s@ThingIndexingConfiguration' {} a -> s {filter' = a} :: ThingIndexingConfiguration)

-- | Contains fields that are indexed and whose types are already known by
-- the Fleet Indexing service.
thingIndexingConfiguration_managedFields :: Lens.Lens' ThingIndexingConfiguration (Prelude.Maybe [Field])
thingIndexingConfiguration_managedFields = Lens.lens (\ThingIndexingConfiguration' {managedFields} -> managedFields) (\s@ThingIndexingConfiguration' {} a -> s {managedFields = a} :: ThingIndexingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Named shadow indexing mode. Valid values are:
--
-- -   ON – Your thing index contains named shadow. To enable thing named
--     shadow indexing, /namedShadowIndexingMode/ must not be set to OFF.
--
-- -   OFF - Named shadow indexing is disabled.
--
-- For more information about Shadows, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/iot-device-shadows.html IoT Device Shadow service.>
thingIndexingConfiguration_namedShadowIndexingMode :: Lens.Lens' ThingIndexingConfiguration (Prelude.Maybe NamedShadowIndexingMode)
thingIndexingConfiguration_namedShadowIndexingMode = Lens.lens (\ThingIndexingConfiguration' {namedShadowIndexingMode} -> namedShadowIndexingMode) (\s@ThingIndexingConfiguration' {} a -> s {namedShadowIndexingMode = a} :: ThingIndexingConfiguration)

-- | Thing connectivity indexing mode. Valid values are:
--
-- -   STATUS – Your thing index contains connectivity status. To enable
--     thing connectivity indexing, /thingIndexMode/ must not be set to
--     OFF.
--
-- -   OFF - Thing connectivity status indexing is disabled.
thingIndexingConfiguration_thingConnectivityIndexingMode :: Lens.Lens' ThingIndexingConfiguration (Prelude.Maybe ThingConnectivityIndexingMode)
thingIndexingConfiguration_thingConnectivityIndexingMode = Lens.lens (\ThingIndexingConfiguration' {thingConnectivityIndexingMode} -> thingConnectivityIndexingMode) (\s@ThingIndexingConfiguration' {} a -> s {thingConnectivityIndexingMode = a} :: ThingIndexingConfiguration)

-- | Thing indexing mode. Valid values are:
--
-- -   REGISTRY – Your thing index contains registry data only.
--
-- -   REGISTRY_AND_SHADOW - Your thing index contains registry and shadow
--     data.
--
-- -   OFF - Thing indexing is disabled.
thingIndexingConfiguration_thingIndexingMode :: Lens.Lens' ThingIndexingConfiguration ThingIndexingMode
thingIndexingConfiguration_thingIndexingMode = Lens.lens (\ThingIndexingConfiguration' {thingIndexingMode} -> thingIndexingMode) (\s@ThingIndexingConfiguration' {} a -> s {thingIndexingMode = a} :: ThingIndexingConfiguration)

instance Data.FromJSON ThingIndexingConfiguration where
  parseJSON =
    Data.withObject
      "ThingIndexingConfiguration"
      ( \x ->
          ThingIndexingConfiguration'
            Prelude.<$> (x Data..:? "customFields" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "deviceDefenderIndexingMode")
            Prelude.<*> (x Data..:? "filter")
            Prelude.<*> (x Data..:? "managedFields" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "namedShadowIndexingMode")
            Prelude.<*> (x Data..:? "thingConnectivityIndexingMode")
            Prelude.<*> (x Data..: "thingIndexingMode")
      )

instance Prelude.Hashable ThingIndexingConfiguration where
  hashWithSalt _salt ThingIndexingConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` customFields
      `Prelude.hashWithSalt` deviceDefenderIndexingMode
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` managedFields
      `Prelude.hashWithSalt` namedShadowIndexingMode
      `Prelude.hashWithSalt` thingConnectivityIndexingMode
      `Prelude.hashWithSalt` thingIndexingMode

instance Prelude.NFData ThingIndexingConfiguration where
  rnf ThingIndexingConfiguration' {..} =
    Prelude.rnf customFields `Prelude.seq`
      Prelude.rnf deviceDefenderIndexingMode `Prelude.seq`
        Prelude.rnf filter' `Prelude.seq`
          Prelude.rnf managedFields `Prelude.seq`
            Prelude.rnf namedShadowIndexingMode `Prelude.seq`
              Prelude.rnf thingConnectivityIndexingMode `Prelude.seq`
                Prelude.rnf thingIndexingMode

instance Data.ToJSON ThingIndexingConfiguration where
  toJSON ThingIndexingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("customFields" Data..=) Prelude.<$> customFields,
            ("deviceDefenderIndexingMode" Data..=)
              Prelude.<$> deviceDefenderIndexingMode,
            ("filter" Data..=) Prelude.<$> filter',
            ("managedFields" Data..=) Prelude.<$> managedFields,
            ("namedShadowIndexingMode" Data..=)
              Prelude.<$> namedShadowIndexingMode,
            ("thingConnectivityIndexingMode" Data..=)
              Prelude.<$> thingConnectivityIndexingMode,
            Prelude.Just
              ("thingIndexingMode" Data..= thingIndexingMode)
          ]
      )
