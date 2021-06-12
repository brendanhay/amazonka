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
-- Module      : Network.AWS.IoT.Types.ThingIndexingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingIndexingConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.Field
import Network.AWS.IoT.Types.ThingConnectivityIndexingMode
import Network.AWS.IoT.Types.ThingIndexingMode
import qualified Network.AWS.Lens as Lens

-- | The thing indexing configuration. For more information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/managing-index.html Managing Thing Indexing>.
--
-- /See:/ 'newThingIndexingConfiguration' smart constructor.
data ThingIndexingConfiguration = ThingIndexingConfiguration'
  { -- | Contains fields that are indexed and whose types are already known by
    -- the Fleet Indexing service.
    managedFields :: Core.Maybe [Field],
    -- | Thing connectivity indexing mode. Valid values are:
    --
    -- -   STATUS – Your thing index contains connectivity status. To enable
    --     thing connectivity indexing, thingIndexMode must not be set to OFF.
    --
    -- -   OFF - Thing connectivity status indexing is disabled.
    thingConnectivityIndexingMode :: Core.Maybe ThingConnectivityIndexingMode,
    -- | Contains custom field names and their data type.
    customFields :: Core.Maybe [Field],
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
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ThingIndexingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedFields', 'thingIndexingConfiguration_managedFields' - Contains fields that are indexed and whose types are already known by
-- the Fleet Indexing service.
--
-- 'thingConnectivityIndexingMode', 'thingIndexingConfiguration_thingConnectivityIndexingMode' - Thing connectivity indexing mode. Valid values are:
--
-- -   STATUS – Your thing index contains connectivity status. To enable
--     thing connectivity indexing, thingIndexMode must not be set to OFF.
--
-- -   OFF - Thing connectivity status indexing is disabled.
--
-- 'customFields', 'thingIndexingConfiguration_customFields' - Contains custom field names and their data type.
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
    { managedFields =
        Core.Nothing,
      thingConnectivityIndexingMode = Core.Nothing,
      customFields = Core.Nothing,
      thingIndexingMode = pThingIndexingMode_
    }

-- | Contains fields that are indexed and whose types are already known by
-- the Fleet Indexing service.
thingIndexingConfiguration_managedFields :: Lens.Lens' ThingIndexingConfiguration (Core.Maybe [Field])
thingIndexingConfiguration_managedFields = Lens.lens (\ThingIndexingConfiguration' {managedFields} -> managedFields) (\s@ThingIndexingConfiguration' {} a -> s {managedFields = a} :: ThingIndexingConfiguration) Core.. Lens.mapping Lens._Coerce

-- | Thing connectivity indexing mode. Valid values are:
--
-- -   STATUS – Your thing index contains connectivity status. To enable
--     thing connectivity indexing, thingIndexMode must not be set to OFF.
--
-- -   OFF - Thing connectivity status indexing is disabled.
thingIndexingConfiguration_thingConnectivityIndexingMode :: Lens.Lens' ThingIndexingConfiguration (Core.Maybe ThingConnectivityIndexingMode)
thingIndexingConfiguration_thingConnectivityIndexingMode = Lens.lens (\ThingIndexingConfiguration' {thingConnectivityIndexingMode} -> thingConnectivityIndexingMode) (\s@ThingIndexingConfiguration' {} a -> s {thingConnectivityIndexingMode = a} :: ThingIndexingConfiguration)

-- | Contains custom field names and their data type.
thingIndexingConfiguration_customFields :: Lens.Lens' ThingIndexingConfiguration (Core.Maybe [Field])
thingIndexingConfiguration_customFields = Lens.lens (\ThingIndexingConfiguration' {customFields} -> customFields) (\s@ThingIndexingConfiguration' {} a -> s {customFields = a} :: ThingIndexingConfiguration) Core.. Lens.mapping Lens._Coerce

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

instance Core.FromJSON ThingIndexingConfiguration where
  parseJSON =
    Core.withObject
      "ThingIndexingConfiguration"
      ( \x ->
          ThingIndexingConfiguration'
            Core.<$> (x Core..:? "managedFields" Core..!= Core.mempty)
            Core.<*> (x Core..:? "thingConnectivityIndexingMode")
            Core.<*> (x Core..:? "customFields" Core..!= Core.mempty)
            Core.<*> (x Core..: "thingIndexingMode")
      )

instance Core.Hashable ThingIndexingConfiguration

instance Core.NFData ThingIndexingConfiguration

instance Core.ToJSON ThingIndexingConfiguration where
  toJSON ThingIndexingConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("managedFields" Core..=) Core.<$> managedFields,
            ("thingConnectivityIndexingMode" Core..=)
              Core.<$> thingConnectivityIndexingMode,
            ("customFields" Core..=) Core.<$> customFields,
            Core.Just
              ("thingIndexingMode" Core..= thingIndexingMode)
          ]
      )
