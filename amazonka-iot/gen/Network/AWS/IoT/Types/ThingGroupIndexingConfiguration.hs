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
-- Module      : Network.AWS.IoT.Types.ThingGroupIndexingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingGroupIndexingConfiguration where

import qualified Network.AWS.Core as Core
import Network.AWS.IoT.Types.Field
import Network.AWS.IoT.Types.ThingGroupIndexingMode
import qualified Network.AWS.Lens as Lens

-- | Thing group indexing configuration.
--
-- /See:/ 'newThingGroupIndexingConfiguration' smart constructor.
data ThingGroupIndexingConfiguration = ThingGroupIndexingConfiguration'
  { -- | Contains fields that are indexed and whose types are already known by
    -- the Fleet Indexing service.
    managedFields :: Core.Maybe [Field],
    -- | A list of thing group fields to index. This list cannot contain any
    -- managed fields. Use the GetIndexingConfiguration API to get a list of
    -- managed fields.
    --
    -- Contains custom field names and their data type.
    customFields :: Core.Maybe [Field],
    -- | Thing group indexing mode.
    thingGroupIndexingMode :: ThingGroupIndexingMode
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ThingGroupIndexingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'managedFields', 'thingGroupIndexingConfiguration_managedFields' - Contains fields that are indexed and whose types are already known by
-- the Fleet Indexing service.
--
-- 'customFields', 'thingGroupIndexingConfiguration_customFields' - A list of thing group fields to index. This list cannot contain any
-- managed fields. Use the GetIndexingConfiguration API to get a list of
-- managed fields.
--
-- Contains custom field names and their data type.
--
-- 'thingGroupIndexingMode', 'thingGroupIndexingConfiguration_thingGroupIndexingMode' - Thing group indexing mode.
newThingGroupIndexingConfiguration ::
  -- | 'thingGroupIndexingMode'
  ThingGroupIndexingMode ->
  ThingGroupIndexingConfiguration
newThingGroupIndexingConfiguration
  pThingGroupIndexingMode_ =
    ThingGroupIndexingConfiguration'
      { managedFields =
          Core.Nothing,
        customFields = Core.Nothing,
        thingGroupIndexingMode =
          pThingGroupIndexingMode_
      }

-- | Contains fields that are indexed and whose types are already known by
-- the Fleet Indexing service.
thingGroupIndexingConfiguration_managedFields :: Lens.Lens' ThingGroupIndexingConfiguration (Core.Maybe [Field])
thingGroupIndexingConfiguration_managedFields = Lens.lens (\ThingGroupIndexingConfiguration' {managedFields} -> managedFields) (\s@ThingGroupIndexingConfiguration' {} a -> s {managedFields = a} :: ThingGroupIndexingConfiguration) Core.. Lens.mapping Lens._Coerce

-- | A list of thing group fields to index. This list cannot contain any
-- managed fields. Use the GetIndexingConfiguration API to get a list of
-- managed fields.
--
-- Contains custom field names and their data type.
thingGroupIndexingConfiguration_customFields :: Lens.Lens' ThingGroupIndexingConfiguration (Core.Maybe [Field])
thingGroupIndexingConfiguration_customFields = Lens.lens (\ThingGroupIndexingConfiguration' {customFields} -> customFields) (\s@ThingGroupIndexingConfiguration' {} a -> s {customFields = a} :: ThingGroupIndexingConfiguration) Core.. Lens.mapping Lens._Coerce

-- | Thing group indexing mode.
thingGroupIndexingConfiguration_thingGroupIndexingMode :: Lens.Lens' ThingGroupIndexingConfiguration ThingGroupIndexingMode
thingGroupIndexingConfiguration_thingGroupIndexingMode = Lens.lens (\ThingGroupIndexingConfiguration' {thingGroupIndexingMode} -> thingGroupIndexingMode) (\s@ThingGroupIndexingConfiguration' {} a -> s {thingGroupIndexingMode = a} :: ThingGroupIndexingConfiguration)

instance
  Core.FromJSON
    ThingGroupIndexingConfiguration
  where
  parseJSON =
    Core.withObject
      "ThingGroupIndexingConfiguration"
      ( \x ->
          ThingGroupIndexingConfiguration'
            Core.<$> (x Core..:? "managedFields" Core..!= Core.mempty)
            Core.<*> (x Core..:? "customFields" Core..!= Core.mempty)
            Core.<*> (x Core..: "thingGroupIndexingMode")
      )

instance
  Core.Hashable
    ThingGroupIndexingConfiguration

instance Core.NFData ThingGroupIndexingConfiguration

instance Core.ToJSON ThingGroupIndexingConfiguration where
  toJSON ThingGroupIndexingConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("managedFields" Core..=) Core.<$> managedFields,
            ("customFields" Core..=) Core.<$> customFields,
            Core.Just
              ( "thingGroupIndexingMode"
                  Core..= thingGroupIndexingMode
              )
          ]
      )
