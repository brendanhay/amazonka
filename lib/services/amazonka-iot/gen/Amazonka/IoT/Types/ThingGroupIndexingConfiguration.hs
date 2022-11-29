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
-- Module      : Amazonka.IoT.Types.ThingGroupIndexingConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ThingGroupIndexingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.Field
import Amazonka.IoT.Types.ThingGroupIndexingMode
import qualified Amazonka.Prelude as Prelude

-- | Thing group indexing configuration.
--
-- /See:/ 'newThingGroupIndexingConfiguration' smart constructor.
data ThingGroupIndexingConfiguration = ThingGroupIndexingConfiguration'
  { -- | Contains fields that are indexed and whose types are already known by
    -- the Fleet Indexing service.
    managedFields :: Prelude.Maybe [Field],
    -- | A list of thing group fields to index. This list cannot contain any
    -- managed fields. Use the GetIndexingConfiguration API to get a list of
    -- managed fields.
    --
    -- Contains custom field names and their data type.
    customFields :: Prelude.Maybe [Field],
    -- | Thing group indexing mode.
    thingGroupIndexingMode :: ThingGroupIndexingMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
          Prelude.Nothing,
        customFields = Prelude.Nothing,
        thingGroupIndexingMode =
          pThingGroupIndexingMode_
      }

-- | Contains fields that are indexed and whose types are already known by
-- the Fleet Indexing service.
thingGroupIndexingConfiguration_managedFields :: Lens.Lens' ThingGroupIndexingConfiguration (Prelude.Maybe [Field])
thingGroupIndexingConfiguration_managedFields = Lens.lens (\ThingGroupIndexingConfiguration' {managedFields} -> managedFields) (\s@ThingGroupIndexingConfiguration' {} a -> s {managedFields = a} :: ThingGroupIndexingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A list of thing group fields to index. This list cannot contain any
-- managed fields. Use the GetIndexingConfiguration API to get a list of
-- managed fields.
--
-- Contains custom field names and their data type.
thingGroupIndexingConfiguration_customFields :: Lens.Lens' ThingGroupIndexingConfiguration (Prelude.Maybe [Field])
thingGroupIndexingConfiguration_customFields = Lens.lens (\ThingGroupIndexingConfiguration' {customFields} -> customFields) (\s@ThingGroupIndexingConfiguration' {} a -> s {customFields = a} :: ThingGroupIndexingConfiguration) Prelude.. Lens.mapping Lens.coerced

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
            Prelude.<$> (x Core..:? "managedFields" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "customFields" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..: "thingGroupIndexingMode")
      )

instance
  Prelude.Hashable
    ThingGroupIndexingConfiguration
  where
  hashWithSalt
    _salt
    ThingGroupIndexingConfiguration' {..} =
      _salt `Prelude.hashWithSalt` managedFields
        `Prelude.hashWithSalt` customFields
        `Prelude.hashWithSalt` thingGroupIndexingMode

instance
  Prelude.NFData
    ThingGroupIndexingConfiguration
  where
  rnf ThingGroupIndexingConfiguration' {..} =
    Prelude.rnf managedFields
      `Prelude.seq` Prelude.rnf customFields
      `Prelude.seq` Prelude.rnf thingGroupIndexingMode

instance Core.ToJSON ThingGroupIndexingConfiguration where
  toJSON ThingGroupIndexingConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("managedFields" Core..=) Prelude.<$> managedFields,
            ("customFields" Core..=) Prelude.<$> customFields,
            Prelude.Just
              ( "thingGroupIndexingMode"
                  Core..= thingGroupIndexingMode
              )
          ]
      )
