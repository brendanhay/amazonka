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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ThingGroupIndexingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.Field
import Amazonka.IoT.Types.ThingGroupIndexingMode
import qualified Amazonka.Prelude as Prelude

-- | Thing group indexing configuration.
--
-- /See:/ 'newThingGroupIndexingConfiguration' smart constructor.
data ThingGroupIndexingConfiguration = ThingGroupIndexingConfiguration'
  { -- | A list of thing group fields to index. This list cannot contain any
    -- managed fields. Use the GetIndexingConfiguration API to get a list of
    -- managed fields.
    --
    -- Contains custom field names and their data type.
    customFields :: Prelude.Maybe [Field],
    -- | Contains fields that are indexed and whose types are already known by
    -- the Fleet Indexing service. This is an optional field. For more
    -- information, see
    -- <https://docs.aws.amazon.com/iot/latest/developerguide/managing-fleet-index.html#managed-field Managed fields>
    -- in the /Amazon Web Services IoT Core Developer Guide/.
    managedFields :: Prelude.Maybe [Field],
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
-- 'customFields', 'thingGroupIndexingConfiguration_customFields' - A list of thing group fields to index. This list cannot contain any
-- managed fields. Use the GetIndexingConfiguration API to get a list of
-- managed fields.
--
-- Contains custom field names and their data type.
--
-- 'managedFields', 'thingGroupIndexingConfiguration_managedFields' - Contains fields that are indexed and whose types are already known by
-- the Fleet Indexing service. This is an optional field. For more
-- information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/managing-fleet-index.html#managed-field Managed fields>
-- in the /Amazon Web Services IoT Core Developer Guide/.
--
-- 'thingGroupIndexingMode', 'thingGroupIndexingConfiguration_thingGroupIndexingMode' - Thing group indexing mode.
newThingGroupIndexingConfiguration ::
  -- | 'thingGroupIndexingMode'
  ThingGroupIndexingMode ->
  ThingGroupIndexingConfiguration
newThingGroupIndexingConfiguration
  pThingGroupIndexingMode_ =
    ThingGroupIndexingConfiguration'
      { customFields =
          Prelude.Nothing,
        managedFields = Prelude.Nothing,
        thingGroupIndexingMode =
          pThingGroupIndexingMode_
      }

-- | A list of thing group fields to index. This list cannot contain any
-- managed fields. Use the GetIndexingConfiguration API to get a list of
-- managed fields.
--
-- Contains custom field names and their data type.
thingGroupIndexingConfiguration_customFields :: Lens.Lens' ThingGroupIndexingConfiguration (Prelude.Maybe [Field])
thingGroupIndexingConfiguration_customFields = Lens.lens (\ThingGroupIndexingConfiguration' {customFields} -> customFields) (\s@ThingGroupIndexingConfiguration' {} a -> s {customFields = a} :: ThingGroupIndexingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Contains fields that are indexed and whose types are already known by
-- the Fleet Indexing service. This is an optional field. For more
-- information, see
-- <https://docs.aws.amazon.com/iot/latest/developerguide/managing-fleet-index.html#managed-field Managed fields>
-- in the /Amazon Web Services IoT Core Developer Guide/.
thingGroupIndexingConfiguration_managedFields :: Lens.Lens' ThingGroupIndexingConfiguration (Prelude.Maybe [Field])
thingGroupIndexingConfiguration_managedFields = Lens.lens (\ThingGroupIndexingConfiguration' {managedFields} -> managedFields) (\s@ThingGroupIndexingConfiguration' {} a -> s {managedFields = a} :: ThingGroupIndexingConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | Thing group indexing mode.
thingGroupIndexingConfiguration_thingGroupIndexingMode :: Lens.Lens' ThingGroupIndexingConfiguration ThingGroupIndexingMode
thingGroupIndexingConfiguration_thingGroupIndexingMode = Lens.lens (\ThingGroupIndexingConfiguration' {thingGroupIndexingMode} -> thingGroupIndexingMode) (\s@ThingGroupIndexingConfiguration' {} a -> s {thingGroupIndexingMode = a} :: ThingGroupIndexingConfiguration)

instance
  Data.FromJSON
    ThingGroupIndexingConfiguration
  where
  parseJSON =
    Data.withObject
      "ThingGroupIndexingConfiguration"
      ( \x ->
          ThingGroupIndexingConfiguration'
            Prelude.<$> (x Data..:? "customFields" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "managedFields" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "thingGroupIndexingMode")
      )

instance
  Prelude.Hashable
    ThingGroupIndexingConfiguration
  where
  hashWithSalt
    _salt
    ThingGroupIndexingConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` customFields
        `Prelude.hashWithSalt` managedFields
        `Prelude.hashWithSalt` thingGroupIndexingMode

instance
  Prelude.NFData
    ThingGroupIndexingConfiguration
  where
  rnf ThingGroupIndexingConfiguration' {..} =
    Prelude.rnf customFields
      `Prelude.seq` Prelude.rnf managedFields
      `Prelude.seq` Prelude.rnf thingGroupIndexingMode

instance Data.ToJSON ThingGroupIndexingConfiguration where
  toJSON ThingGroupIndexingConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("customFields" Data..=) Prelude.<$> customFields,
            ("managedFields" Data..=) Prelude.<$> managedFields,
            Prelude.Just
              ( "thingGroupIndexingMode"
                  Data..= thingGroupIndexingMode
              )
          ]
      )
