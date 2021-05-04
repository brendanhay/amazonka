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
-- Module      : Network.AWS.IoT.Types.ThingGroupIndexingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingGroupIndexingConfiguration where

import Network.AWS.IoT.Types.Field
import Network.AWS.IoT.Types.ThingGroupIndexingMode
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
thingGroupIndexingConfiguration_managedFields = Lens.lens (\ThingGroupIndexingConfiguration' {managedFields} -> managedFields) (\s@ThingGroupIndexingConfiguration' {} a -> s {managedFields = a} :: ThingGroupIndexingConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | A list of thing group fields to index. This list cannot contain any
-- managed fields. Use the GetIndexingConfiguration API to get a list of
-- managed fields.
--
-- Contains custom field names and their data type.
thingGroupIndexingConfiguration_customFields :: Lens.Lens' ThingGroupIndexingConfiguration (Prelude.Maybe [Field])
thingGroupIndexingConfiguration_customFields = Lens.lens (\ThingGroupIndexingConfiguration' {customFields} -> customFields) (\s@ThingGroupIndexingConfiguration' {} a -> s {customFields = a} :: ThingGroupIndexingConfiguration) Prelude.. Lens.mapping Prelude._Coerce

-- | Thing group indexing mode.
thingGroupIndexingConfiguration_thingGroupIndexingMode :: Lens.Lens' ThingGroupIndexingConfiguration ThingGroupIndexingMode
thingGroupIndexingConfiguration_thingGroupIndexingMode = Lens.lens (\ThingGroupIndexingConfiguration' {thingGroupIndexingMode} -> thingGroupIndexingMode) (\s@ThingGroupIndexingConfiguration' {} a -> s {thingGroupIndexingMode = a} :: ThingGroupIndexingConfiguration)

instance
  Prelude.FromJSON
    ThingGroupIndexingConfiguration
  where
  parseJSON =
    Prelude.withObject
      "ThingGroupIndexingConfiguration"
      ( \x ->
          ThingGroupIndexingConfiguration'
            Prelude.<$> ( x Prelude..:? "managedFields"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> ( x Prelude..:? "customFields"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..: "thingGroupIndexingMode")
      )

instance
  Prelude.Hashable
    ThingGroupIndexingConfiguration

instance
  Prelude.NFData
    ThingGroupIndexingConfiguration

instance
  Prelude.ToJSON
    ThingGroupIndexingConfiguration
  where
  toJSON ThingGroupIndexingConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("managedFields" Prelude..=)
              Prelude.<$> managedFields,
            ("customFields" Prelude..=) Prelude.<$> customFields,
            Prelude.Just
              ( "thingGroupIndexingMode"
                  Prelude..= thingGroupIndexingMode
              )
          ]
      )
