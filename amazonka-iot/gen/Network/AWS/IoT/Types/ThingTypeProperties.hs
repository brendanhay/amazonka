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
-- Module      : Network.AWS.IoT.Types.ThingTypeProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingTypeProperties where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The ThingTypeProperties contains information about the thing type
-- including: a thing type description, and a list of searchable thing
-- attribute names.
--
-- /See:/ 'newThingTypeProperties' smart constructor.
data ThingTypeProperties = ThingTypeProperties'
  { -- | A list of searchable thing attribute names.
    searchableAttributes :: Core.Maybe [Core.Text],
    -- | The description of the thing type.
    thingTypeDescription :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ThingTypeProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'searchableAttributes', 'thingTypeProperties_searchableAttributes' - A list of searchable thing attribute names.
--
-- 'thingTypeDescription', 'thingTypeProperties_thingTypeDescription' - The description of the thing type.
newThingTypeProperties ::
  ThingTypeProperties
newThingTypeProperties =
  ThingTypeProperties'
    { searchableAttributes =
        Core.Nothing,
      thingTypeDescription = Core.Nothing
    }

-- | A list of searchable thing attribute names.
thingTypeProperties_searchableAttributes :: Lens.Lens' ThingTypeProperties (Core.Maybe [Core.Text])
thingTypeProperties_searchableAttributes = Lens.lens (\ThingTypeProperties' {searchableAttributes} -> searchableAttributes) (\s@ThingTypeProperties' {} a -> s {searchableAttributes = a} :: ThingTypeProperties) Core.. Lens.mapping Lens._Coerce

-- | The description of the thing type.
thingTypeProperties_thingTypeDescription :: Lens.Lens' ThingTypeProperties (Core.Maybe Core.Text)
thingTypeProperties_thingTypeDescription = Lens.lens (\ThingTypeProperties' {thingTypeDescription} -> thingTypeDescription) (\s@ThingTypeProperties' {} a -> s {thingTypeDescription = a} :: ThingTypeProperties)

instance Core.FromJSON ThingTypeProperties where
  parseJSON =
    Core.withObject
      "ThingTypeProperties"
      ( \x ->
          ThingTypeProperties'
            Core.<$> ( x Core..:? "searchableAttributes"
                         Core..!= Core.mempty
                     )
            Core.<*> (x Core..:? "thingTypeDescription")
      )

instance Core.Hashable ThingTypeProperties

instance Core.NFData ThingTypeProperties

instance Core.ToJSON ThingTypeProperties where
  toJSON ThingTypeProperties' {..} =
    Core.object
      ( Core.catMaybes
          [ ("searchableAttributes" Core..=)
              Core.<$> searchableAttributes,
            ("thingTypeDescription" Core..=)
              Core.<$> thingTypeDescription
          ]
      )
