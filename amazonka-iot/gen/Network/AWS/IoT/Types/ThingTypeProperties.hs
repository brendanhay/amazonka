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
-- Module      : Network.AWS.IoT.Types.ThingTypeProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingTypeProperties where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The ThingTypeProperties contains information about the thing type
-- including: a thing type description, and a list of searchable thing
-- attribute names.
--
-- /See:/ 'newThingTypeProperties' smart constructor.
data ThingTypeProperties = ThingTypeProperties'
  { -- | A list of searchable thing attribute names.
    searchableAttributes :: Prelude.Maybe [Prelude.Text],
    -- | The description of the thing type.
    thingTypeDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      thingTypeDescription = Prelude.Nothing
    }

-- | A list of searchable thing attribute names.
thingTypeProperties_searchableAttributes :: Lens.Lens' ThingTypeProperties (Prelude.Maybe [Prelude.Text])
thingTypeProperties_searchableAttributes = Lens.lens (\ThingTypeProperties' {searchableAttributes} -> searchableAttributes) (\s@ThingTypeProperties' {} a -> s {searchableAttributes = a} :: ThingTypeProperties) Prelude.. Lens.mapping Prelude._Coerce

-- | The description of the thing type.
thingTypeProperties_thingTypeDescription :: Lens.Lens' ThingTypeProperties (Prelude.Maybe Prelude.Text)
thingTypeProperties_thingTypeDescription = Lens.lens (\ThingTypeProperties' {thingTypeDescription} -> thingTypeDescription) (\s@ThingTypeProperties' {} a -> s {thingTypeDescription = a} :: ThingTypeProperties)

instance Prelude.FromJSON ThingTypeProperties where
  parseJSON =
    Prelude.withObject
      "ThingTypeProperties"
      ( \x ->
          ThingTypeProperties'
            Prelude.<$> ( x Prelude..:? "searchableAttributes"
                            Prelude..!= Prelude.mempty
                        )
            Prelude.<*> (x Prelude..:? "thingTypeDescription")
      )

instance Prelude.Hashable ThingTypeProperties

instance Prelude.NFData ThingTypeProperties

instance Prelude.ToJSON ThingTypeProperties where
  toJSON ThingTypeProperties' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("searchableAttributes" Prelude..=)
              Prelude.<$> searchableAttributes,
            ("thingTypeDescription" Prelude..=)
              Prelude.<$> thingTypeDescription
          ]
      )
