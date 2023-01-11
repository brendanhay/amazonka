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
-- Module      : Amazonka.IoT.Types.ThingTypeProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ThingTypeProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
thingTypeProperties_searchableAttributes = Lens.lens (\ThingTypeProperties' {searchableAttributes} -> searchableAttributes) (\s@ThingTypeProperties' {} a -> s {searchableAttributes = a} :: ThingTypeProperties) Prelude.. Lens.mapping Lens.coerced

-- | The description of the thing type.
thingTypeProperties_thingTypeDescription :: Lens.Lens' ThingTypeProperties (Prelude.Maybe Prelude.Text)
thingTypeProperties_thingTypeDescription = Lens.lens (\ThingTypeProperties' {thingTypeDescription} -> thingTypeDescription) (\s@ThingTypeProperties' {} a -> s {thingTypeDescription = a} :: ThingTypeProperties)

instance Data.FromJSON ThingTypeProperties where
  parseJSON =
    Data.withObject
      "ThingTypeProperties"
      ( \x ->
          ThingTypeProperties'
            Prelude.<$> ( x Data..:? "searchableAttributes"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "thingTypeDescription")
      )

instance Prelude.Hashable ThingTypeProperties where
  hashWithSalt _salt ThingTypeProperties' {..} =
    _salt `Prelude.hashWithSalt` searchableAttributes
      `Prelude.hashWithSalt` thingTypeDescription

instance Prelude.NFData ThingTypeProperties where
  rnf ThingTypeProperties' {..} =
    Prelude.rnf searchableAttributes
      `Prelude.seq` Prelude.rnf thingTypeDescription

instance Data.ToJSON ThingTypeProperties where
  toJSON ThingTypeProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("searchableAttributes" Data..=)
              Prelude.<$> searchableAttributes,
            ("thingTypeDescription" Data..=)
              Prelude.<$> thingTypeDescription
          ]
      )
