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
-- Module      : Amazonka.IoT.Types.ThingGroupProperties
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ThingGroupProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.AttributePayload
import qualified Amazonka.Prelude as Prelude

-- | Thing group properties.
--
-- /See:/ 'newThingGroupProperties' smart constructor.
data ThingGroupProperties = ThingGroupProperties'
  { -- | The thing group attributes in JSON format.
    attributePayload :: Prelude.Maybe AttributePayload,
    -- | The thing group description.
    thingGroupDescription :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThingGroupProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributePayload', 'thingGroupProperties_attributePayload' - The thing group attributes in JSON format.
--
-- 'thingGroupDescription', 'thingGroupProperties_thingGroupDescription' - The thing group description.
newThingGroupProperties ::
  ThingGroupProperties
newThingGroupProperties =
  ThingGroupProperties'
    { attributePayload =
        Prelude.Nothing,
      thingGroupDescription = Prelude.Nothing
    }

-- | The thing group attributes in JSON format.
thingGroupProperties_attributePayload :: Lens.Lens' ThingGroupProperties (Prelude.Maybe AttributePayload)
thingGroupProperties_attributePayload = Lens.lens (\ThingGroupProperties' {attributePayload} -> attributePayload) (\s@ThingGroupProperties' {} a -> s {attributePayload = a} :: ThingGroupProperties)

-- | The thing group description.
thingGroupProperties_thingGroupDescription :: Lens.Lens' ThingGroupProperties (Prelude.Maybe Prelude.Text)
thingGroupProperties_thingGroupDescription = Lens.lens (\ThingGroupProperties' {thingGroupDescription} -> thingGroupDescription) (\s@ThingGroupProperties' {} a -> s {thingGroupDescription = a} :: ThingGroupProperties)

instance Data.FromJSON ThingGroupProperties where
  parseJSON =
    Data.withObject
      "ThingGroupProperties"
      ( \x ->
          ThingGroupProperties'
            Prelude.<$> (x Data..:? "attributePayload")
            Prelude.<*> (x Data..:? "thingGroupDescription")
      )

instance Prelude.Hashable ThingGroupProperties where
  hashWithSalt _salt ThingGroupProperties' {..} =
    _salt
      `Prelude.hashWithSalt` attributePayload
      `Prelude.hashWithSalt` thingGroupDescription

instance Prelude.NFData ThingGroupProperties where
  rnf ThingGroupProperties' {..} =
    Prelude.rnf attributePayload
      `Prelude.seq` Prelude.rnf thingGroupDescription

instance Data.ToJSON ThingGroupProperties where
  toJSON ThingGroupProperties' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("attributePayload" Data..=)
              Prelude.<$> attributePayload,
            ("thingGroupDescription" Data..=)
              Prelude.<$> thingGroupDescription
          ]
      )
