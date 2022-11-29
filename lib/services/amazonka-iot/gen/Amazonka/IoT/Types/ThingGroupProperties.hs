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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ThingGroupProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoT.Types.AttributePayload
import qualified Amazonka.Prelude as Prelude

-- | Thing group properties.
--
-- /See:/ 'newThingGroupProperties' smart constructor.
data ThingGroupProperties = ThingGroupProperties'
  { -- | The thing group description.
    thingGroupDescription :: Prelude.Maybe Prelude.Text,
    -- | The thing group attributes in JSON format.
    attributePayload :: Prelude.Maybe AttributePayload
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
-- 'thingGroupDescription', 'thingGroupProperties_thingGroupDescription' - The thing group description.
--
-- 'attributePayload', 'thingGroupProperties_attributePayload' - The thing group attributes in JSON format.
newThingGroupProperties ::
  ThingGroupProperties
newThingGroupProperties =
  ThingGroupProperties'
    { thingGroupDescription =
        Prelude.Nothing,
      attributePayload = Prelude.Nothing
    }

-- | The thing group description.
thingGroupProperties_thingGroupDescription :: Lens.Lens' ThingGroupProperties (Prelude.Maybe Prelude.Text)
thingGroupProperties_thingGroupDescription = Lens.lens (\ThingGroupProperties' {thingGroupDescription} -> thingGroupDescription) (\s@ThingGroupProperties' {} a -> s {thingGroupDescription = a} :: ThingGroupProperties)

-- | The thing group attributes in JSON format.
thingGroupProperties_attributePayload :: Lens.Lens' ThingGroupProperties (Prelude.Maybe AttributePayload)
thingGroupProperties_attributePayload = Lens.lens (\ThingGroupProperties' {attributePayload} -> attributePayload) (\s@ThingGroupProperties' {} a -> s {attributePayload = a} :: ThingGroupProperties)

instance Core.FromJSON ThingGroupProperties where
  parseJSON =
    Core.withObject
      "ThingGroupProperties"
      ( \x ->
          ThingGroupProperties'
            Prelude.<$> (x Core..:? "thingGroupDescription")
            Prelude.<*> (x Core..:? "attributePayload")
      )

instance Prelude.Hashable ThingGroupProperties where
  hashWithSalt _salt ThingGroupProperties' {..} =
    _salt `Prelude.hashWithSalt` thingGroupDescription
      `Prelude.hashWithSalt` attributePayload

instance Prelude.NFData ThingGroupProperties where
  rnf ThingGroupProperties' {..} =
    Prelude.rnf thingGroupDescription
      `Prelude.seq` Prelude.rnf attributePayload

instance Core.ToJSON ThingGroupProperties where
  toJSON ThingGroupProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("thingGroupDescription" Core..=)
              Prelude.<$> thingGroupDescription,
            ("attributePayload" Core..=)
              Prelude.<$> attributePayload
          ]
      )
