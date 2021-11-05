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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ThingGroupProperties where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.AttributePayload
import qualified Amazonka.Lens as Lens
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

instance Core.FromJSON ThingGroupProperties where
  parseJSON =
    Core.withObject
      "ThingGroupProperties"
      ( \x ->
          ThingGroupProperties'
            Prelude.<$> (x Core..:? "attributePayload")
            Prelude.<*> (x Core..:? "thingGroupDescription")
      )

instance Prelude.Hashable ThingGroupProperties

instance Prelude.NFData ThingGroupProperties

instance Core.ToJSON ThingGroupProperties where
  toJSON ThingGroupProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("attributePayload" Core..=)
              Prelude.<$> attributePayload,
            ("thingGroupDescription" Core..=)
              Prelude.<$> thingGroupDescription
          ]
      )
