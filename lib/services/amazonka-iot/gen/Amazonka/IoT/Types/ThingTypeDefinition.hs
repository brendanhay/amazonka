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
-- Module      : Amazonka.IoT.Types.ThingTypeDefinition
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ThingTypeDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT.Types.ThingTypeMetadata
import Amazonka.IoT.Types.ThingTypeProperties
import qualified Amazonka.Prelude as Prelude

-- | The definition of the thing type, including thing type name and
-- description.
--
-- /See:/ 'newThingTypeDefinition' smart constructor.
data ThingTypeDefinition = ThingTypeDefinition'
  { -- | The ThingTypeMetadata contains additional information about the thing
    -- type including: creation date and time, a value indicating whether the
    -- thing type is deprecated, and a date and time when it was deprecated.
    thingTypeMetadata :: Prelude.Maybe ThingTypeMetadata,
    -- | The name of the thing type.
    thingTypeName :: Prelude.Maybe Prelude.Text,
    -- | The thing type ARN.
    thingTypeArn :: Prelude.Maybe Prelude.Text,
    -- | The ThingTypeProperties for the thing type.
    thingTypeProperties :: Prelude.Maybe ThingTypeProperties
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThingTypeDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingTypeMetadata', 'thingTypeDefinition_thingTypeMetadata' - The ThingTypeMetadata contains additional information about the thing
-- type including: creation date and time, a value indicating whether the
-- thing type is deprecated, and a date and time when it was deprecated.
--
-- 'thingTypeName', 'thingTypeDefinition_thingTypeName' - The name of the thing type.
--
-- 'thingTypeArn', 'thingTypeDefinition_thingTypeArn' - The thing type ARN.
--
-- 'thingTypeProperties', 'thingTypeDefinition_thingTypeProperties' - The ThingTypeProperties for the thing type.
newThingTypeDefinition ::
  ThingTypeDefinition
newThingTypeDefinition =
  ThingTypeDefinition'
    { thingTypeMetadata =
        Prelude.Nothing,
      thingTypeName = Prelude.Nothing,
      thingTypeArn = Prelude.Nothing,
      thingTypeProperties = Prelude.Nothing
    }

-- | The ThingTypeMetadata contains additional information about the thing
-- type including: creation date and time, a value indicating whether the
-- thing type is deprecated, and a date and time when it was deprecated.
thingTypeDefinition_thingTypeMetadata :: Lens.Lens' ThingTypeDefinition (Prelude.Maybe ThingTypeMetadata)
thingTypeDefinition_thingTypeMetadata = Lens.lens (\ThingTypeDefinition' {thingTypeMetadata} -> thingTypeMetadata) (\s@ThingTypeDefinition' {} a -> s {thingTypeMetadata = a} :: ThingTypeDefinition)

-- | The name of the thing type.
thingTypeDefinition_thingTypeName :: Lens.Lens' ThingTypeDefinition (Prelude.Maybe Prelude.Text)
thingTypeDefinition_thingTypeName = Lens.lens (\ThingTypeDefinition' {thingTypeName} -> thingTypeName) (\s@ThingTypeDefinition' {} a -> s {thingTypeName = a} :: ThingTypeDefinition)

-- | The thing type ARN.
thingTypeDefinition_thingTypeArn :: Lens.Lens' ThingTypeDefinition (Prelude.Maybe Prelude.Text)
thingTypeDefinition_thingTypeArn = Lens.lens (\ThingTypeDefinition' {thingTypeArn} -> thingTypeArn) (\s@ThingTypeDefinition' {} a -> s {thingTypeArn = a} :: ThingTypeDefinition)

-- | The ThingTypeProperties for the thing type.
thingTypeDefinition_thingTypeProperties :: Lens.Lens' ThingTypeDefinition (Prelude.Maybe ThingTypeProperties)
thingTypeDefinition_thingTypeProperties = Lens.lens (\ThingTypeDefinition' {thingTypeProperties} -> thingTypeProperties) (\s@ThingTypeDefinition' {} a -> s {thingTypeProperties = a} :: ThingTypeDefinition)

instance Data.FromJSON ThingTypeDefinition where
  parseJSON =
    Data.withObject
      "ThingTypeDefinition"
      ( \x ->
          ThingTypeDefinition'
            Prelude.<$> (x Data..:? "thingTypeMetadata")
            Prelude.<*> (x Data..:? "thingTypeName")
            Prelude.<*> (x Data..:? "thingTypeArn")
            Prelude.<*> (x Data..:? "thingTypeProperties")
      )

instance Prelude.Hashable ThingTypeDefinition where
  hashWithSalt _salt ThingTypeDefinition' {..} =
    _salt `Prelude.hashWithSalt` thingTypeMetadata
      `Prelude.hashWithSalt` thingTypeName
      `Prelude.hashWithSalt` thingTypeArn
      `Prelude.hashWithSalt` thingTypeProperties

instance Prelude.NFData ThingTypeDefinition where
  rnf ThingTypeDefinition' {..} =
    Prelude.rnf thingTypeMetadata
      `Prelude.seq` Prelude.rnf thingTypeName
      `Prelude.seq` Prelude.rnf thingTypeArn
      `Prelude.seq` Prelude.rnf thingTypeProperties
