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
-- Module      : Network.AWS.IoT.Types.ThingTypeDefinition
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingTypeDefinition where

import Network.AWS.IoT.Types.ThingTypeMetadata
import Network.AWS.IoT.Types.ThingTypeProperties
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The definition of the thing type, including thing type name and
-- description.
--
-- /See:/ 'newThingTypeDefinition' smart constructor.
data ThingTypeDefinition = ThingTypeDefinition'
  { -- | The ThingTypeProperties for the thing type.
    thingTypeProperties :: Prelude.Maybe ThingTypeProperties,
    -- | The ThingTypeMetadata contains additional information about the thing
    -- type including: creation date and time, a value indicating whether the
    -- thing type is deprecated, and a date and time when it was deprecated.
    thingTypeMetadata :: Prelude.Maybe ThingTypeMetadata,
    -- | The thing type ARN.
    thingTypeArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the thing type.
    thingTypeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ThingTypeDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thingTypeProperties', 'thingTypeDefinition_thingTypeProperties' - The ThingTypeProperties for the thing type.
--
-- 'thingTypeMetadata', 'thingTypeDefinition_thingTypeMetadata' - The ThingTypeMetadata contains additional information about the thing
-- type including: creation date and time, a value indicating whether the
-- thing type is deprecated, and a date and time when it was deprecated.
--
-- 'thingTypeArn', 'thingTypeDefinition_thingTypeArn' - The thing type ARN.
--
-- 'thingTypeName', 'thingTypeDefinition_thingTypeName' - The name of the thing type.
newThingTypeDefinition ::
  ThingTypeDefinition
newThingTypeDefinition =
  ThingTypeDefinition'
    { thingTypeProperties =
        Prelude.Nothing,
      thingTypeMetadata = Prelude.Nothing,
      thingTypeArn = Prelude.Nothing,
      thingTypeName = Prelude.Nothing
    }

-- | The ThingTypeProperties for the thing type.
thingTypeDefinition_thingTypeProperties :: Lens.Lens' ThingTypeDefinition (Prelude.Maybe ThingTypeProperties)
thingTypeDefinition_thingTypeProperties = Lens.lens (\ThingTypeDefinition' {thingTypeProperties} -> thingTypeProperties) (\s@ThingTypeDefinition' {} a -> s {thingTypeProperties = a} :: ThingTypeDefinition)

-- | The ThingTypeMetadata contains additional information about the thing
-- type including: creation date and time, a value indicating whether the
-- thing type is deprecated, and a date and time when it was deprecated.
thingTypeDefinition_thingTypeMetadata :: Lens.Lens' ThingTypeDefinition (Prelude.Maybe ThingTypeMetadata)
thingTypeDefinition_thingTypeMetadata = Lens.lens (\ThingTypeDefinition' {thingTypeMetadata} -> thingTypeMetadata) (\s@ThingTypeDefinition' {} a -> s {thingTypeMetadata = a} :: ThingTypeDefinition)

-- | The thing type ARN.
thingTypeDefinition_thingTypeArn :: Lens.Lens' ThingTypeDefinition (Prelude.Maybe Prelude.Text)
thingTypeDefinition_thingTypeArn = Lens.lens (\ThingTypeDefinition' {thingTypeArn} -> thingTypeArn) (\s@ThingTypeDefinition' {} a -> s {thingTypeArn = a} :: ThingTypeDefinition)

-- | The name of the thing type.
thingTypeDefinition_thingTypeName :: Lens.Lens' ThingTypeDefinition (Prelude.Maybe Prelude.Text)
thingTypeDefinition_thingTypeName = Lens.lens (\ThingTypeDefinition' {thingTypeName} -> thingTypeName) (\s@ThingTypeDefinition' {} a -> s {thingTypeName = a} :: ThingTypeDefinition)

instance Prelude.FromJSON ThingTypeDefinition where
  parseJSON =
    Prelude.withObject
      "ThingTypeDefinition"
      ( \x ->
          ThingTypeDefinition'
            Prelude.<$> (x Prelude..:? "thingTypeProperties")
            Prelude.<*> (x Prelude..:? "thingTypeMetadata")
            Prelude.<*> (x Prelude..:? "thingTypeArn")
            Prelude.<*> (x Prelude..:? "thingTypeName")
      )

instance Prelude.Hashable ThingTypeDefinition

instance Prelude.NFData ThingTypeDefinition
