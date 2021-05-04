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
-- Module      : Network.AWS.IoT.Types.ThingTypeMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.ThingTypeMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The ThingTypeMetadata contains additional information about the thing
-- type including: creation date and time, a value indicating whether the
-- thing type is deprecated, and a date and time when time was deprecated.
--
-- /See:/ 'newThingTypeMetadata' smart constructor.
data ThingTypeMetadata = ThingTypeMetadata'
  { -- | The date and time when the thing type was deprecated.
    deprecationDate :: Prelude.Maybe Prelude.POSIX,
    -- | The date and time when the thing type was created.
    creationDate :: Prelude.Maybe Prelude.POSIX,
    -- | Whether the thing type is deprecated. If __true__, no new things could
    -- be associated with this type.
    deprecated :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ThingTypeMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deprecationDate', 'thingTypeMetadata_deprecationDate' - The date and time when the thing type was deprecated.
--
-- 'creationDate', 'thingTypeMetadata_creationDate' - The date and time when the thing type was created.
--
-- 'deprecated', 'thingTypeMetadata_deprecated' - Whether the thing type is deprecated. If __true__, no new things could
-- be associated with this type.
newThingTypeMetadata ::
  ThingTypeMetadata
newThingTypeMetadata =
  ThingTypeMetadata'
    { deprecationDate =
        Prelude.Nothing,
      creationDate = Prelude.Nothing,
      deprecated = Prelude.Nothing
    }

-- | The date and time when the thing type was deprecated.
thingTypeMetadata_deprecationDate :: Lens.Lens' ThingTypeMetadata (Prelude.Maybe Prelude.UTCTime)
thingTypeMetadata_deprecationDate = Lens.lens (\ThingTypeMetadata' {deprecationDate} -> deprecationDate) (\s@ThingTypeMetadata' {} a -> s {deprecationDate = a} :: ThingTypeMetadata) Prelude.. Lens.mapping Prelude._Time

-- | The date and time when the thing type was created.
thingTypeMetadata_creationDate :: Lens.Lens' ThingTypeMetadata (Prelude.Maybe Prelude.UTCTime)
thingTypeMetadata_creationDate = Lens.lens (\ThingTypeMetadata' {creationDate} -> creationDate) (\s@ThingTypeMetadata' {} a -> s {creationDate = a} :: ThingTypeMetadata) Prelude.. Lens.mapping Prelude._Time

-- | Whether the thing type is deprecated. If __true__, no new things could
-- be associated with this type.
thingTypeMetadata_deprecated :: Lens.Lens' ThingTypeMetadata (Prelude.Maybe Prelude.Bool)
thingTypeMetadata_deprecated = Lens.lens (\ThingTypeMetadata' {deprecated} -> deprecated) (\s@ThingTypeMetadata' {} a -> s {deprecated = a} :: ThingTypeMetadata)

instance Prelude.FromJSON ThingTypeMetadata where
  parseJSON =
    Prelude.withObject
      "ThingTypeMetadata"
      ( \x ->
          ThingTypeMetadata'
            Prelude.<$> (x Prelude..:? "deprecationDate")
            Prelude.<*> (x Prelude..:? "creationDate")
            Prelude.<*> (x Prelude..:? "deprecated")
      )

instance Prelude.Hashable ThingTypeMetadata

instance Prelude.NFData ThingTypeMetadata
