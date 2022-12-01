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
-- Module      : Amazonka.IoT.Types.ThingTypeMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.ThingTypeMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The ThingTypeMetadata contains additional information about the thing
-- type including: creation date and time, a value indicating whether the
-- thing type is deprecated, and a date and time when time was deprecated.
--
-- /See:/ 'newThingTypeMetadata' smart constructor.
data ThingTypeMetadata = ThingTypeMetadata'
  { -- | Whether the thing type is deprecated. If __true__, no new things could
    -- be associated with this type.
    deprecated :: Prelude.Maybe Prelude.Bool,
    -- | The date and time when the thing type was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The date and time when the thing type was deprecated.
    deprecationDate :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThingTypeMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deprecated', 'thingTypeMetadata_deprecated' - Whether the thing type is deprecated. If __true__, no new things could
-- be associated with this type.
--
-- 'creationDate', 'thingTypeMetadata_creationDate' - The date and time when the thing type was created.
--
-- 'deprecationDate', 'thingTypeMetadata_deprecationDate' - The date and time when the thing type was deprecated.
newThingTypeMetadata ::
  ThingTypeMetadata
newThingTypeMetadata =
  ThingTypeMetadata'
    { deprecated = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      deprecationDate = Prelude.Nothing
    }

-- | Whether the thing type is deprecated. If __true__, no new things could
-- be associated with this type.
thingTypeMetadata_deprecated :: Lens.Lens' ThingTypeMetadata (Prelude.Maybe Prelude.Bool)
thingTypeMetadata_deprecated = Lens.lens (\ThingTypeMetadata' {deprecated} -> deprecated) (\s@ThingTypeMetadata' {} a -> s {deprecated = a} :: ThingTypeMetadata)

-- | The date and time when the thing type was created.
thingTypeMetadata_creationDate :: Lens.Lens' ThingTypeMetadata (Prelude.Maybe Prelude.UTCTime)
thingTypeMetadata_creationDate = Lens.lens (\ThingTypeMetadata' {creationDate} -> creationDate) (\s@ThingTypeMetadata' {} a -> s {creationDate = a} :: ThingTypeMetadata) Prelude.. Lens.mapping Core._Time

-- | The date and time when the thing type was deprecated.
thingTypeMetadata_deprecationDate :: Lens.Lens' ThingTypeMetadata (Prelude.Maybe Prelude.UTCTime)
thingTypeMetadata_deprecationDate = Lens.lens (\ThingTypeMetadata' {deprecationDate} -> deprecationDate) (\s@ThingTypeMetadata' {} a -> s {deprecationDate = a} :: ThingTypeMetadata) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ThingTypeMetadata where
  parseJSON =
    Core.withObject
      "ThingTypeMetadata"
      ( \x ->
          ThingTypeMetadata'
            Prelude.<$> (x Core..:? "deprecated")
            Prelude.<*> (x Core..:? "creationDate")
            Prelude.<*> (x Core..:? "deprecationDate")
      )

instance Prelude.Hashable ThingTypeMetadata where
  hashWithSalt _salt ThingTypeMetadata' {..} =
    _salt `Prelude.hashWithSalt` deprecated
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` deprecationDate

instance Prelude.NFData ThingTypeMetadata where
  rnf ThingTypeMetadata' {..} =
    Prelude.rnf deprecated
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf deprecationDate
