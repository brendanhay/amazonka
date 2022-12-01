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
-- Module      : Amazonka.LexModels.Types.SlotTypeMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.SlotTypeMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a slot type..
--
-- /See:/ 'newSlotTypeMetadata' smart constructor.
data SlotTypeMetadata = SlotTypeMetadata'
  { -- | The name of the slot type.
    name :: Prelude.Maybe Prelude.Text,
    -- | The date that the slot type was updated. When you create a resource, the
    -- creation date and last updated date are the same.
    lastUpdatedDate :: Prelude.Maybe Core.POSIX,
    -- | A description of the slot type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The date that the slot type was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The version of the slot type.
    version :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotTypeMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'slotTypeMetadata_name' - The name of the slot type.
--
-- 'lastUpdatedDate', 'slotTypeMetadata_lastUpdatedDate' - The date that the slot type was updated. When you create a resource, the
-- creation date and last updated date are the same.
--
-- 'description', 'slotTypeMetadata_description' - A description of the slot type.
--
-- 'createdDate', 'slotTypeMetadata_createdDate' - The date that the slot type was created.
--
-- 'version', 'slotTypeMetadata_version' - The version of the slot type.
newSlotTypeMetadata ::
  SlotTypeMetadata
newSlotTypeMetadata =
  SlotTypeMetadata'
    { name = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      description = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      version = Prelude.Nothing
    }

-- | The name of the slot type.
slotTypeMetadata_name :: Lens.Lens' SlotTypeMetadata (Prelude.Maybe Prelude.Text)
slotTypeMetadata_name = Lens.lens (\SlotTypeMetadata' {name} -> name) (\s@SlotTypeMetadata' {} a -> s {name = a} :: SlotTypeMetadata)

-- | The date that the slot type was updated. When you create a resource, the
-- creation date and last updated date are the same.
slotTypeMetadata_lastUpdatedDate :: Lens.Lens' SlotTypeMetadata (Prelude.Maybe Prelude.UTCTime)
slotTypeMetadata_lastUpdatedDate = Lens.lens (\SlotTypeMetadata' {lastUpdatedDate} -> lastUpdatedDate) (\s@SlotTypeMetadata' {} a -> s {lastUpdatedDate = a} :: SlotTypeMetadata) Prelude.. Lens.mapping Core._Time

-- | A description of the slot type.
slotTypeMetadata_description :: Lens.Lens' SlotTypeMetadata (Prelude.Maybe Prelude.Text)
slotTypeMetadata_description = Lens.lens (\SlotTypeMetadata' {description} -> description) (\s@SlotTypeMetadata' {} a -> s {description = a} :: SlotTypeMetadata)

-- | The date that the slot type was created.
slotTypeMetadata_createdDate :: Lens.Lens' SlotTypeMetadata (Prelude.Maybe Prelude.UTCTime)
slotTypeMetadata_createdDate = Lens.lens (\SlotTypeMetadata' {createdDate} -> createdDate) (\s@SlotTypeMetadata' {} a -> s {createdDate = a} :: SlotTypeMetadata) Prelude.. Lens.mapping Core._Time

-- | The version of the slot type.
slotTypeMetadata_version :: Lens.Lens' SlotTypeMetadata (Prelude.Maybe Prelude.Text)
slotTypeMetadata_version = Lens.lens (\SlotTypeMetadata' {version} -> version) (\s@SlotTypeMetadata' {} a -> s {version = a} :: SlotTypeMetadata)

instance Core.FromJSON SlotTypeMetadata where
  parseJSON =
    Core.withObject
      "SlotTypeMetadata"
      ( \x ->
          SlotTypeMetadata'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "lastUpdatedDate")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "createdDate")
            Prelude.<*> (x Core..:? "version")
      )

instance Prelude.Hashable SlotTypeMetadata where
  hashWithSalt _salt SlotTypeMetadata' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lastUpdatedDate
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` version

instance Prelude.NFData SlotTypeMetadata where
  rnf SlotTypeMetadata' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf lastUpdatedDate
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf version
