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
-- Module      : Network.AWS.LexModels.Types.SlotTypeMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexModels.Types.SlotTypeMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information about a slot type..
--
-- /See:/ 'newSlotTypeMetadata' smart constructor.
data SlotTypeMetadata = SlotTypeMetadata'
  { -- | The date that the slot type was created.
    createdDate :: Prelude.Maybe Prelude.POSIX,
    -- | The date that the slot type was updated. When you create a resource, the
    -- creation date and last updated date are the same.
    lastUpdatedDate :: Prelude.Maybe Prelude.POSIX,
    -- | The version of the slot type.
    version :: Prelude.Maybe Prelude.Text,
    -- | The name of the slot type.
    name :: Prelude.Maybe Prelude.Text,
    -- | A description of the slot type.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SlotTypeMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdDate', 'slotTypeMetadata_createdDate' - The date that the slot type was created.
--
-- 'lastUpdatedDate', 'slotTypeMetadata_lastUpdatedDate' - The date that the slot type was updated. When you create a resource, the
-- creation date and last updated date are the same.
--
-- 'version', 'slotTypeMetadata_version' - The version of the slot type.
--
-- 'name', 'slotTypeMetadata_name' - The name of the slot type.
--
-- 'description', 'slotTypeMetadata_description' - A description of the slot type.
newSlotTypeMetadata ::
  SlotTypeMetadata
newSlotTypeMetadata =
  SlotTypeMetadata'
    { createdDate = Prelude.Nothing,
      lastUpdatedDate = Prelude.Nothing,
      version = Prelude.Nothing,
      name = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The date that the slot type was created.
slotTypeMetadata_createdDate :: Lens.Lens' SlotTypeMetadata (Prelude.Maybe Prelude.UTCTime)
slotTypeMetadata_createdDate = Lens.lens (\SlotTypeMetadata' {createdDate} -> createdDate) (\s@SlotTypeMetadata' {} a -> s {createdDate = a} :: SlotTypeMetadata) Prelude.. Lens.mapping Prelude._Time

-- | The date that the slot type was updated. When you create a resource, the
-- creation date and last updated date are the same.
slotTypeMetadata_lastUpdatedDate :: Lens.Lens' SlotTypeMetadata (Prelude.Maybe Prelude.UTCTime)
slotTypeMetadata_lastUpdatedDate = Lens.lens (\SlotTypeMetadata' {lastUpdatedDate} -> lastUpdatedDate) (\s@SlotTypeMetadata' {} a -> s {lastUpdatedDate = a} :: SlotTypeMetadata) Prelude.. Lens.mapping Prelude._Time

-- | The version of the slot type.
slotTypeMetadata_version :: Lens.Lens' SlotTypeMetadata (Prelude.Maybe Prelude.Text)
slotTypeMetadata_version = Lens.lens (\SlotTypeMetadata' {version} -> version) (\s@SlotTypeMetadata' {} a -> s {version = a} :: SlotTypeMetadata)

-- | The name of the slot type.
slotTypeMetadata_name :: Lens.Lens' SlotTypeMetadata (Prelude.Maybe Prelude.Text)
slotTypeMetadata_name = Lens.lens (\SlotTypeMetadata' {name} -> name) (\s@SlotTypeMetadata' {} a -> s {name = a} :: SlotTypeMetadata)

-- | A description of the slot type.
slotTypeMetadata_description :: Lens.Lens' SlotTypeMetadata (Prelude.Maybe Prelude.Text)
slotTypeMetadata_description = Lens.lens (\SlotTypeMetadata' {description} -> description) (\s@SlotTypeMetadata' {} a -> s {description = a} :: SlotTypeMetadata)

instance Prelude.FromJSON SlotTypeMetadata where
  parseJSON =
    Prelude.withObject
      "SlotTypeMetadata"
      ( \x ->
          SlotTypeMetadata'
            Prelude.<$> (x Prelude..:? "createdDate")
            Prelude.<*> (x Prelude..:? "lastUpdatedDate")
            Prelude.<*> (x Prelude..:? "version")
            Prelude.<*> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "description")
      )

instance Prelude.Hashable SlotTypeMetadata

instance Prelude.NFData SlotTypeMetadata
