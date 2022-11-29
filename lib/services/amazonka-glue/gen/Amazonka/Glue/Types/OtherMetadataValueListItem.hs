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
-- Module      : Amazonka.Glue.Types.OtherMetadataValueListItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.OtherMetadataValueListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure containing other metadata for a schema version belonging to
-- the same metadata key.
--
-- /See:/ 'newOtherMetadataValueListItem' smart constructor.
data OtherMetadataValueListItem = OtherMetadataValueListItem'
  { -- | The time at which the entry was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The metadata key’s corresponding value for the other metadata belonging
    -- to the same metadata key.
    metadataValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OtherMetadataValueListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTime', 'otherMetadataValueListItem_createdTime' - The time at which the entry was created.
--
-- 'metadataValue', 'otherMetadataValueListItem_metadataValue' - The metadata key’s corresponding value for the other metadata belonging
-- to the same metadata key.
newOtherMetadataValueListItem ::
  OtherMetadataValueListItem
newOtherMetadataValueListItem =
  OtherMetadataValueListItem'
    { createdTime =
        Prelude.Nothing,
      metadataValue = Prelude.Nothing
    }

-- | The time at which the entry was created.
otherMetadataValueListItem_createdTime :: Lens.Lens' OtherMetadataValueListItem (Prelude.Maybe Prelude.Text)
otherMetadataValueListItem_createdTime = Lens.lens (\OtherMetadataValueListItem' {createdTime} -> createdTime) (\s@OtherMetadataValueListItem' {} a -> s {createdTime = a} :: OtherMetadataValueListItem)

-- | The metadata key’s corresponding value for the other metadata belonging
-- to the same metadata key.
otherMetadataValueListItem_metadataValue :: Lens.Lens' OtherMetadataValueListItem (Prelude.Maybe Prelude.Text)
otherMetadataValueListItem_metadataValue = Lens.lens (\OtherMetadataValueListItem' {metadataValue} -> metadataValue) (\s@OtherMetadataValueListItem' {} a -> s {metadataValue = a} :: OtherMetadataValueListItem)

instance Core.FromJSON OtherMetadataValueListItem where
  parseJSON =
    Core.withObject
      "OtherMetadataValueListItem"
      ( \x ->
          OtherMetadataValueListItem'
            Prelude.<$> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "MetadataValue")
      )

instance Prelude.Hashable OtherMetadataValueListItem where
  hashWithSalt _salt OtherMetadataValueListItem' {..} =
    _salt `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` metadataValue

instance Prelude.NFData OtherMetadataValueListItem where
  rnf OtherMetadataValueListItem' {..} =
    Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf metadataValue
