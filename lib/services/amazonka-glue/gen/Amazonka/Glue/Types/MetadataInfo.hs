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
-- Module      : Amazonka.Glue.Types.MetadataInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.MetadataInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.OtherMetadataValueListItem
import qualified Amazonka.Prelude as Prelude

-- | A structure containing metadata information for a schema version.
--
-- /See:/ 'newMetadataInfo' smart constructor.
data MetadataInfo = MetadataInfo'
  { -- | The time at which the entry was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The metadata key’s corresponding value.
    metadataValue :: Prelude.Maybe Prelude.Text,
    -- | Other metadata belonging to the same metadata key.
    otherMetadataValueList :: Prelude.Maybe [OtherMetadataValueListItem]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetadataInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdTime', 'metadataInfo_createdTime' - The time at which the entry was created.
--
-- 'metadataValue', 'metadataInfo_metadataValue' - The metadata key’s corresponding value.
--
-- 'otherMetadataValueList', 'metadataInfo_otherMetadataValueList' - Other metadata belonging to the same metadata key.
newMetadataInfo ::
  MetadataInfo
newMetadataInfo =
  MetadataInfo'
    { createdTime = Prelude.Nothing,
      metadataValue = Prelude.Nothing,
      otherMetadataValueList = Prelude.Nothing
    }

-- | The time at which the entry was created.
metadataInfo_createdTime :: Lens.Lens' MetadataInfo (Prelude.Maybe Prelude.Text)
metadataInfo_createdTime = Lens.lens (\MetadataInfo' {createdTime} -> createdTime) (\s@MetadataInfo' {} a -> s {createdTime = a} :: MetadataInfo)

-- | The metadata key’s corresponding value.
metadataInfo_metadataValue :: Lens.Lens' MetadataInfo (Prelude.Maybe Prelude.Text)
metadataInfo_metadataValue = Lens.lens (\MetadataInfo' {metadataValue} -> metadataValue) (\s@MetadataInfo' {} a -> s {metadataValue = a} :: MetadataInfo)

-- | Other metadata belonging to the same metadata key.
metadataInfo_otherMetadataValueList :: Lens.Lens' MetadataInfo (Prelude.Maybe [OtherMetadataValueListItem])
metadataInfo_otherMetadataValueList = Lens.lens (\MetadataInfo' {otherMetadataValueList} -> otherMetadataValueList) (\s@MetadataInfo' {} a -> s {otherMetadataValueList = a} :: MetadataInfo) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON MetadataInfo where
  parseJSON =
    Data.withObject
      "MetadataInfo"
      ( \x ->
          MetadataInfo'
            Prelude.<$> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "MetadataValue")
            Prelude.<*> ( x Data..:? "OtherMetadataValueList"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable MetadataInfo where
  hashWithSalt _salt MetadataInfo' {..} =
    _salt `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` metadataValue
      `Prelude.hashWithSalt` otherMetadataValueList

instance Prelude.NFData MetadataInfo where
  rnf MetadataInfo' {..} =
    Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf metadataValue
      `Prelude.seq` Prelude.rnf otherMetadataValueList
