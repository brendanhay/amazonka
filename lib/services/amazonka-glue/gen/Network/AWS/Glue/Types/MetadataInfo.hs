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
-- Module      : Network.AWS.Glue.Types.MetadataInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MetadataInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types.OtherMetadataValueListItem
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure containing metadata information for a schema version.
--
-- /See:/ 'newMetadataInfo' smart constructor.
data MetadataInfo = MetadataInfo'
  { -- | The time at which the entry was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | Other metadata belonging to the same metadata key.
    otherMetadataValueList :: Prelude.Maybe [OtherMetadataValueListItem],
    -- | The metadata key’s corresponding value.
    metadataValue :: Prelude.Maybe Prelude.Text
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
-- 'otherMetadataValueList', 'metadataInfo_otherMetadataValueList' - Other metadata belonging to the same metadata key.
--
-- 'metadataValue', 'metadataInfo_metadataValue' - The metadata key’s corresponding value.
newMetadataInfo ::
  MetadataInfo
newMetadataInfo =
  MetadataInfo'
    { createdTime = Prelude.Nothing,
      otherMetadataValueList = Prelude.Nothing,
      metadataValue = Prelude.Nothing
    }

-- | The time at which the entry was created.
metadataInfo_createdTime :: Lens.Lens' MetadataInfo (Prelude.Maybe Prelude.Text)
metadataInfo_createdTime = Lens.lens (\MetadataInfo' {createdTime} -> createdTime) (\s@MetadataInfo' {} a -> s {createdTime = a} :: MetadataInfo)

-- | Other metadata belonging to the same metadata key.
metadataInfo_otherMetadataValueList :: Lens.Lens' MetadataInfo (Prelude.Maybe [OtherMetadataValueListItem])
metadataInfo_otherMetadataValueList = Lens.lens (\MetadataInfo' {otherMetadataValueList} -> otherMetadataValueList) (\s@MetadataInfo' {} a -> s {otherMetadataValueList = a} :: MetadataInfo) Prelude.. Lens.mapping Lens.coerced

-- | The metadata key’s corresponding value.
metadataInfo_metadataValue :: Lens.Lens' MetadataInfo (Prelude.Maybe Prelude.Text)
metadataInfo_metadataValue = Lens.lens (\MetadataInfo' {metadataValue} -> metadataValue) (\s@MetadataInfo' {} a -> s {metadataValue = a} :: MetadataInfo)

instance Core.FromJSON MetadataInfo where
  parseJSON =
    Core.withObject
      "MetadataInfo"
      ( \x ->
          MetadataInfo'
            Prelude.<$> (x Core..:? "CreatedTime")
            Prelude.<*> ( x Core..:? "OtherMetadataValueList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "MetadataValue")
      )

instance Prelude.Hashable MetadataInfo

instance Prelude.NFData MetadataInfo
