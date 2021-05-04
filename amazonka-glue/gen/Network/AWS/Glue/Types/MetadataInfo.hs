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
-- Module      : Network.AWS.Glue.Types.MetadataInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MetadataInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure containing metadata information for a schema version.
--
-- /See:/ 'newMetadataInfo' smart constructor.
data MetadataInfo = MetadataInfo'
  { -- | The time at which the entry was created.
    createdTime :: Prelude.Maybe Prelude.Text,
    -- | The metadata key’s corresponding value.
    metadataValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
newMetadataInfo ::
  MetadataInfo
newMetadataInfo =
  MetadataInfo'
    { createdTime = Prelude.Nothing,
      metadataValue = Prelude.Nothing
    }

-- | The time at which the entry was created.
metadataInfo_createdTime :: Lens.Lens' MetadataInfo (Prelude.Maybe Prelude.Text)
metadataInfo_createdTime = Lens.lens (\MetadataInfo' {createdTime} -> createdTime) (\s@MetadataInfo' {} a -> s {createdTime = a} :: MetadataInfo)

-- | The metadata key’s corresponding value.
metadataInfo_metadataValue :: Lens.Lens' MetadataInfo (Prelude.Maybe Prelude.Text)
metadataInfo_metadataValue = Lens.lens (\MetadataInfo' {metadataValue} -> metadataValue) (\s@MetadataInfo' {} a -> s {metadataValue = a} :: MetadataInfo)

instance Prelude.FromJSON MetadataInfo where
  parseJSON =
    Prelude.withObject
      "MetadataInfo"
      ( \x ->
          MetadataInfo'
            Prelude.<$> (x Prelude..:? "CreatedTime")
            Prelude.<*> (x Prelude..:? "MetadataValue")
      )

instance Prelude.Hashable MetadataInfo

instance Prelude.NFData MetadataInfo
