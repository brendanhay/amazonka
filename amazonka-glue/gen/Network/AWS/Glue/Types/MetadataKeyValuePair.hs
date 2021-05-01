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
-- Module      : Network.AWS.Glue.Types.MetadataKeyValuePair
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.MetadataKeyValuePair where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | A structure containing a key value pair for metadata.
--
-- /See:/ 'newMetadataKeyValuePair' smart constructor.
data MetadataKeyValuePair = MetadataKeyValuePair'
  { -- | A metadata key.
    metadataKey :: Prelude.Maybe Prelude.Text,
    -- | A metadata key’s corresponding value.
    metadataValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'MetadataKeyValuePair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadataKey', 'metadataKeyValuePair_metadataKey' - A metadata key.
--
-- 'metadataValue', 'metadataKeyValuePair_metadataValue' - A metadata key’s corresponding value.
newMetadataKeyValuePair ::
  MetadataKeyValuePair
newMetadataKeyValuePair =
  MetadataKeyValuePair'
    { metadataKey =
        Prelude.Nothing,
      metadataValue = Prelude.Nothing
    }

-- | A metadata key.
metadataKeyValuePair_metadataKey :: Lens.Lens' MetadataKeyValuePair (Prelude.Maybe Prelude.Text)
metadataKeyValuePair_metadataKey = Lens.lens (\MetadataKeyValuePair' {metadataKey} -> metadataKey) (\s@MetadataKeyValuePair' {} a -> s {metadataKey = a} :: MetadataKeyValuePair)

-- | A metadata key’s corresponding value.
metadataKeyValuePair_metadataValue :: Lens.Lens' MetadataKeyValuePair (Prelude.Maybe Prelude.Text)
metadataKeyValuePair_metadataValue = Lens.lens (\MetadataKeyValuePair' {metadataValue} -> metadataValue) (\s@MetadataKeyValuePair' {} a -> s {metadataValue = a} :: MetadataKeyValuePair)

instance Prelude.Hashable MetadataKeyValuePair

instance Prelude.NFData MetadataKeyValuePair

instance Prelude.ToJSON MetadataKeyValuePair where
  toJSON MetadataKeyValuePair' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("MetadataKey" Prelude..=) Prelude.<$> metadataKey,
            ("MetadataValue" Prelude..=)
              Prelude.<$> metadataValue
          ]
      )
