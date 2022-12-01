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
-- Module      : Amazonka.Glue.Types.MetadataKeyValuePair
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.MetadataKeyValuePair where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure containing a key value pair for metadata.
--
-- /See:/ 'newMetadataKeyValuePair' smart constructor.
data MetadataKeyValuePair = MetadataKeyValuePair'
  { -- | A metadata key’s corresponding value.
    metadataValue :: Prelude.Maybe Prelude.Text,
    -- | A metadata key.
    metadataKey :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MetadataKeyValuePair' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'metadataValue', 'metadataKeyValuePair_metadataValue' - A metadata key’s corresponding value.
--
-- 'metadataKey', 'metadataKeyValuePair_metadataKey' - A metadata key.
newMetadataKeyValuePair ::
  MetadataKeyValuePair
newMetadataKeyValuePair =
  MetadataKeyValuePair'
    { metadataValue =
        Prelude.Nothing,
      metadataKey = Prelude.Nothing
    }

-- | A metadata key’s corresponding value.
metadataKeyValuePair_metadataValue :: Lens.Lens' MetadataKeyValuePair (Prelude.Maybe Prelude.Text)
metadataKeyValuePair_metadataValue = Lens.lens (\MetadataKeyValuePair' {metadataValue} -> metadataValue) (\s@MetadataKeyValuePair' {} a -> s {metadataValue = a} :: MetadataKeyValuePair)

-- | A metadata key.
metadataKeyValuePair_metadataKey :: Lens.Lens' MetadataKeyValuePair (Prelude.Maybe Prelude.Text)
metadataKeyValuePair_metadataKey = Lens.lens (\MetadataKeyValuePair' {metadataKey} -> metadataKey) (\s@MetadataKeyValuePair' {} a -> s {metadataKey = a} :: MetadataKeyValuePair)

instance Prelude.Hashable MetadataKeyValuePair where
  hashWithSalt _salt MetadataKeyValuePair' {..} =
    _salt `Prelude.hashWithSalt` metadataValue
      `Prelude.hashWithSalt` metadataKey

instance Prelude.NFData MetadataKeyValuePair where
  rnf MetadataKeyValuePair' {..} =
    Prelude.rnf metadataValue
      `Prelude.seq` Prelude.rnf metadataKey

instance Core.ToJSON MetadataKeyValuePair where
  toJSON MetadataKeyValuePair' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MetadataValue" Core..=) Prelude.<$> metadataValue,
            ("MetadataKey" Core..=) Prelude.<$> metadataKey
          ]
      )
