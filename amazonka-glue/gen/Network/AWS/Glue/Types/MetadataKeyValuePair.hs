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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A structure containing a key value pair for metadata.
--
-- /See:/ 'newMetadataKeyValuePair' smart constructor.
data MetadataKeyValuePair = MetadataKeyValuePair'
  { -- | A metadata key.
    metadataKey :: Core.Maybe Core.Text,
    -- | A metadata key’s corresponding value.
    metadataValue :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { metadataKey = Core.Nothing,
      metadataValue = Core.Nothing
    }

-- | A metadata key.
metadataKeyValuePair_metadataKey :: Lens.Lens' MetadataKeyValuePair (Core.Maybe Core.Text)
metadataKeyValuePair_metadataKey = Lens.lens (\MetadataKeyValuePair' {metadataKey} -> metadataKey) (\s@MetadataKeyValuePair' {} a -> s {metadataKey = a} :: MetadataKeyValuePair)

-- | A metadata key’s corresponding value.
metadataKeyValuePair_metadataValue :: Lens.Lens' MetadataKeyValuePair (Core.Maybe Core.Text)
metadataKeyValuePair_metadataValue = Lens.lens (\MetadataKeyValuePair' {metadataValue} -> metadataValue) (\s@MetadataKeyValuePair' {} a -> s {metadataValue = a} :: MetadataKeyValuePair)

instance Core.Hashable MetadataKeyValuePair

instance Core.NFData MetadataKeyValuePair

instance Core.ToJSON MetadataKeyValuePair where
  toJSON MetadataKeyValuePair' {..} =
    Core.object
      ( Core.catMaybes
          [ ("MetadataKey" Core..=) Core.<$> metadataKey,
            ("MetadataValue" Core..=) Core.<$> metadataValue
          ]
      )
