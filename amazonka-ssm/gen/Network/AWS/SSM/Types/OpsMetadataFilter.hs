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
-- Module      : Network.AWS.SSM.Types.OpsMetadataFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OpsMetadataFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | A filter to limit the number of OpsMetadata objects displayed.
--
-- /See:/ 'newOpsMetadataFilter' smart constructor.
data OpsMetadataFilter = OpsMetadataFilter'
  { -- | A filter key.
    key :: Core.Text,
    -- | A filter value.
    values :: Core.NonEmpty Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OpsMetadataFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'opsMetadataFilter_key' - A filter key.
--
-- 'values', 'opsMetadataFilter_values' - A filter value.
newOpsMetadataFilter ::
  -- | 'key'
  Core.Text ->
  -- | 'values'
  Core.NonEmpty Core.Text ->
  OpsMetadataFilter
newOpsMetadataFilter pKey_ pValues_ =
  OpsMetadataFilter'
    { key = pKey_,
      values = Lens._Coerce Lens.# pValues_
    }

-- | A filter key.
opsMetadataFilter_key :: Lens.Lens' OpsMetadataFilter Core.Text
opsMetadataFilter_key = Lens.lens (\OpsMetadataFilter' {key} -> key) (\s@OpsMetadataFilter' {} a -> s {key = a} :: OpsMetadataFilter)

-- | A filter value.
opsMetadataFilter_values :: Lens.Lens' OpsMetadataFilter (Core.NonEmpty Core.Text)
opsMetadataFilter_values = Lens.lens (\OpsMetadataFilter' {values} -> values) (\s@OpsMetadataFilter' {} a -> s {values = a} :: OpsMetadataFilter) Core.. Lens._Coerce

instance Core.Hashable OpsMetadataFilter

instance Core.NFData OpsMetadataFilter

instance Core.ToJSON OpsMetadataFilter where
  toJSON OpsMetadataFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Key" Core..= key),
            Core.Just ("Values" Core..= values)
          ]
      )
