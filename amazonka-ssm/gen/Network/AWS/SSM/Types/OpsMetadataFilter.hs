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
import qualified Network.AWS.Prelude as Prelude

-- | A filter to limit the number of OpsMetadata objects displayed.
--
-- /See:/ 'newOpsMetadataFilter' smart constructor.
data OpsMetadataFilter = OpsMetadataFilter'
  { -- | A filter key.
    key :: Prelude.Text,
    -- | A filter value.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  OpsMetadataFilter
newOpsMetadataFilter pKey_ pValues_ =
  OpsMetadataFilter'
    { key = pKey_,
      values = Lens._Coerce Lens.# pValues_
    }

-- | A filter key.
opsMetadataFilter_key :: Lens.Lens' OpsMetadataFilter Prelude.Text
opsMetadataFilter_key = Lens.lens (\OpsMetadataFilter' {key} -> key) (\s@OpsMetadataFilter' {} a -> s {key = a} :: OpsMetadataFilter)

-- | A filter value.
opsMetadataFilter_values :: Lens.Lens' OpsMetadataFilter (Prelude.NonEmpty Prelude.Text)
opsMetadataFilter_values = Lens.lens (\OpsMetadataFilter' {values} -> values) (\s@OpsMetadataFilter' {} a -> s {values = a} :: OpsMetadataFilter) Prelude.. Lens._Coerce

instance Prelude.Hashable OpsMetadataFilter

instance Prelude.NFData OpsMetadataFilter

instance Core.ToJSON OpsMetadataFilter where
  toJSON OpsMetadataFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Core..= key),
            Prelude.Just ("Values" Core..= values)
          ]
      )
