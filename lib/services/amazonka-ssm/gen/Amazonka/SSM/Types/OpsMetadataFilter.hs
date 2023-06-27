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
-- Module      : Amazonka.SSM.Types.OpsMetadataFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.OpsMetadataFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

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
      values = Lens.coerced Lens.# pValues_
    }

-- | A filter key.
opsMetadataFilter_key :: Lens.Lens' OpsMetadataFilter Prelude.Text
opsMetadataFilter_key = Lens.lens (\OpsMetadataFilter' {key} -> key) (\s@OpsMetadataFilter' {} a -> s {key = a} :: OpsMetadataFilter)

-- | A filter value.
opsMetadataFilter_values :: Lens.Lens' OpsMetadataFilter (Prelude.NonEmpty Prelude.Text)
opsMetadataFilter_values = Lens.lens (\OpsMetadataFilter' {values} -> values) (\s@OpsMetadataFilter' {} a -> s {values = a} :: OpsMetadataFilter) Prelude.. Lens.coerced

instance Prelude.Hashable OpsMetadataFilter where
  hashWithSalt _salt OpsMetadataFilter' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` values

instance Prelude.NFData OpsMetadataFilter where
  rnf OpsMetadataFilter' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf values

instance Data.ToJSON OpsMetadataFilter where
  toJSON OpsMetadataFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Key" Data..= key),
            Prelude.Just ("Values" Data..= values)
          ]
      )
