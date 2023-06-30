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
-- Module      : Amazonka.SSM.Types.InstanceInformationFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InstanceInformationFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.InstanceInformationFilterKey

-- | Describes a filter for a specific list of managed nodes. You can filter
-- node information by using tags. You specify tags by using a key-value
-- mapping.
--
-- Use this operation instead of the
-- DescribeInstanceInformationRequest$InstanceInformationFilterList method.
-- The @InstanceInformationFilterList@ method is a legacy method and
-- doesn\'t support tags.
--
-- /See:/ 'newInstanceInformationFilter' smart constructor.
data InstanceInformationFilter = InstanceInformationFilter'
  { -- | The name of the filter.
    key :: InstanceInformationFilterKey,
    -- | The filter values.
    valueSet :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceInformationFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'instanceInformationFilter_key' - The name of the filter.
--
-- 'valueSet', 'instanceInformationFilter_valueSet' - The filter values.
newInstanceInformationFilter ::
  -- | 'key'
  InstanceInformationFilterKey ->
  -- | 'valueSet'
  Prelude.NonEmpty Prelude.Text ->
  InstanceInformationFilter
newInstanceInformationFilter pKey_ pValueSet_ =
  InstanceInformationFilter'
    { key = pKey_,
      valueSet = Lens.coerced Lens.# pValueSet_
    }

-- | The name of the filter.
instanceInformationFilter_key :: Lens.Lens' InstanceInformationFilter InstanceInformationFilterKey
instanceInformationFilter_key = Lens.lens (\InstanceInformationFilter' {key} -> key) (\s@InstanceInformationFilter' {} a -> s {key = a} :: InstanceInformationFilter)

-- | The filter values.
instanceInformationFilter_valueSet :: Lens.Lens' InstanceInformationFilter (Prelude.NonEmpty Prelude.Text)
instanceInformationFilter_valueSet = Lens.lens (\InstanceInformationFilter' {valueSet} -> valueSet) (\s@InstanceInformationFilter' {} a -> s {valueSet = a} :: InstanceInformationFilter) Prelude.. Lens.coerced

instance Prelude.Hashable InstanceInformationFilter where
  hashWithSalt _salt InstanceInformationFilter' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` valueSet

instance Prelude.NFData InstanceInformationFilter where
  rnf InstanceInformationFilter' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf valueSet

instance Data.ToJSON InstanceInformationFilter where
  toJSON InstanceInformationFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("key" Data..= key),
            Prelude.Just ("valueSet" Data..= valueSet)
          ]
      )
