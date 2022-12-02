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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.PoolFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.PoolFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types.PoolFilterName
import qualified Amazonka.Prelude as Prelude

-- | The information for a pool that meets a specified criteria.
--
-- /See:/ 'newPoolFilter' smart constructor.
data PoolFilter = PoolFilter'
  { -- | The name of the attribute to filter on.
    name :: PoolFilterName,
    -- | An array values to filter for.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PoolFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'poolFilter_name' - The name of the attribute to filter on.
--
-- 'values', 'poolFilter_values' - An array values to filter for.
newPoolFilter ::
  -- | 'name'
  PoolFilterName ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  PoolFilter
newPoolFilter pName_ pValues_ =
  PoolFilter'
    { name = pName_,
      values = Lens.coerced Lens.# pValues_
    }

-- | The name of the attribute to filter on.
poolFilter_name :: Lens.Lens' PoolFilter PoolFilterName
poolFilter_name = Lens.lens (\PoolFilter' {name} -> name) (\s@PoolFilter' {} a -> s {name = a} :: PoolFilter)

-- | An array values to filter for.
poolFilter_values :: Lens.Lens' PoolFilter (Prelude.NonEmpty Prelude.Text)
poolFilter_values = Lens.lens (\PoolFilter' {values} -> values) (\s@PoolFilter' {} a -> s {values = a} :: PoolFilter) Prelude.. Lens.coerced

instance Prelude.Hashable PoolFilter where
  hashWithSalt _salt PoolFilter' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` values

instance Prelude.NFData PoolFilter where
  rnf PoolFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON PoolFilter where
  toJSON PoolFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Values" Data..= values)
          ]
      )
