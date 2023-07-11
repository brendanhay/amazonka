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
-- Module      : Amazonka.PinpointSmsVoiceV2.Types.PoolOriginationIdentitiesFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.PinpointSmsVoiceV2.Types.PoolOriginationIdentitiesFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.PinpointSmsVoiceV2.Types.PoolOriginationIdentitiesFilterName
import qualified Amazonka.Prelude as Prelude

-- | Information about origination identities associated with a pool that
-- meets a specified criteria.
--
-- /See:/ 'newPoolOriginationIdentitiesFilter' smart constructor.
data PoolOriginationIdentitiesFilter = PoolOriginationIdentitiesFilter'
  { -- | The name of the attribute to filter on.
    name :: PoolOriginationIdentitiesFilterName,
    -- | An array values to filter for.
    values :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PoolOriginationIdentitiesFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'poolOriginationIdentitiesFilter_name' - The name of the attribute to filter on.
--
-- 'values', 'poolOriginationIdentitiesFilter_values' - An array values to filter for.
newPoolOriginationIdentitiesFilter ::
  -- | 'name'
  PoolOriginationIdentitiesFilterName ->
  -- | 'values'
  Prelude.NonEmpty Prelude.Text ->
  PoolOriginationIdentitiesFilter
newPoolOriginationIdentitiesFilter pName_ pValues_ =
  PoolOriginationIdentitiesFilter'
    { name = pName_,
      values = Lens.coerced Lens.# pValues_
    }

-- | The name of the attribute to filter on.
poolOriginationIdentitiesFilter_name :: Lens.Lens' PoolOriginationIdentitiesFilter PoolOriginationIdentitiesFilterName
poolOriginationIdentitiesFilter_name = Lens.lens (\PoolOriginationIdentitiesFilter' {name} -> name) (\s@PoolOriginationIdentitiesFilter' {} a -> s {name = a} :: PoolOriginationIdentitiesFilter)

-- | An array values to filter for.
poolOriginationIdentitiesFilter_values :: Lens.Lens' PoolOriginationIdentitiesFilter (Prelude.NonEmpty Prelude.Text)
poolOriginationIdentitiesFilter_values = Lens.lens (\PoolOriginationIdentitiesFilter' {values} -> values) (\s@PoolOriginationIdentitiesFilter' {} a -> s {values = a} :: PoolOriginationIdentitiesFilter) Prelude.. Lens.coerced

instance
  Prelude.Hashable
    PoolOriginationIdentitiesFilter
  where
  hashWithSalt
    _salt
    PoolOriginationIdentitiesFilter' {..} =
      _salt
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` values

instance
  Prelude.NFData
    PoolOriginationIdentitiesFilter
  where
  rnf PoolOriginationIdentitiesFilter' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf values

instance Data.ToJSON PoolOriginationIdentitiesFilter where
  toJSON PoolOriginationIdentitiesFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Values" Data..= values)
          ]
      )
