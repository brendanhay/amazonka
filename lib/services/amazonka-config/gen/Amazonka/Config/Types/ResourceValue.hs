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
-- Module      : Amazonka.Config.Types.ResourceValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.ResourceValue where

import Amazonka.Config.Types.ResourceValueType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The dynamic value of the resource.
--
-- /See:/ 'newResourceValue' smart constructor.
data ResourceValue = ResourceValue'
  { -- | The value is a resource ID.
    value :: ResourceValueType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'resourceValue_value' - The value is a resource ID.
newResourceValue ::
  -- | 'value'
  ResourceValueType ->
  ResourceValue
newResourceValue pValue_ =
  ResourceValue' {value = pValue_}

-- | The value is a resource ID.
resourceValue_value :: Lens.Lens' ResourceValue ResourceValueType
resourceValue_value = Lens.lens (\ResourceValue' {value} -> value) (\s@ResourceValue' {} a -> s {value = a} :: ResourceValue)

instance Core.FromJSON ResourceValue where
  parseJSON =
    Core.withObject
      "ResourceValue"
      ( \x ->
          ResourceValue' Prelude.<$> (x Core..: "Value")
      )

instance Prelude.Hashable ResourceValue where
  hashWithSalt _salt ResourceValue' {..} =
    _salt `Prelude.hashWithSalt` value

instance Prelude.NFData ResourceValue where
  rnf ResourceValue' {..} = Prelude.rnf value

instance Core.ToJSON ResourceValue where
  toJSON ResourceValue' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Value" Core..= value)]
      )
