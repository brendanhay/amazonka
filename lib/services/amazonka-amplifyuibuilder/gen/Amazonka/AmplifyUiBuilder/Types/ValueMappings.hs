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
-- Module      : Amazonka.AmplifyUiBuilder.Types.ValueMappings
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyUiBuilder.Types.ValueMappings where

import Amazonka.AmplifyUiBuilder.Types.ValueMapping
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the data binding configuration for a value map.
--
-- /See:/ 'newValueMappings' smart constructor.
data ValueMappings = ValueMappings'
  { -- | The value and display value pairs.
    values :: [ValueMapping]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ValueMappings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'valueMappings_values' - The value and display value pairs.
newValueMappings ::
  ValueMappings
newValueMappings =
  ValueMappings' {values = Prelude.mempty}

-- | The value and display value pairs.
valueMappings_values :: Lens.Lens' ValueMappings [ValueMapping]
valueMappings_values = Lens.lens (\ValueMappings' {values} -> values) (\s@ValueMappings' {} a -> s {values = a} :: ValueMappings) Prelude.. Lens.coerced

instance Core.FromJSON ValueMappings where
  parseJSON =
    Core.withObject
      "ValueMappings"
      ( \x ->
          ValueMappings'
            Prelude.<$> (x Core..:? "values" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable ValueMappings where
  hashWithSalt _salt ValueMappings' {..} =
    _salt `Prelude.hashWithSalt` values

instance Prelude.NFData ValueMappings where
  rnf ValueMappings' {..} = Prelude.rnf values

instance Core.ToJSON ValueMappings where
  toJSON ValueMappings' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("values" Core..= values)]
      )
