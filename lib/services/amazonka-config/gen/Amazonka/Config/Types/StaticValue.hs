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
-- Module      : Amazonka.Config.Types.StaticValue
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.StaticValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The static value of the resource.
--
-- /See:/ 'newStaticValue' smart constructor.
data StaticValue = StaticValue'
  { -- | A list of values. For example, the ARN of the assumed role.
    values :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StaticValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'values', 'staticValue_values' - A list of values. For example, the ARN of the assumed role.
newStaticValue ::
  StaticValue
newStaticValue =
  StaticValue' {values = Prelude.mempty}

-- | A list of values. For example, the ARN of the assumed role.
staticValue_values :: Lens.Lens' StaticValue [Prelude.Text]
staticValue_values = Lens.lens (\StaticValue' {values} -> values) (\s@StaticValue' {} a -> s {values = a} :: StaticValue) Prelude.. Lens.coerced

instance Data.FromJSON StaticValue where
  parseJSON =
    Data.withObject
      "StaticValue"
      ( \x ->
          StaticValue'
            Prelude.<$> (x Data..:? "Values" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable StaticValue where
  hashWithSalt _salt StaticValue' {..} =
    _salt `Prelude.hashWithSalt` values

instance Prelude.NFData StaticValue where
  rnf StaticValue' {..} = Prelude.rnf values

instance Data.ToJSON StaticValue where
  toJSON StaticValue' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Values" Data..= values)]
      )
