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
-- Module      : Amazonka.XRay.Types.SamplingStrategy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.XRay.Types.SamplingStrategy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.XRay.Types.SamplingStrategyName

-- | The name and value of a sampling rule to apply to a trace summary.
--
-- /See:/ 'newSamplingStrategy' smart constructor.
data SamplingStrategy = SamplingStrategy'
  { -- | The name of a sampling rule.
    name :: Prelude.Maybe SamplingStrategyName,
    -- | The value of a sampling rule.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SamplingStrategy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'samplingStrategy_name' - The name of a sampling rule.
--
-- 'value', 'samplingStrategy_value' - The value of a sampling rule.
newSamplingStrategy ::
  SamplingStrategy
newSamplingStrategy =
  SamplingStrategy'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of a sampling rule.
samplingStrategy_name :: Lens.Lens' SamplingStrategy (Prelude.Maybe SamplingStrategyName)
samplingStrategy_name = Lens.lens (\SamplingStrategy' {name} -> name) (\s@SamplingStrategy' {} a -> s {name = a} :: SamplingStrategy)

-- | The value of a sampling rule.
samplingStrategy_value :: Lens.Lens' SamplingStrategy (Prelude.Maybe Prelude.Double)
samplingStrategy_value = Lens.lens (\SamplingStrategy' {value} -> value) (\s@SamplingStrategy' {} a -> s {value = a} :: SamplingStrategy)

instance Prelude.Hashable SamplingStrategy where
  hashWithSalt _salt SamplingStrategy' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData SamplingStrategy where
  rnf SamplingStrategy' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value

instance Data.ToJSON SamplingStrategy where
  toJSON SamplingStrategy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
