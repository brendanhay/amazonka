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
-- Module      : Network.AWS.XRay.Types.SamplingStrategy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingStrategy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.XRay.Types.SamplingStrategyName

-- | The name and value of a sampling rule to apply to a trace summary.
--
-- /See:/ 'newSamplingStrategy' smart constructor.
data SamplingStrategy = SamplingStrategy'
  { -- | The name of a sampling rule.
    name :: Core.Maybe SamplingStrategyName,
    -- | The value of a sampling rule.
    value :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { name = Core.Nothing,
      value = Core.Nothing
    }

-- | The name of a sampling rule.
samplingStrategy_name :: Lens.Lens' SamplingStrategy (Core.Maybe SamplingStrategyName)
samplingStrategy_name = Lens.lens (\SamplingStrategy' {name} -> name) (\s@SamplingStrategy' {} a -> s {name = a} :: SamplingStrategy)

-- | The value of a sampling rule.
samplingStrategy_value :: Lens.Lens' SamplingStrategy (Core.Maybe Core.Double)
samplingStrategy_value = Lens.lens (\SamplingStrategy' {value} -> value) (\s@SamplingStrategy' {} a -> s {value = a} :: SamplingStrategy)

instance Core.Hashable SamplingStrategy

instance Core.NFData SamplingStrategy

instance Core.ToJSON SamplingStrategy where
  toJSON SamplingStrategy' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Name" Core..=) Core.<$> name,
            ("Value" Core..=) Core.<$> value
          ]
      )
