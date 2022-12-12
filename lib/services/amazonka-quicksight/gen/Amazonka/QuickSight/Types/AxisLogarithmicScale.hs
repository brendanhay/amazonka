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
-- Module      : Amazonka.QuickSight.Types.AxisLogarithmicScale
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AxisLogarithmicScale where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The logarithmic axis scale setup.
--
-- /See:/ 'newAxisLogarithmicScale' smart constructor.
data AxisLogarithmicScale = AxisLogarithmicScale'
  { -- | The base setup of a logarithmic axis scale.
    base :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AxisLogarithmicScale' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'base', 'axisLogarithmicScale_base' - The base setup of a logarithmic axis scale.
newAxisLogarithmicScale ::
  AxisLogarithmicScale
newAxisLogarithmicScale =
  AxisLogarithmicScale' {base = Prelude.Nothing}

-- | The base setup of a logarithmic axis scale.
axisLogarithmicScale_base :: Lens.Lens' AxisLogarithmicScale (Prelude.Maybe Prelude.Double)
axisLogarithmicScale_base = Lens.lens (\AxisLogarithmicScale' {base} -> base) (\s@AxisLogarithmicScale' {} a -> s {base = a} :: AxisLogarithmicScale)

instance Data.FromJSON AxisLogarithmicScale where
  parseJSON =
    Data.withObject
      "AxisLogarithmicScale"
      ( \x ->
          AxisLogarithmicScale'
            Prelude.<$> (x Data..:? "Base")
      )

instance Prelude.Hashable AxisLogarithmicScale where
  hashWithSalt _salt AxisLogarithmicScale' {..} =
    _salt `Prelude.hashWithSalt` base

instance Prelude.NFData AxisLogarithmicScale where
  rnf AxisLogarithmicScale' {..} = Prelude.rnf base

instance Data.ToJSON AxisLogarithmicScale where
  toJSON AxisLogarithmicScale' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Base" Data..=) Prelude.<$> base]
      )
