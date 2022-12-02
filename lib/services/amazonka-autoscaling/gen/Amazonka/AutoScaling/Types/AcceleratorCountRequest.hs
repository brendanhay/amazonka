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
-- Module      : Amazonka.AutoScaling.Types.AcceleratorCountRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.AcceleratorCountRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the minimum and maximum for the @AcceleratorCount@ object when
-- you specify InstanceRequirements for an Auto Scaling group.
--
-- /See:/ 'newAcceleratorCountRequest' smart constructor.
data AcceleratorCountRequest = AcceleratorCountRequest'
  { -- | The maximum value.
    max :: Prelude.Maybe Prelude.Natural,
    -- | The minimum value.
    min :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceleratorCountRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'acceleratorCountRequest_max' - The maximum value.
--
-- 'min', 'acceleratorCountRequest_min' - The minimum value.
newAcceleratorCountRequest ::
  AcceleratorCountRequest
newAcceleratorCountRequest =
  AcceleratorCountRequest'
    { max = Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum value.
acceleratorCountRequest_max :: Lens.Lens' AcceleratorCountRequest (Prelude.Maybe Prelude.Natural)
acceleratorCountRequest_max = Lens.lens (\AcceleratorCountRequest' {max} -> max) (\s@AcceleratorCountRequest' {} a -> s {max = a} :: AcceleratorCountRequest)

-- | The minimum value.
acceleratorCountRequest_min :: Lens.Lens' AcceleratorCountRequest (Prelude.Maybe Prelude.Natural)
acceleratorCountRequest_min = Lens.lens (\AcceleratorCountRequest' {min} -> min) (\s@AcceleratorCountRequest' {} a -> s {min = a} :: AcceleratorCountRequest)

instance Data.FromXML AcceleratorCountRequest where
  parseXML x =
    AcceleratorCountRequest'
      Prelude.<$> (x Data..@? "Max") Prelude.<*> (x Data..@? "Min")

instance Prelude.Hashable AcceleratorCountRequest where
  hashWithSalt _salt AcceleratorCountRequest' {..} =
    _salt `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData AcceleratorCountRequest where
  rnf AcceleratorCountRequest' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery AcceleratorCountRequest where
  toQuery AcceleratorCountRequest' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
