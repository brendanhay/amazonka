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
-- Module      : Amazonka.EC2.Types.AcceleratorCountRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AcceleratorCountRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum number of accelerators (GPUs, FPGAs, or Amazon
-- Web Services Inferentia chips) on an instance. To exclude
-- accelerator-enabled instance types, set @Max@ to @0@.
--
-- /See:/ 'newAcceleratorCountRequest' smart constructor.
data AcceleratorCountRequest = AcceleratorCountRequest'
  { -- | The maximum number of accelerators. To specify no maximum limit, omit
    -- this parameter. To exclude accelerator-enabled instance types, set @Max@
    -- to @0@.
    max :: Prelude.Maybe Prelude.Int,
    -- | The minimum number of accelerators. To specify no minimum limit, omit
    -- this parameter.
    min :: Prelude.Maybe Prelude.Int
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
-- 'max', 'acceleratorCountRequest_max' - The maximum number of accelerators. To specify no maximum limit, omit
-- this parameter. To exclude accelerator-enabled instance types, set @Max@
-- to @0@.
--
-- 'min', 'acceleratorCountRequest_min' - The minimum number of accelerators. To specify no minimum limit, omit
-- this parameter.
newAcceleratorCountRequest ::
  AcceleratorCountRequest
newAcceleratorCountRequest =
  AcceleratorCountRequest'
    { max = Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum number of accelerators. To specify no maximum limit, omit
-- this parameter. To exclude accelerator-enabled instance types, set @Max@
-- to @0@.
acceleratorCountRequest_max :: Lens.Lens' AcceleratorCountRequest (Prelude.Maybe Prelude.Int)
acceleratorCountRequest_max = Lens.lens (\AcceleratorCountRequest' {max} -> max) (\s@AcceleratorCountRequest' {} a -> s {max = a} :: AcceleratorCountRequest)

-- | The minimum number of accelerators. To specify no minimum limit, omit
-- this parameter.
acceleratorCountRequest_min :: Lens.Lens' AcceleratorCountRequest (Prelude.Maybe Prelude.Int)
acceleratorCountRequest_min = Lens.lens (\AcceleratorCountRequest' {min} -> min) (\s@AcceleratorCountRequest' {} a -> s {min = a} :: AcceleratorCountRequest)

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
