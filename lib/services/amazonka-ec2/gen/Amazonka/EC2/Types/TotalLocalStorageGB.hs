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
-- Module      : Amazonka.EC2.Types.TotalLocalStorageGB
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.TotalLocalStorageGB where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The minimum and maximum amount of total local storage, in GB.
--
-- /See:/ 'newTotalLocalStorageGB' smart constructor.
data TotalLocalStorageGB = TotalLocalStorageGB'
  { -- | The maximum amount of total local storage, in GB. If this parameter is
    -- not specified, there is no maximum limit.
    max :: Prelude.Maybe Prelude.Double,
    -- | The minimum amount of total local storage, in GB. If this parameter is
    -- not specified, there is no minimum limit.
    min :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TotalLocalStorageGB' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'totalLocalStorageGB_max' - The maximum amount of total local storage, in GB. If this parameter is
-- not specified, there is no maximum limit.
--
-- 'min', 'totalLocalStorageGB_min' - The minimum amount of total local storage, in GB. If this parameter is
-- not specified, there is no minimum limit.
newTotalLocalStorageGB ::
  TotalLocalStorageGB
newTotalLocalStorageGB =
  TotalLocalStorageGB'
    { max = Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The maximum amount of total local storage, in GB. If this parameter is
-- not specified, there is no maximum limit.
totalLocalStorageGB_max :: Lens.Lens' TotalLocalStorageGB (Prelude.Maybe Prelude.Double)
totalLocalStorageGB_max = Lens.lens (\TotalLocalStorageGB' {max} -> max) (\s@TotalLocalStorageGB' {} a -> s {max = a} :: TotalLocalStorageGB)

-- | The minimum amount of total local storage, in GB. If this parameter is
-- not specified, there is no minimum limit.
totalLocalStorageGB_min :: Lens.Lens' TotalLocalStorageGB (Prelude.Maybe Prelude.Double)
totalLocalStorageGB_min = Lens.lens (\TotalLocalStorageGB' {min} -> min) (\s@TotalLocalStorageGB' {} a -> s {min = a} :: TotalLocalStorageGB)

instance Data.FromXML TotalLocalStorageGB where
  parseXML x =
    TotalLocalStorageGB'
      Prelude.<$> (x Data..@? "max") Prelude.<*> (x Data..@? "min")

instance Prelude.Hashable TotalLocalStorageGB where
  hashWithSalt _salt TotalLocalStorageGB' {..} =
    _salt `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData TotalLocalStorageGB where
  rnf TotalLocalStorageGB' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery TotalLocalStorageGB where
  toQuery TotalLocalStorageGB' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
