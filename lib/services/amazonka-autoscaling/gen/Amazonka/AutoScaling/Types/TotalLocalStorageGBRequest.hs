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
-- Module      : Amazonka.AutoScaling.Types.TotalLocalStorageGBRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.TotalLocalStorageGBRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the minimum and maximum for the @TotalLocalStorageGB@ object
-- when you specify InstanceRequirements for an Auto Scaling group.
--
-- /See:/ 'newTotalLocalStorageGBRequest' smart constructor.
data TotalLocalStorageGBRequest = TotalLocalStorageGBRequest'
  { -- | The storage maximum in GB.
    max :: Prelude.Maybe Prelude.Double,
    -- | The storage minimum in GB.
    min :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TotalLocalStorageGBRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'max', 'totalLocalStorageGBRequest_max' - The storage maximum in GB.
--
-- 'min', 'totalLocalStorageGBRequest_min' - The storage minimum in GB.
newTotalLocalStorageGBRequest ::
  TotalLocalStorageGBRequest
newTotalLocalStorageGBRequest =
  TotalLocalStorageGBRequest'
    { max = Prelude.Nothing,
      min = Prelude.Nothing
    }

-- | The storage maximum in GB.
totalLocalStorageGBRequest_max :: Lens.Lens' TotalLocalStorageGBRequest (Prelude.Maybe Prelude.Double)
totalLocalStorageGBRequest_max = Lens.lens (\TotalLocalStorageGBRequest' {max} -> max) (\s@TotalLocalStorageGBRequest' {} a -> s {max = a} :: TotalLocalStorageGBRequest)

-- | The storage minimum in GB.
totalLocalStorageGBRequest_min :: Lens.Lens' TotalLocalStorageGBRequest (Prelude.Maybe Prelude.Double)
totalLocalStorageGBRequest_min = Lens.lens (\TotalLocalStorageGBRequest' {min} -> min) (\s@TotalLocalStorageGBRequest' {} a -> s {min = a} :: TotalLocalStorageGBRequest)

instance Data.FromXML TotalLocalStorageGBRequest where
  parseXML x =
    TotalLocalStorageGBRequest'
      Prelude.<$> (x Data..@? "Max")
      Prelude.<*> (x Data..@? "Min")

instance Prelude.Hashable TotalLocalStorageGBRequest where
  hashWithSalt _salt TotalLocalStorageGBRequest' {..} =
    _salt
      `Prelude.hashWithSalt` max
      `Prelude.hashWithSalt` min

instance Prelude.NFData TotalLocalStorageGBRequest where
  rnf TotalLocalStorageGBRequest' {..} =
    Prelude.rnf max `Prelude.seq` Prelude.rnf min

instance Data.ToQuery TotalLocalStorageGBRequest where
  toQuery TotalLocalStorageGBRequest' {..} =
    Prelude.mconcat
      ["Max" Data.=: max, "Min" Data.=: min]
