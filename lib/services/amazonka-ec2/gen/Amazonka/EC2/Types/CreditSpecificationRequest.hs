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
-- Module      : Amazonka.EC2.Types.CreditSpecificationRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CreditSpecificationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The credit option for CPU usage of a T instance.
--
-- /See:/ 'newCreditSpecificationRequest' smart constructor.
data CreditSpecificationRequest = CreditSpecificationRequest'
  { -- | The credit option for CPU usage of a T instance.
    --
    -- Valid values: @standard@ | @unlimited@
    cpuCredits :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreditSpecificationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpuCredits', 'creditSpecificationRequest_cpuCredits' - The credit option for CPU usage of a T instance.
--
-- Valid values: @standard@ | @unlimited@
newCreditSpecificationRequest ::
  -- | 'cpuCredits'
  Prelude.Text ->
  CreditSpecificationRequest
newCreditSpecificationRequest pCpuCredits_ =
  CreditSpecificationRequest'
    { cpuCredits =
        pCpuCredits_
    }

-- | The credit option for CPU usage of a T instance.
--
-- Valid values: @standard@ | @unlimited@
creditSpecificationRequest_cpuCredits :: Lens.Lens' CreditSpecificationRequest Prelude.Text
creditSpecificationRequest_cpuCredits = Lens.lens (\CreditSpecificationRequest' {cpuCredits} -> cpuCredits) (\s@CreditSpecificationRequest' {} a -> s {cpuCredits = a} :: CreditSpecificationRequest)

instance Prelude.Hashable CreditSpecificationRequest where
  hashWithSalt _salt CreditSpecificationRequest' {..} =
    _salt `Prelude.hashWithSalt` cpuCredits

instance Prelude.NFData CreditSpecificationRequest where
  rnf CreditSpecificationRequest' {..} =
    Prelude.rnf cpuCredits

instance Data.ToQuery CreditSpecificationRequest where
  toQuery CreditSpecificationRequest' {..} =
    Prelude.mconcat ["CpuCredits" Data.=: cpuCredits]
