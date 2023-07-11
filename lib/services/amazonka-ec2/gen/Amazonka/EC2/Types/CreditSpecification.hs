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
-- Module      : Amazonka.EC2.Types.CreditSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.CreditSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the credit option for CPU usage of a T instance.
--
-- /See:/ 'newCreditSpecification' smart constructor.
data CreditSpecification = CreditSpecification'
  { -- | The credit option for CPU usage of a T instance.
    --
    -- Valid values: @standard@ | @unlimited@
    cpuCredits :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreditSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpuCredits', 'creditSpecification_cpuCredits' - The credit option for CPU usage of a T instance.
--
-- Valid values: @standard@ | @unlimited@
newCreditSpecification ::
  CreditSpecification
newCreditSpecification =
  CreditSpecification' {cpuCredits = Prelude.Nothing}

-- | The credit option for CPU usage of a T instance.
--
-- Valid values: @standard@ | @unlimited@
creditSpecification_cpuCredits :: Lens.Lens' CreditSpecification (Prelude.Maybe Prelude.Text)
creditSpecification_cpuCredits = Lens.lens (\CreditSpecification' {cpuCredits} -> cpuCredits) (\s@CreditSpecification' {} a -> s {cpuCredits = a} :: CreditSpecification)

instance Data.FromXML CreditSpecification where
  parseXML x =
    CreditSpecification'
      Prelude.<$> (x Data..@? "cpuCredits")

instance Prelude.Hashable CreditSpecification where
  hashWithSalt _salt CreditSpecification' {..} =
    _salt `Prelude.hashWithSalt` cpuCredits

instance Prelude.NFData CreditSpecification where
  rnf CreditSpecification' {..} = Prelude.rnf cpuCredits
