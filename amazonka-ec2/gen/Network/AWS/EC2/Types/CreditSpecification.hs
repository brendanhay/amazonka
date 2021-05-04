{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.EC2.Types.CreditSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.CreditSpecification where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the credit option for CPU usage of a T2, T3, or T3a instance.
--
-- /See:/ 'newCreditSpecification' smart constructor.
data CreditSpecification = CreditSpecification'
  { -- | The credit option for CPU usage of a T2, T3, or T3a instance. Valid
    -- values are @standard@ and @unlimited@.
    cpuCredits :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CreditSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cpuCredits', 'creditSpecification_cpuCredits' - The credit option for CPU usage of a T2, T3, or T3a instance. Valid
-- values are @standard@ and @unlimited@.
newCreditSpecification ::
  CreditSpecification
newCreditSpecification =
  CreditSpecification' {cpuCredits = Prelude.Nothing}

-- | The credit option for CPU usage of a T2, T3, or T3a instance. Valid
-- values are @standard@ and @unlimited@.
creditSpecification_cpuCredits :: Lens.Lens' CreditSpecification (Prelude.Maybe Prelude.Text)
creditSpecification_cpuCredits = Lens.lens (\CreditSpecification' {cpuCredits} -> cpuCredits) (\s@CreditSpecification' {} a -> s {cpuCredits = a} :: CreditSpecification)

instance Prelude.FromXML CreditSpecification where
  parseXML x =
    CreditSpecification'
      Prelude.<$> (x Prelude..@? "cpuCredits")

instance Prelude.Hashable CreditSpecification

instance Prelude.NFData CreditSpecification
