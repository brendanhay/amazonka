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
-- Module      : Network.AWS.EC2.Types.Phase1DHGroupNumbersRequestListValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1DHGroupNumbersRequestListValue where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a Diffie-Hellman group number for the VPN tunnel for phase 1
-- IKE negotiations.
--
-- /See:/ 'newPhase1DHGroupNumbersRequestListValue' smart constructor.
data Phase1DHGroupNumbersRequestListValue = Phase1DHGroupNumbersRequestListValue'
  { -- | The Diffie-Hellmann group number.
    value :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Phase1DHGroupNumbersRequestListValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'phase1DHGroupNumbersRequestListValue_value' - The Diffie-Hellmann group number.
newPhase1DHGroupNumbersRequestListValue ::
  Phase1DHGroupNumbersRequestListValue
newPhase1DHGroupNumbersRequestListValue =
  Phase1DHGroupNumbersRequestListValue'
    { value =
        Prelude.Nothing
    }

-- | The Diffie-Hellmann group number.
phase1DHGroupNumbersRequestListValue_value :: Lens.Lens' Phase1DHGroupNumbersRequestListValue (Prelude.Maybe Prelude.Int)
phase1DHGroupNumbersRequestListValue_value = Lens.lens (\Phase1DHGroupNumbersRequestListValue' {value} -> value) (\s@Phase1DHGroupNumbersRequestListValue' {} a -> s {value = a} :: Phase1DHGroupNumbersRequestListValue)

instance
  Prelude.Hashable
    Phase1DHGroupNumbersRequestListValue

instance
  Prelude.NFData
    Phase1DHGroupNumbersRequestListValue

instance
  Prelude.ToQuery
    Phase1DHGroupNumbersRequestListValue
  where
  toQuery Phase1DHGroupNumbersRequestListValue' {..} =
    Prelude.mconcat ["Value" Prelude.=: value]
