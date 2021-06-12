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
-- Module      : Network.AWS.EC2.Types.Phase1DHGroupNumbersListValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1DHGroupNumbersListValue where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | The Diffie-Hellmann group number for phase 1 IKE negotiations.
--
-- /See:/ 'newPhase1DHGroupNumbersListValue' smart constructor.
data Phase1DHGroupNumbersListValue = Phase1DHGroupNumbersListValue'
  { -- | The Diffie-Hellmann group number.
    value :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Phase1DHGroupNumbersListValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'phase1DHGroupNumbersListValue_value' - The Diffie-Hellmann group number.
newPhase1DHGroupNumbersListValue ::
  Phase1DHGroupNumbersListValue
newPhase1DHGroupNumbersListValue =
  Phase1DHGroupNumbersListValue'
    { value =
        Core.Nothing
    }

-- | The Diffie-Hellmann group number.
phase1DHGroupNumbersListValue_value :: Lens.Lens' Phase1DHGroupNumbersListValue (Core.Maybe Core.Int)
phase1DHGroupNumbersListValue_value = Lens.lens (\Phase1DHGroupNumbersListValue' {value} -> value) (\s@Phase1DHGroupNumbersListValue' {} a -> s {value = a} :: Phase1DHGroupNumbersListValue)

instance Core.FromXML Phase1DHGroupNumbersListValue where
  parseXML x =
    Phase1DHGroupNumbersListValue'
      Core.<$> (x Core..@? "value")

instance Core.Hashable Phase1DHGroupNumbersListValue

instance Core.NFData Phase1DHGroupNumbersListValue
