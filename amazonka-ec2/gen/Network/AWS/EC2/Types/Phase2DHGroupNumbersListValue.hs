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
-- Module      : Network.AWS.EC2.Types.Phase2DHGroupNumbersListValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase2DHGroupNumbersListValue where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens

-- | The Diffie-Hellmann group number for phase 2 IKE negotiations.
--
-- /See:/ 'newPhase2DHGroupNumbersListValue' smart constructor.
data Phase2DHGroupNumbersListValue = Phase2DHGroupNumbersListValue'
  { -- | The Diffie-Hellmann group number.
    value :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Phase2DHGroupNumbersListValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'phase2DHGroupNumbersListValue_value' - The Diffie-Hellmann group number.
newPhase2DHGroupNumbersListValue ::
  Phase2DHGroupNumbersListValue
newPhase2DHGroupNumbersListValue =
  Phase2DHGroupNumbersListValue'
    { value =
        Core.Nothing
    }

-- | The Diffie-Hellmann group number.
phase2DHGroupNumbersListValue_value :: Lens.Lens' Phase2DHGroupNumbersListValue (Core.Maybe Core.Int)
phase2DHGroupNumbersListValue_value = Lens.lens (\Phase2DHGroupNumbersListValue' {value} -> value) (\s@Phase2DHGroupNumbersListValue' {} a -> s {value = a} :: Phase2DHGroupNumbersListValue)

instance Core.FromXML Phase2DHGroupNumbersListValue where
  parseXML x =
    Phase2DHGroupNumbersListValue'
      Core.<$> (x Core..@? "value")

instance Core.Hashable Phase2DHGroupNumbersListValue

instance Core.NFData Phase2DHGroupNumbersListValue
