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
-- Module      : Network.AWS.EC2.Types.Phase1DHGroupNumbersListValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1DHGroupNumbersListValue where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The Diffie-Hellmann group number for phase 1 IKE negotiations.
--
-- /See:/ 'newPhase1DHGroupNumbersListValue' smart constructor.
data Phase1DHGroupNumbersListValue = Phase1DHGroupNumbersListValue'
  { -- | The Diffie-Hellmann group number.
    value :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The Diffie-Hellmann group number.
phase1DHGroupNumbersListValue_value :: Lens.Lens' Phase1DHGroupNumbersListValue (Prelude.Maybe Prelude.Int)
phase1DHGroupNumbersListValue_value = Lens.lens (\Phase1DHGroupNumbersListValue' {value} -> value) (\s@Phase1DHGroupNumbersListValue' {} a -> s {value = a} :: Phase1DHGroupNumbersListValue)

instance
  Prelude.FromXML
    Phase1DHGroupNumbersListValue
  where
  parseXML x =
    Phase1DHGroupNumbersListValue'
      Prelude.<$> (x Prelude..@? "value")

instance
  Prelude.Hashable
    Phase1DHGroupNumbersListValue

instance Prelude.NFData Phase1DHGroupNumbersListValue
