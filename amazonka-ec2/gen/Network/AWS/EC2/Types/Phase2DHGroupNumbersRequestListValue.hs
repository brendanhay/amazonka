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
-- Module      : Network.AWS.EC2.Types.Phase2DHGroupNumbersRequestListValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase2DHGroupNumbersRequestListValue where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies a Diffie-Hellman group number for the VPN tunnel for phase 2
-- IKE negotiations.
--
-- /See:/ 'newPhase2DHGroupNumbersRequestListValue' smart constructor.
data Phase2DHGroupNumbersRequestListValue = Phase2DHGroupNumbersRequestListValue'
  { -- | The Diffie-Hellmann group number.
    value :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Phase2DHGroupNumbersRequestListValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'phase2DHGroupNumbersRequestListValue_value' - The Diffie-Hellmann group number.
newPhase2DHGroupNumbersRequestListValue ::
  Phase2DHGroupNumbersRequestListValue
newPhase2DHGroupNumbersRequestListValue =
  Phase2DHGroupNumbersRequestListValue'
    { value =
        Prelude.Nothing
    }

-- | The Diffie-Hellmann group number.
phase2DHGroupNumbersRequestListValue_value :: Lens.Lens' Phase2DHGroupNumbersRequestListValue (Prelude.Maybe Prelude.Int)
phase2DHGroupNumbersRequestListValue_value = Lens.lens (\Phase2DHGroupNumbersRequestListValue' {value} -> value) (\s@Phase2DHGroupNumbersRequestListValue' {} a -> s {value = a} :: Phase2DHGroupNumbersRequestListValue)

instance
  Prelude.Hashable
    Phase2DHGroupNumbersRequestListValue

instance
  Prelude.NFData
    Phase2DHGroupNumbersRequestListValue

instance
  Prelude.ToQuery
    Phase2DHGroupNumbersRequestListValue
  where
  toQuery Phase2DHGroupNumbersRequestListValue' {..} =
    Prelude.mconcat ["Value" Prelude.=: value]
