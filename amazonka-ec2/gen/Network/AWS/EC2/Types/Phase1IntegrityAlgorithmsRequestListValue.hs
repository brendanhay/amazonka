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
-- Module      : Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsRequestListValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsRequestListValue where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the integrity algorithm for the VPN tunnel for phase 1 IKE
-- negotiations.
--
-- /See:/ 'newPhase1IntegrityAlgorithmsRequestListValue' smart constructor.
data Phase1IntegrityAlgorithmsRequestListValue = Phase1IntegrityAlgorithmsRequestListValue'
  { -- | The value for the integrity algorithm.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Phase1IntegrityAlgorithmsRequestListValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'phase1IntegrityAlgorithmsRequestListValue_value' - The value for the integrity algorithm.
newPhase1IntegrityAlgorithmsRequestListValue ::
  Phase1IntegrityAlgorithmsRequestListValue
newPhase1IntegrityAlgorithmsRequestListValue =
  Phase1IntegrityAlgorithmsRequestListValue'
    { value =
        Prelude.Nothing
    }

-- | The value for the integrity algorithm.
phase1IntegrityAlgorithmsRequestListValue_value :: Lens.Lens' Phase1IntegrityAlgorithmsRequestListValue (Prelude.Maybe Prelude.Text)
phase1IntegrityAlgorithmsRequestListValue_value = Lens.lens (\Phase1IntegrityAlgorithmsRequestListValue' {value} -> value) (\s@Phase1IntegrityAlgorithmsRequestListValue' {} a -> s {value = a} :: Phase1IntegrityAlgorithmsRequestListValue)

instance
  Prelude.Hashable
    Phase1IntegrityAlgorithmsRequestListValue

instance
  Prelude.NFData
    Phase1IntegrityAlgorithmsRequestListValue

instance
  Prelude.ToQuery
    Phase1IntegrityAlgorithmsRequestListValue
  where
  toQuery
    Phase1IntegrityAlgorithmsRequestListValue' {..} =
      Prelude.mconcat ["Value" Prelude.=: value]
