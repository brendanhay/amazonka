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
-- Module      : Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsRequestListValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase2IntegrityAlgorithmsRequestListValue where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the integrity algorithm for the VPN tunnel for phase 2 IKE
-- negotiations.
--
-- /See:/ 'newPhase2IntegrityAlgorithmsRequestListValue' smart constructor.
data Phase2IntegrityAlgorithmsRequestListValue = Phase2IntegrityAlgorithmsRequestListValue'
  { -- | The integrity algorithm.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Phase2IntegrityAlgorithmsRequestListValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'phase2IntegrityAlgorithmsRequestListValue_value' - The integrity algorithm.
newPhase2IntegrityAlgorithmsRequestListValue ::
  Phase2IntegrityAlgorithmsRequestListValue
newPhase2IntegrityAlgorithmsRequestListValue =
  Phase2IntegrityAlgorithmsRequestListValue'
    { value =
        Prelude.Nothing
    }

-- | The integrity algorithm.
phase2IntegrityAlgorithmsRequestListValue_value :: Lens.Lens' Phase2IntegrityAlgorithmsRequestListValue (Prelude.Maybe Prelude.Text)
phase2IntegrityAlgorithmsRequestListValue_value = Lens.lens (\Phase2IntegrityAlgorithmsRequestListValue' {value} -> value) (\s@Phase2IntegrityAlgorithmsRequestListValue' {} a -> s {value = a} :: Phase2IntegrityAlgorithmsRequestListValue)

instance
  Prelude.Hashable
    Phase2IntegrityAlgorithmsRequestListValue

instance
  Prelude.NFData
    Phase2IntegrityAlgorithmsRequestListValue

instance
  Prelude.ToQuery
    Phase2IntegrityAlgorithmsRequestListValue
  where
  toQuery
    Phase2IntegrityAlgorithmsRequestListValue' {..} =
      Prelude.mconcat ["Value" Prelude.=: value]
