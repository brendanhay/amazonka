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
-- Module      : Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsListValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Phase1IntegrityAlgorithmsListValue where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The integrity algorithm for phase 1 IKE negotiations.
--
-- /See:/ 'newPhase1IntegrityAlgorithmsListValue' smart constructor.
data Phase1IntegrityAlgorithmsListValue = Phase1IntegrityAlgorithmsListValue'
  { -- | The value for the integrity algorithm.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Phase1IntegrityAlgorithmsListValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'phase1IntegrityAlgorithmsListValue_value' - The value for the integrity algorithm.
newPhase1IntegrityAlgorithmsListValue ::
  Phase1IntegrityAlgorithmsListValue
newPhase1IntegrityAlgorithmsListValue =
  Phase1IntegrityAlgorithmsListValue'
    { value =
        Prelude.Nothing
    }

-- | The value for the integrity algorithm.
phase1IntegrityAlgorithmsListValue_value :: Lens.Lens' Phase1IntegrityAlgorithmsListValue (Prelude.Maybe Prelude.Text)
phase1IntegrityAlgorithmsListValue_value = Lens.lens (\Phase1IntegrityAlgorithmsListValue' {value} -> value) (\s@Phase1IntegrityAlgorithmsListValue' {} a -> s {value = a} :: Phase1IntegrityAlgorithmsListValue)

instance
  Prelude.FromXML
    Phase1IntegrityAlgorithmsListValue
  where
  parseXML x =
    Phase1IntegrityAlgorithmsListValue'
      Prelude.<$> (x Prelude..@? "value")

instance
  Prelude.Hashable
    Phase1IntegrityAlgorithmsListValue

instance
  Prelude.NFData
    Phase1IntegrityAlgorithmsListValue
