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
-- Module      : Amazonka.EC2.Types.Phase1IntegrityAlgorithmsListValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Phase1IntegrityAlgorithmsListValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | The integrity algorithm for phase 1 IKE negotiations.
--
-- /See:/ 'newPhase1IntegrityAlgorithmsListValue' smart constructor.
data Phase1IntegrityAlgorithmsListValue = Phase1IntegrityAlgorithmsListValue'
  { -- | The value for the integrity algorithm.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Data.FromXML
    Phase1IntegrityAlgorithmsListValue
  where
  parseXML x =
    Phase1IntegrityAlgorithmsListValue'
      Prelude.<$> (x Data..@? "value")

instance
  Prelude.Hashable
    Phase1IntegrityAlgorithmsListValue
  where
  hashWithSalt
    _salt
    Phase1IntegrityAlgorithmsListValue' {..} =
      _salt `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    Phase1IntegrityAlgorithmsListValue
  where
  rnf Phase1IntegrityAlgorithmsListValue' {..} =
    Prelude.rnf value
