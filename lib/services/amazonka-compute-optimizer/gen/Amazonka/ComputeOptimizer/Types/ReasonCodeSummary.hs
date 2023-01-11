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
-- Module      : Amazonka.ComputeOptimizer.Types.ReasonCodeSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ComputeOptimizer.Types.ReasonCodeSummary where

import Amazonka.ComputeOptimizer.Types.FindingReasonCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A summary of a finding reason code.
--
-- /See:/ 'newReasonCodeSummary' smart constructor.
data ReasonCodeSummary = ReasonCodeSummary'
  { -- | The name of the finding reason code.
    name :: Prelude.Maybe FindingReasonCode,
    -- | The value of the finding reason code summary.
    value :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReasonCodeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'reasonCodeSummary_name' - The name of the finding reason code.
--
-- 'value', 'reasonCodeSummary_value' - The value of the finding reason code summary.
newReasonCodeSummary ::
  ReasonCodeSummary
newReasonCodeSummary =
  ReasonCodeSummary'
    { name = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the finding reason code.
reasonCodeSummary_name :: Lens.Lens' ReasonCodeSummary (Prelude.Maybe FindingReasonCode)
reasonCodeSummary_name = Lens.lens (\ReasonCodeSummary' {name} -> name) (\s@ReasonCodeSummary' {} a -> s {name = a} :: ReasonCodeSummary)

-- | The value of the finding reason code summary.
reasonCodeSummary_value :: Lens.Lens' ReasonCodeSummary (Prelude.Maybe Prelude.Double)
reasonCodeSummary_value = Lens.lens (\ReasonCodeSummary' {value} -> value) (\s@ReasonCodeSummary' {} a -> s {value = a} :: ReasonCodeSummary)

instance Data.FromJSON ReasonCodeSummary where
  parseJSON =
    Data.withObject
      "ReasonCodeSummary"
      ( \x ->
          ReasonCodeSummary'
            Prelude.<$> (x Data..:? "name") Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable ReasonCodeSummary where
  hashWithSalt _salt ReasonCodeSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` value

instance Prelude.NFData ReasonCodeSummary where
  rnf ReasonCodeSummary' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf value
