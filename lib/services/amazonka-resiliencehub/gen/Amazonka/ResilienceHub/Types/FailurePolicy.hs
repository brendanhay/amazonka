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
-- Module      : Amazonka.ResilienceHub.Types.FailurePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ResilienceHub.Types.FailurePolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Defines a failure policy.
--
-- /See:/ 'newFailurePolicy' smart constructor.
data FailurePolicy = FailurePolicy'
  { -- | The Recovery Point Objective (RPO), in seconds.
    rpoInSecs :: Prelude.Natural,
    -- | The Recovery Time Objective (RTO), in seconds.
    rtoInSecs :: Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailurePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rpoInSecs', 'failurePolicy_rpoInSecs' - The Recovery Point Objective (RPO), in seconds.
--
-- 'rtoInSecs', 'failurePolicy_rtoInSecs' - The Recovery Time Objective (RTO), in seconds.
newFailurePolicy ::
  -- | 'rpoInSecs'
  Prelude.Natural ->
  -- | 'rtoInSecs'
  Prelude.Natural ->
  FailurePolicy
newFailurePolicy pRpoInSecs_ pRtoInSecs_ =
  FailurePolicy'
    { rpoInSecs = pRpoInSecs_,
      rtoInSecs = pRtoInSecs_
    }

-- | The Recovery Point Objective (RPO), in seconds.
failurePolicy_rpoInSecs :: Lens.Lens' FailurePolicy Prelude.Natural
failurePolicy_rpoInSecs = Lens.lens (\FailurePolicy' {rpoInSecs} -> rpoInSecs) (\s@FailurePolicy' {} a -> s {rpoInSecs = a} :: FailurePolicy)

-- | The Recovery Time Objective (RTO), in seconds.
failurePolicy_rtoInSecs :: Lens.Lens' FailurePolicy Prelude.Natural
failurePolicy_rtoInSecs = Lens.lens (\FailurePolicy' {rtoInSecs} -> rtoInSecs) (\s@FailurePolicy' {} a -> s {rtoInSecs = a} :: FailurePolicy)

instance Data.FromJSON FailurePolicy where
  parseJSON =
    Data.withObject
      "FailurePolicy"
      ( \x ->
          FailurePolicy'
            Prelude.<$> (x Data..: "rpoInSecs")
            Prelude.<*> (x Data..: "rtoInSecs")
      )

instance Prelude.Hashable FailurePolicy where
  hashWithSalt _salt FailurePolicy' {..} =
    _salt
      `Prelude.hashWithSalt` rpoInSecs
      `Prelude.hashWithSalt` rtoInSecs

instance Prelude.NFData FailurePolicy where
  rnf FailurePolicy' {..} =
    Prelude.rnf rpoInSecs
      `Prelude.seq` Prelude.rnf rtoInSecs

instance Data.ToJSON FailurePolicy where
  toJSON FailurePolicy' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("rpoInSecs" Data..= rpoInSecs),
            Prelude.Just ("rtoInSecs" Data..= rtoInSecs)
          ]
      )
