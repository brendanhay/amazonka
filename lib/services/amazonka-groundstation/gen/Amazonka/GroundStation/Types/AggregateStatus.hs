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
-- Module      : Amazonka.GroundStation.Types.AggregateStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Types.AggregateStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.Types.AgentStatus
import qualified Amazonka.Prelude as Prelude

-- | Aggregate status of Agent components.
--
-- /See:/ 'newAggregateStatus' smart constructor.
data AggregateStatus = AggregateStatus'
  { -- | Sparse map of failure signatures.
    signatureMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Bool),
    -- | Aggregate status.
    status :: AgentStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregateStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'signatureMap', 'aggregateStatus_signatureMap' - Sparse map of failure signatures.
--
-- 'status', 'aggregateStatus_status' - Aggregate status.
newAggregateStatus ::
  -- | 'status'
  AgentStatus ->
  AggregateStatus
newAggregateStatus pStatus_ =
  AggregateStatus'
    { signatureMap = Prelude.Nothing,
      status = pStatus_
    }

-- | Sparse map of failure signatures.
aggregateStatus_signatureMap :: Lens.Lens' AggregateStatus (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Bool))
aggregateStatus_signatureMap = Lens.lens (\AggregateStatus' {signatureMap} -> signatureMap) (\s@AggregateStatus' {} a -> s {signatureMap = a} :: AggregateStatus) Prelude.. Lens.mapping Lens.coerced

-- | Aggregate status.
aggregateStatus_status :: Lens.Lens' AggregateStatus AgentStatus
aggregateStatus_status = Lens.lens (\AggregateStatus' {status} -> status) (\s@AggregateStatus' {} a -> s {status = a} :: AggregateStatus)

instance Prelude.Hashable AggregateStatus where
  hashWithSalt _salt AggregateStatus' {..} =
    _salt
      `Prelude.hashWithSalt` signatureMap
      `Prelude.hashWithSalt` status

instance Prelude.NFData AggregateStatus where
  rnf AggregateStatus' {..} =
    Prelude.rnf signatureMap
      `Prelude.seq` Prelude.rnf status

instance Data.ToJSON AggregateStatus where
  toJSON AggregateStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("signatureMap" Data..=) Prelude.<$> signatureMap,
            Prelude.Just ("status" Data..= status)
          ]
      )
