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
-- Module      : Amazonka.Config.Types.PendingAggregationRequest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.PendingAggregationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | An object that represents the account ID and region of an aggregator
-- account that is requesting authorization but is not yet authorized.
--
-- /See:/ 'newPendingAggregationRequest' smart constructor.
data PendingAggregationRequest = PendingAggregationRequest'
  { -- | The 12-digit account ID of the account requesting to aggregate data.
    requesterAccountId :: Prelude.Maybe Prelude.Text,
    -- | The region requesting to aggregate data.
    requesterAwsRegion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PendingAggregationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requesterAccountId', 'pendingAggregationRequest_requesterAccountId' - The 12-digit account ID of the account requesting to aggregate data.
--
-- 'requesterAwsRegion', 'pendingAggregationRequest_requesterAwsRegion' - The region requesting to aggregate data.
newPendingAggregationRequest ::
  PendingAggregationRequest
newPendingAggregationRequest =
  PendingAggregationRequest'
    { requesterAccountId =
        Prelude.Nothing,
      requesterAwsRegion = Prelude.Nothing
    }

-- | The 12-digit account ID of the account requesting to aggregate data.
pendingAggregationRequest_requesterAccountId :: Lens.Lens' PendingAggregationRequest (Prelude.Maybe Prelude.Text)
pendingAggregationRequest_requesterAccountId = Lens.lens (\PendingAggregationRequest' {requesterAccountId} -> requesterAccountId) (\s@PendingAggregationRequest' {} a -> s {requesterAccountId = a} :: PendingAggregationRequest)

-- | The region requesting to aggregate data.
pendingAggregationRequest_requesterAwsRegion :: Lens.Lens' PendingAggregationRequest (Prelude.Maybe Prelude.Text)
pendingAggregationRequest_requesterAwsRegion = Lens.lens (\PendingAggregationRequest' {requesterAwsRegion} -> requesterAwsRegion) (\s@PendingAggregationRequest' {} a -> s {requesterAwsRegion = a} :: PendingAggregationRequest)

instance Data.FromJSON PendingAggregationRequest where
  parseJSON =
    Data.withObject
      "PendingAggregationRequest"
      ( \x ->
          PendingAggregationRequest'
            Prelude.<$> (x Data..:? "RequesterAccountId")
            Prelude.<*> (x Data..:? "RequesterAwsRegion")
      )

instance Prelude.Hashable PendingAggregationRequest where
  hashWithSalt _salt PendingAggregationRequest' {..} =
    _salt
      `Prelude.hashWithSalt` requesterAccountId
      `Prelude.hashWithSalt` requesterAwsRegion

instance Prelude.NFData PendingAggregationRequest where
  rnf PendingAggregationRequest' {..} =
    Prelude.rnf requesterAccountId `Prelude.seq`
      Prelude.rnf requesterAwsRegion
