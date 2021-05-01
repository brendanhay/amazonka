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
-- Module      : Network.AWS.Config.Types.PendingAggregationRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.PendingAggregationRequest where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that represents the account ID and region of an aggregator
-- account that is requesting authorization but is not yet authorized.
--
-- /See:/ 'newPendingAggregationRequest' smart constructor.
data PendingAggregationRequest = PendingAggregationRequest'
  { -- | The region requesting to aggregate data.
    requesterAwsRegion :: Prelude.Maybe Prelude.Text,
    -- | The 12-digit account ID of the account requesting to aggregate data.
    requesterAccountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PendingAggregationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requesterAwsRegion', 'pendingAggregationRequest_requesterAwsRegion' - The region requesting to aggregate data.
--
-- 'requesterAccountId', 'pendingAggregationRequest_requesterAccountId' - The 12-digit account ID of the account requesting to aggregate data.
newPendingAggregationRequest ::
  PendingAggregationRequest
newPendingAggregationRequest =
  PendingAggregationRequest'
    { requesterAwsRegion =
        Prelude.Nothing,
      requesterAccountId = Prelude.Nothing
    }

-- | The region requesting to aggregate data.
pendingAggregationRequest_requesterAwsRegion :: Lens.Lens' PendingAggregationRequest (Prelude.Maybe Prelude.Text)
pendingAggregationRequest_requesterAwsRegion = Lens.lens (\PendingAggregationRequest' {requesterAwsRegion} -> requesterAwsRegion) (\s@PendingAggregationRequest' {} a -> s {requesterAwsRegion = a} :: PendingAggregationRequest)

-- | The 12-digit account ID of the account requesting to aggregate data.
pendingAggregationRequest_requesterAccountId :: Lens.Lens' PendingAggregationRequest (Prelude.Maybe Prelude.Text)
pendingAggregationRequest_requesterAccountId = Lens.lens (\PendingAggregationRequest' {requesterAccountId} -> requesterAccountId) (\s@PendingAggregationRequest' {} a -> s {requesterAccountId = a} :: PendingAggregationRequest)

instance Prelude.FromJSON PendingAggregationRequest where
  parseJSON =
    Prelude.withObject
      "PendingAggregationRequest"
      ( \x ->
          PendingAggregationRequest'
            Prelude.<$> (x Prelude..:? "RequesterAwsRegion")
            Prelude.<*> (x Prelude..:? "RequesterAccountId")
      )

instance Prelude.Hashable PendingAggregationRequest

instance Prelude.NFData PendingAggregationRequest
