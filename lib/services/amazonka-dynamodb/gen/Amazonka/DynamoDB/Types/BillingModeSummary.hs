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
-- Module      : Amazonka.DynamoDB.Types.BillingModeSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Types.BillingModeSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.Types.AttributeValue
import Amazonka.DynamoDB.Types.BillingMode
import Amazonka.DynamoDB.Types.WriteRequest
import qualified Amazonka.Prelude as Prelude

-- | Contains the details for the read\/write capacity mode. This page talks
-- about @PROVISIONED@ and @PAY_PER_REQUEST@ billing modes. For more
-- information about these modes, see
-- <https://docs.aws.amazon.com/amazondynamodb/latest/developerguide/HowItWorks.ReadWriteCapacityMode.html Read\/write capacity mode>.
--
-- You may need to switch to on-demand mode at least once in order to
-- return a @BillingModeSummary@ response.
--
-- /See:/ 'newBillingModeSummary' smart constructor.
data BillingModeSummary = BillingModeSummary'
  { -- | Controls how you are charged for read and write throughput and how you
    -- manage capacity. This setting can be changed later.
    --
    -- -   @PROVISIONED@ - Sets the read\/write capacity mode to @PROVISIONED@.
    --     We recommend using @PROVISIONED@ for predictable workloads.
    --
    -- -   @PAY_PER_REQUEST@ - Sets the read\/write capacity mode to
    --     @PAY_PER_REQUEST@. We recommend using @PAY_PER_REQUEST@ for
    --     unpredictable workloads.
    billingMode :: Prelude.Maybe BillingMode,
    -- | Represents the time when @PAY_PER_REQUEST@ was last set as the
    -- read\/write capacity mode.
    lastUpdateToPayPerRequestDateTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BillingModeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingMode', 'billingModeSummary_billingMode' - Controls how you are charged for read and write throughput and how you
-- manage capacity. This setting can be changed later.
--
-- -   @PROVISIONED@ - Sets the read\/write capacity mode to @PROVISIONED@.
--     We recommend using @PROVISIONED@ for predictable workloads.
--
-- -   @PAY_PER_REQUEST@ - Sets the read\/write capacity mode to
--     @PAY_PER_REQUEST@. We recommend using @PAY_PER_REQUEST@ for
--     unpredictable workloads.
--
-- 'lastUpdateToPayPerRequestDateTime', 'billingModeSummary_lastUpdateToPayPerRequestDateTime' - Represents the time when @PAY_PER_REQUEST@ was last set as the
-- read\/write capacity mode.
newBillingModeSummary ::
  BillingModeSummary
newBillingModeSummary =
  BillingModeSummary'
    { billingMode = Prelude.Nothing,
      lastUpdateToPayPerRequestDateTime = Prelude.Nothing
    }

-- | Controls how you are charged for read and write throughput and how you
-- manage capacity. This setting can be changed later.
--
-- -   @PROVISIONED@ - Sets the read\/write capacity mode to @PROVISIONED@.
--     We recommend using @PROVISIONED@ for predictable workloads.
--
-- -   @PAY_PER_REQUEST@ - Sets the read\/write capacity mode to
--     @PAY_PER_REQUEST@. We recommend using @PAY_PER_REQUEST@ for
--     unpredictable workloads.
billingModeSummary_billingMode :: Lens.Lens' BillingModeSummary (Prelude.Maybe BillingMode)
billingModeSummary_billingMode = Lens.lens (\BillingModeSummary' {billingMode} -> billingMode) (\s@BillingModeSummary' {} a -> s {billingMode = a} :: BillingModeSummary)

-- | Represents the time when @PAY_PER_REQUEST@ was last set as the
-- read\/write capacity mode.
billingModeSummary_lastUpdateToPayPerRequestDateTime :: Lens.Lens' BillingModeSummary (Prelude.Maybe Prelude.UTCTime)
billingModeSummary_lastUpdateToPayPerRequestDateTime = Lens.lens (\BillingModeSummary' {lastUpdateToPayPerRequestDateTime} -> lastUpdateToPayPerRequestDateTime) (\s@BillingModeSummary' {} a -> s {lastUpdateToPayPerRequestDateTime = a} :: BillingModeSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON BillingModeSummary where
  parseJSON =
    Data.withObject
      "BillingModeSummary"
      ( \x ->
          BillingModeSummary'
            Prelude.<$> (x Data..:? "BillingMode")
            Prelude.<*> (x Data..:? "LastUpdateToPayPerRequestDateTime")
      )

instance Prelude.Hashable BillingModeSummary where
  hashWithSalt _salt BillingModeSummary' {..} =
    _salt `Prelude.hashWithSalt` billingMode
      `Prelude.hashWithSalt` lastUpdateToPayPerRequestDateTime

instance Prelude.NFData BillingModeSummary where
  rnf BillingModeSummary' {..} =
    Prelude.rnf billingMode
      `Prelude.seq` Prelude.rnf lastUpdateToPayPerRequestDateTime
