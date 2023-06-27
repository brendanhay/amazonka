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
-- Module      : Amazonka.SecurityHub.Types.AwsDynamoDbTableBillingModeSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsDynamoDbTableBillingModeSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the billing for read\/write capacity on the
-- table.
--
-- /See:/ 'newAwsDynamoDbTableBillingModeSummary' smart constructor.
data AwsDynamoDbTableBillingModeSummary = AwsDynamoDbTableBillingModeSummary'
  { -- | The method used to charge for read and write throughput and to manage
    -- capacity.
    billingMode :: Prelude.Maybe Prelude.Text,
    -- | If the billing mode is @PAY_PER_REQUEST@, indicates when the billing
    -- mode was set to that value.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces, and date and time should be separated
    -- by @T@. For example, @2020-03-22T13:22:13.933Z@.
    lastUpdateToPayPerRequestDateTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsDynamoDbTableBillingModeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'billingMode', 'awsDynamoDbTableBillingModeSummary_billingMode' - The method used to charge for read and write throughput and to manage
-- capacity.
--
-- 'lastUpdateToPayPerRequestDateTime', 'awsDynamoDbTableBillingModeSummary_lastUpdateToPayPerRequestDateTime' - If the billing mode is @PAY_PER_REQUEST@, indicates when the billing
-- mode was set to that value.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces, and date and time should be separated
-- by @T@. For example, @2020-03-22T13:22:13.933Z@.
newAwsDynamoDbTableBillingModeSummary ::
  AwsDynamoDbTableBillingModeSummary
newAwsDynamoDbTableBillingModeSummary =
  AwsDynamoDbTableBillingModeSummary'
    { billingMode =
        Prelude.Nothing,
      lastUpdateToPayPerRequestDateTime =
        Prelude.Nothing
    }

-- | The method used to charge for read and write throughput and to manage
-- capacity.
awsDynamoDbTableBillingModeSummary_billingMode :: Lens.Lens' AwsDynamoDbTableBillingModeSummary (Prelude.Maybe Prelude.Text)
awsDynamoDbTableBillingModeSummary_billingMode = Lens.lens (\AwsDynamoDbTableBillingModeSummary' {billingMode} -> billingMode) (\s@AwsDynamoDbTableBillingModeSummary' {} a -> s {billingMode = a} :: AwsDynamoDbTableBillingModeSummary)

-- | If the billing mode is @PAY_PER_REQUEST@, indicates when the billing
-- mode was set to that value.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces, and date and time should be separated
-- by @T@. For example, @2020-03-22T13:22:13.933Z@.
awsDynamoDbTableBillingModeSummary_lastUpdateToPayPerRequestDateTime :: Lens.Lens' AwsDynamoDbTableBillingModeSummary (Prelude.Maybe Prelude.Text)
awsDynamoDbTableBillingModeSummary_lastUpdateToPayPerRequestDateTime = Lens.lens (\AwsDynamoDbTableBillingModeSummary' {lastUpdateToPayPerRequestDateTime} -> lastUpdateToPayPerRequestDateTime) (\s@AwsDynamoDbTableBillingModeSummary' {} a -> s {lastUpdateToPayPerRequestDateTime = a} :: AwsDynamoDbTableBillingModeSummary)

instance
  Data.FromJSON
    AwsDynamoDbTableBillingModeSummary
  where
  parseJSON =
    Data.withObject
      "AwsDynamoDbTableBillingModeSummary"
      ( \x ->
          AwsDynamoDbTableBillingModeSummary'
            Prelude.<$> (x Data..:? "BillingMode")
            Prelude.<*> (x Data..:? "LastUpdateToPayPerRequestDateTime")
      )

instance
  Prelude.Hashable
    AwsDynamoDbTableBillingModeSummary
  where
  hashWithSalt
    _salt
    AwsDynamoDbTableBillingModeSummary' {..} =
      _salt
        `Prelude.hashWithSalt` billingMode
        `Prelude.hashWithSalt` lastUpdateToPayPerRequestDateTime

instance
  Prelude.NFData
    AwsDynamoDbTableBillingModeSummary
  where
  rnf AwsDynamoDbTableBillingModeSummary' {..} =
    Prelude.rnf billingMode
      `Prelude.seq` Prelude.rnf lastUpdateToPayPerRequestDateTime

instance
  Data.ToJSON
    AwsDynamoDbTableBillingModeSummary
  where
  toJSON AwsDynamoDbTableBillingModeSummary' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BillingMode" Data..=) Prelude.<$> billingMode,
            ("LastUpdateToPayPerRequestDateTime" Data..=)
              Prelude.<$> lastUpdateToPayPerRequestDateTime
          ]
      )
