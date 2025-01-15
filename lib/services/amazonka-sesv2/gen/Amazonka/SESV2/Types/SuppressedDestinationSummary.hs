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
-- Module      : Amazonka.SESV2.Types.SuppressedDestinationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.SuppressedDestinationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.SuppressionListReason

-- | A summary that describes the suppressed email address.
--
-- /See:/ 'newSuppressedDestinationSummary' smart constructor.
data SuppressedDestinationSummary = SuppressedDestinationSummary'
  { -- | The email address that\'s on the suppression list for your account.
    emailAddress :: Prelude.Text,
    -- | The reason that the address was added to the suppression list for your
    -- account.
    reason :: SuppressionListReason,
    -- | The date and time when the suppressed destination was last updated,
    -- shown in Unix time format.
    lastUpdateTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SuppressedDestinationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailAddress', 'suppressedDestinationSummary_emailAddress' - The email address that\'s on the suppression list for your account.
--
-- 'reason', 'suppressedDestinationSummary_reason' - The reason that the address was added to the suppression list for your
-- account.
--
-- 'lastUpdateTime', 'suppressedDestinationSummary_lastUpdateTime' - The date and time when the suppressed destination was last updated,
-- shown in Unix time format.
newSuppressedDestinationSummary ::
  -- | 'emailAddress'
  Prelude.Text ->
  -- | 'reason'
  SuppressionListReason ->
  -- | 'lastUpdateTime'
  Prelude.UTCTime ->
  SuppressedDestinationSummary
newSuppressedDestinationSummary
  pEmailAddress_
  pReason_
  pLastUpdateTime_ =
    SuppressedDestinationSummary'
      { emailAddress =
          pEmailAddress_,
        reason = pReason_,
        lastUpdateTime =
          Data._Time Lens.# pLastUpdateTime_
      }

-- | The email address that\'s on the suppression list for your account.
suppressedDestinationSummary_emailAddress :: Lens.Lens' SuppressedDestinationSummary Prelude.Text
suppressedDestinationSummary_emailAddress = Lens.lens (\SuppressedDestinationSummary' {emailAddress} -> emailAddress) (\s@SuppressedDestinationSummary' {} a -> s {emailAddress = a} :: SuppressedDestinationSummary)

-- | The reason that the address was added to the suppression list for your
-- account.
suppressedDestinationSummary_reason :: Lens.Lens' SuppressedDestinationSummary SuppressionListReason
suppressedDestinationSummary_reason = Lens.lens (\SuppressedDestinationSummary' {reason} -> reason) (\s@SuppressedDestinationSummary' {} a -> s {reason = a} :: SuppressedDestinationSummary)

-- | The date and time when the suppressed destination was last updated,
-- shown in Unix time format.
suppressedDestinationSummary_lastUpdateTime :: Lens.Lens' SuppressedDestinationSummary Prelude.UTCTime
suppressedDestinationSummary_lastUpdateTime = Lens.lens (\SuppressedDestinationSummary' {lastUpdateTime} -> lastUpdateTime) (\s@SuppressedDestinationSummary' {} a -> s {lastUpdateTime = a} :: SuppressedDestinationSummary) Prelude.. Data._Time

instance Data.FromJSON SuppressedDestinationSummary where
  parseJSON =
    Data.withObject
      "SuppressedDestinationSummary"
      ( \x ->
          SuppressedDestinationSummary'
            Prelude.<$> (x Data..: "EmailAddress")
            Prelude.<*> (x Data..: "Reason")
            Prelude.<*> (x Data..: "LastUpdateTime")
      )

instance
  Prelude.Hashable
    SuppressedDestinationSummary
  where
  hashWithSalt _salt SuppressedDestinationSummary' {..} =
    _salt
      `Prelude.hashWithSalt` emailAddress
      `Prelude.hashWithSalt` reason
      `Prelude.hashWithSalt` lastUpdateTime

instance Prelude.NFData SuppressedDestinationSummary where
  rnf SuppressedDestinationSummary' {..} =
    Prelude.rnf emailAddress `Prelude.seq`
      Prelude.rnf reason `Prelude.seq`
        Prelude.rnf lastUpdateTime
