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
-- Module      : Amazonka.Support.Types.TrustedAdvisorCheckRefreshStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Support.Types.TrustedAdvisorCheckRefreshStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The refresh status of a Trusted Advisor check.
--
-- /See:/ 'newTrustedAdvisorCheckRefreshStatus' smart constructor.
data TrustedAdvisorCheckRefreshStatus = TrustedAdvisorCheckRefreshStatus'
  { -- | The unique identifier for the Trusted Advisor check.
    checkId :: Prelude.Text,
    -- | The status of the Trusted Advisor check for which a refresh has been
    -- requested:
    --
    -- -   @none@ - The check is not refreshed or the non-success status
    --     exceeds the timeout
    --
    -- -   @enqueued@ - The check refresh requests has entered the refresh
    --     queue
    --
    -- -   @processing@ - The check refresh request is picked up by the rule
    --     processing engine
    --
    -- -   @success@ - The check is successfully refreshed
    --
    -- -   @abandoned@ - The check refresh has failed
    status :: Prelude.Text,
    -- | The amount of time, in milliseconds, until the Trusted Advisor check is
    -- eligible for refresh.
    millisUntilNextRefreshable :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TrustedAdvisorCheckRefreshStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checkId', 'trustedAdvisorCheckRefreshStatus_checkId' - The unique identifier for the Trusted Advisor check.
--
-- 'status', 'trustedAdvisorCheckRefreshStatus_status' - The status of the Trusted Advisor check for which a refresh has been
-- requested:
--
-- -   @none@ - The check is not refreshed or the non-success status
--     exceeds the timeout
--
-- -   @enqueued@ - The check refresh requests has entered the refresh
--     queue
--
-- -   @processing@ - The check refresh request is picked up by the rule
--     processing engine
--
-- -   @success@ - The check is successfully refreshed
--
-- -   @abandoned@ - The check refresh has failed
--
-- 'millisUntilNextRefreshable', 'trustedAdvisorCheckRefreshStatus_millisUntilNextRefreshable' - The amount of time, in milliseconds, until the Trusted Advisor check is
-- eligible for refresh.
newTrustedAdvisorCheckRefreshStatus ::
  -- | 'checkId'
  Prelude.Text ->
  -- | 'status'
  Prelude.Text ->
  -- | 'millisUntilNextRefreshable'
  Prelude.Integer ->
  TrustedAdvisorCheckRefreshStatus
newTrustedAdvisorCheckRefreshStatus
  pCheckId_
  pStatus_
  pMillisUntilNextRefreshable_ =
    TrustedAdvisorCheckRefreshStatus'
      { checkId =
          pCheckId_,
        status = pStatus_,
        millisUntilNextRefreshable =
          pMillisUntilNextRefreshable_
      }

-- | The unique identifier for the Trusted Advisor check.
trustedAdvisorCheckRefreshStatus_checkId :: Lens.Lens' TrustedAdvisorCheckRefreshStatus Prelude.Text
trustedAdvisorCheckRefreshStatus_checkId = Lens.lens (\TrustedAdvisorCheckRefreshStatus' {checkId} -> checkId) (\s@TrustedAdvisorCheckRefreshStatus' {} a -> s {checkId = a} :: TrustedAdvisorCheckRefreshStatus)

-- | The status of the Trusted Advisor check for which a refresh has been
-- requested:
--
-- -   @none@ - The check is not refreshed or the non-success status
--     exceeds the timeout
--
-- -   @enqueued@ - The check refresh requests has entered the refresh
--     queue
--
-- -   @processing@ - The check refresh request is picked up by the rule
--     processing engine
--
-- -   @success@ - The check is successfully refreshed
--
-- -   @abandoned@ - The check refresh has failed
trustedAdvisorCheckRefreshStatus_status :: Lens.Lens' TrustedAdvisorCheckRefreshStatus Prelude.Text
trustedAdvisorCheckRefreshStatus_status = Lens.lens (\TrustedAdvisorCheckRefreshStatus' {status} -> status) (\s@TrustedAdvisorCheckRefreshStatus' {} a -> s {status = a} :: TrustedAdvisorCheckRefreshStatus)

-- | The amount of time, in milliseconds, until the Trusted Advisor check is
-- eligible for refresh.
trustedAdvisorCheckRefreshStatus_millisUntilNextRefreshable :: Lens.Lens' TrustedAdvisorCheckRefreshStatus Prelude.Integer
trustedAdvisorCheckRefreshStatus_millisUntilNextRefreshable = Lens.lens (\TrustedAdvisorCheckRefreshStatus' {millisUntilNextRefreshable} -> millisUntilNextRefreshable) (\s@TrustedAdvisorCheckRefreshStatus' {} a -> s {millisUntilNextRefreshable = a} :: TrustedAdvisorCheckRefreshStatus)

instance
  Data.FromJSON
    TrustedAdvisorCheckRefreshStatus
  where
  parseJSON =
    Data.withObject
      "TrustedAdvisorCheckRefreshStatus"
      ( \x ->
          TrustedAdvisorCheckRefreshStatus'
            Prelude.<$> (x Data..: "checkId")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "millisUntilNextRefreshable")
      )

instance
  Prelude.Hashable
    TrustedAdvisorCheckRefreshStatus
  where
  hashWithSalt
    _salt
    TrustedAdvisorCheckRefreshStatus' {..} =
      _salt
        `Prelude.hashWithSalt` checkId
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` millisUntilNextRefreshable

instance
  Prelude.NFData
    TrustedAdvisorCheckRefreshStatus
  where
  rnf TrustedAdvisorCheckRefreshStatus' {..} =
    Prelude.rnf checkId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf millisUntilNextRefreshable
