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
-- Module      : Amazonka.Config.Types.FailedDeleteRemediationExceptionsBatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Config.Types.FailedDeleteRemediationExceptionsBatch where

import Amazonka.Config.Types.RemediationExceptionResourceKey
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | List of each of the failed delete remediation exceptions with specific
-- reasons.
--
-- /See:/ 'newFailedDeleteRemediationExceptionsBatch' smart constructor.
data FailedDeleteRemediationExceptionsBatch = FailedDeleteRemediationExceptionsBatch'
  { -- | Returns remediation exception resource key object of the failed items.
    failedItems :: Prelude.Maybe (Prelude.NonEmpty RemediationExceptionResourceKey),
    -- | Returns a failure message for delete remediation exception. For example,
    -- Config creates an exception due to an internal error.
    failureMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailedDeleteRemediationExceptionsBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedItems', 'failedDeleteRemediationExceptionsBatch_failedItems' - Returns remediation exception resource key object of the failed items.
--
-- 'failureMessage', 'failedDeleteRemediationExceptionsBatch_failureMessage' - Returns a failure message for delete remediation exception. For example,
-- Config creates an exception due to an internal error.
newFailedDeleteRemediationExceptionsBatch ::
  FailedDeleteRemediationExceptionsBatch
newFailedDeleteRemediationExceptionsBatch =
  FailedDeleteRemediationExceptionsBatch'
    { failedItems =
        Prelude.Nothing,
      failureMessage = Prelude.Nothing
    }

-- | Returns remediation exception resource key object of the failed items.
failedDeleteRemediationExceptionsBatch_failedItems :: Lens.Lens' FailedDeleteRemediationExceptionsBatch (Prelude.Maybe (Prelude.NonEmpty RemediationExceptionResourceKey))
failedDeleteRemediationExceptionsBatch_failedItems = Lens.lens (\FailedDeleteRemediationExceptionsBatch' {failedItems} -> failedItems) (\s@FailedDeleteRemediationExceptionsBatch' {} a -> s {failedItems = a} :: FailedDeleteRemediationExceptionsBatch) Prelude.. Lens.mapping Lens.coerced

-- | Returns a failure message for delete remediation exception. For example,
-- Config creates an exception due to an internal error.
failedDeleteRemediationExceptionsBatch_failureMessage :: Lens.Lens' FailedDeleteRemediationExceptionsBatch (Prelude.Maybe Prelude.Text)
failedDeleteRemediationExceptionsBatch_failureMessage = Lens.lens (\FailedDeleteRemediationExceptionsBatch' {failureMessage} -> failureMessage) (\s@FailedDeleteRemediationExceptionsBatch' {} a -> s {failureMessage = a} :: FailedDeleteRemediationExceptionsBatch)

instance
  Data.FromJSON
    FailedDeleteRemediationExceptionsBatch
  where
  parseJSON =
    Data.withObject
      "FailedDeleteRemediationExceptionsBatch"
      ( \x ->
          FailedDeleteRemediationExceptionsBatch'
            Prelude.<$> (x Data..:? "FailedItems")
            Prelude.<*> (x Data..:? "FailureMessage")
      )

instance
  Prelude.Hashable
    FailedDeleteRemediationExceptionsBatch
  where
  hashWithSalt
    _salt
    FailedDeleteRemediationExceptionsBatch' {..} =
      _salt `Prelude.hashWithSalt` failedItems
        `Prelude.hashWithSalt` failureMessage

instance
  Prelude.NFData
    FailedDeleteRemediationExceptionsBatch
  where
  rnf FailedDeleteRemediationExceptionsBatch' {..} =
    Prelude.rnf failedItems
      `Prelude.seq` Prelude.rnf failureMessage
