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
-- Module      : Network.AWS.Config.Types.FailedDeleteRemediationExceptionsBatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.FailedDeleteRemediationExceptionsBatch where

import Network.AWS.Config.Types.RemediationExceptionResourceKey
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | List of each of the failed delete remediation exceptions with specific
-- reasons.
--
-- /See:/ 'newFailedDeleteRemediationExceptionsBatch' smart constructor.
data FailedDeleteRemediationExceptionsBatch = FailedDeleteRemediationExceptionsBatch'
  { -- | Returns a failure message for delete remediation exception. For example,
    -- Config creates an exception due to an internal error.
    failureMessage :: Prelude.Maybe Prelude.Text,
    -- | Returns remediation exception resource key object of the failed items.
    failedItems :: Prelude.Maybe (Prelude.NonEmpty RemediationExceptionResourceKey)
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
-- 'failureMessage', 'failedDeleteRemediationExceptionsBatch_failureMessage' - Returns a failure message for delete remediation exception. For example,
-- Config creates an exception due to an internal error.
--
-- 'failedItems', 'failedDeleteRemediationExceptionsBatch_failedItems' - Returns remediation exception resource key object of the failed items.
newFailedDeleteRemediationExceptionsBatch ::
  FailedDeleteRemediationExceptionsBatch
newFailedDeleteRemediationExceptionsBatch =
  FailedDeleteRemediationExceptionsBatch'
    { failureMessage =
        Prelude.Nothing,
      failedItems = Prelude.Nothing
    }

-- | Returns a failure message for delete remediation exception. For example,
-- Config creates an exception due to an internal error.
failedDeleteRemediationExceptionsBatch_failureMessage :: Lens.Lens' FailedDeleteRemediationExceptionsBatch (Prelude.Maybe Prelude.Text)
failedDeleteRemediationExceptionsBatch_failureMessage = Lens.lens (\FailedDeleteRemediationExceptionsBatch' {failureMessage} -> failureMessage) (\s@FailedDeleteRemediationExceptionsBatch' {} a -> s {failureMessage = a} :: FailedDeleteRemediationExceptionsBatch)

-- | Returns remediation exception resource key object of the failed items.
failedDeleteRemediationExceptionsBatch_failedItems :: Lens.Lens' FailedDeleteRemediationExceptionsBatch (Prelude.Maybe (Prelude.NonEmpty RemediationExceptionResourceKey))
failedDeleteRemediationExceptionsBatch_failedItems = Lens.lens (\FailedDeleteRemediationExceptionsBatch' {failedItems} -> failedItems) (\s@FailedDeleteRemediationExceptionsBatch' {} a -> s {failedItems = a} :: FailedDeleteRemediationExceptionsBatch) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    FailedDeleteRemediationExceptionsBatch
  where
  parseJSON =
    Core.withObject
      "FailedDeleteRemediationExceptionsBatch"
      ( \x ->
          FailedDeleteRemediationExceptionsBatch'
            Prelude.<$> (x Core..:? "FailureMessage")
            Prelude.<*> (x Core..:? "FailedItems")
      )

instance
  Prelude.Hashable
    FailedDeleteRemediationExceptionsBatch

instance
  Prelude.NFData
    FailedDeleteRemediationExceptionsBatch
