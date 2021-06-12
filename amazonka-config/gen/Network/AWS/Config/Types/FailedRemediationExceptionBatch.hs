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
-- Module      : Network.AWS.Config.Types.FailedRemediationExceptionBatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.FailedRemediationExceptionBatch where

import Network.AWS.Config.Types.RemediationException
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | List of each of the failed remediation exceptions with specific reasons.
--
-- /See:/ 'newFailedRemediationExceptionBatch' smart constructor.
data FailedRemediationExceptionBatch = FailedRemediationExceptionBatch'
  { -- | Returns a failure message. For example, the auto-remediation has failed.
    failureMessage :: Core.Maybe Core.Text,
    -- | Returns remediation exception resource key object of the failed items.
    failedItems :: Core.Maybe [RemediationException]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FailedRemediationExceptionBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureMessage', 'failedRemediationExceptionBatch_failureMessage' - Returns a failure message. For example, the auto-remediation has failed.
--
-- 'failedItems', 'failedRemediationExceptionBatch_failedItems' - Returns remediation exception resource key object of the failed items.
newFailedRemediationExceptionBatch ::
  FailedRemediationExceptionBatch
newFailedRemediationExceptionBatch =
  FailedRemediationExceptionBatch'
    { failureMessage =
        Core.Nothing,
      failedItems = Core.Nothing
    }

-- | Returns a failure message. For example, the auto-remediation has failed.
failedRemediationExceptionBatch_failureMessage :: Lens.Lens' FailedRemediationExceptionBatch (Core.Maybe Core.Text)
failedRemediationExceptionBatch_failureMessage = Lens.lens (\FailedRemediationExceptionBatch' {failureMessage} -> failureMessage) (\s@FailedRemediationExceptionBatch' {} a -> s {failureMessage = a} :: FailedRemediationExceptionBatch)

-- | Returns remediation exception resource key object of the failed items.
failedRemediationExceptionBatch_failedItems :: Lens.Lens' FailedRemediationExceptionBatch (Core.Maybe [RemediationException])
failedRemediationExceptionBatch_failedItems = Lens.lens (\FailedRemediationExceptionBatch' {failedItems} -> failedItems) (\s@FailedRemediationExceptionBatch' {} a -> s {failedItems = a} :: FailedRemediationExceptionBatch) Core.. Lens.mapping Lens._Coerce

instance
  Core.FromJSON
    FailedRemediationExceptionBatch
  where
  parseJSON =
    Core.withObject
      "FailedRemediationExceptionBatch"
      ( \x ->
          FailedRemediationExceptionBatch'
            Core.<$> (x Core..:? "FailureMessage")
            Core.<*> (x Core..:? "FailedItems" Core..!= Core.mempty)
      )

instance
  Core.Hashable
    FailedRemediationExceptionBatch

instance Core.NFData FailedRemediationExceptionBatch
