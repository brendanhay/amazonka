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
-- Module      : Network.AWS.Config.Types.FailedRemediationBatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.FailedRemediationBatch where

import Network.AWS.Config.Types.RemediationConfiguration
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | List of each of the failed remediations with specific reasons.
--
-- /See:/ 'newFailedRemediationBatch' smart constructor.
data FailedRemediationBatch = FailedRemediationBatch'
  { -- | Returns a failure message. For example, the resource is already
    -- compliant.
    failureMessage :: Core.Maybe Core.Text,
    -- | Returns remediation configurations of the failed items.
    failedItems :: Core.Maybe [RemediationConfiguration]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FailedRemediationBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureMessage', 'failedRemediationBatch_failureMessage' - Returns a failure message. For example, the resource is already
-- compliant.
--
-- 'failedItems', 'failedRemediationBatch_failedItems' - Returns remediation configurations of the failed items.
newFailedRemediationBatch ::
  FailedRemediationBatch
newFailedRemediationBatch =
  FailedRemediationBatch'
    { failureMessage =
        Core.Nothing,
      failedItems = Core.Nothing
    }

-- | Returns a failure message. For example, the resource is already
-- compliant.
failedRemediationBatch_failureMessage :: Lens.Lens' FailedRemediationBatch (Core.Maybe Core.Text)
failedRemediationBatch_failureMessage = Lens.lens (\FailedRemediationBatch' {failureMessage} -> failureMessage) (\s@FailedRemediationBatch' {} a -> s {failureMessage = a} :: FailedRemediationBatch)

-- | Returns remediation configurations of the failed items.
failedRemediationBatch_failedItems :: Lens.Lens' FailedRemediationBatch (Core.Maybe [RemediationConfiguration])
failedRemediationBatch_failedItems = Lens.lens (\FailedRemediationBatch' {failedItems} -> failedItems) (\s@FailedRemediationBatch' {} a -> s {failedItems = a} :: FailedRemediationBatch) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON FailedRemediationBatch where
  parseJSON =
    Core.withObject
      "FailedRemediationBatch"
      ( \x ->
          FailedRemediationBatch'
            Core.<$> (x Core..:? "FailureMessage")
            Core.<*> (x Core..:? "FailedItems" Core..!= Core.mempty)
      )

instance Core.Hashable FailedRemediationBatch

instance Core.NFData FailedRemediationBatch
