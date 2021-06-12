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
-- Module      : Network.AWS.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata where

import Network.AWS.CodeCommit.Types.OverrideStatus
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns information about an override event for approval rules for a
-- pull request.
--
-- /See:/ 'newApprovalRuleOverriddenEventMetadata' smart constructor.
data ApprovalRuleOverriddenEventMetadata = ApprovalRuleOverriddenEventMetadata'
  { -- | The revision ID of the pull request when the override event occurred.
    revisionId :: Core.Maybe Core.Text,
    -- | The status of the override event.
    overrideStatus :: Core.Maybe OverrideStatus
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApprovalRuleOverriddenEventMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'approvalRuleOverriddenEventMetadata_revisionId' - The revision ID of the pull request when the override event occurred.
--
-- 'overrideStatus', 'approvalRuleOverriddenEventMetadata_overrideStatus' - The status of the override event.
newApprovalRuleOverriddenEventMetadata ::
  ApprovalRuleOverriddenEventMetadata
newApprovalRuleOverriddenEventMetadata =
  ApprovalRuleOverriddenEventMetadata'
    { revisionId =
        Core.Nothing,
      overrideStatus = Core.Nothing
    }

-- | The revision ID of the pull request when the override event occurred.
approvalRuleOverriddenEventMetadata_revisionId :: Lens.Lens' ApprovalRuleOverriddenEventMetadata (Core.Maybe Core.Text)
approvalRuleOverriddenEventMetadata_revisionId = Lens.lens (\ApprovalRuleOverriddenEventMetadata' {revisionId} -> revisionId) (\s@ApprovalRuleOverriddenEventMetadata' {} a -> s {revisionId = a} :: ApprovalRuleOverriddenEventMetadata)

-- | The status of the override event.
approvalRuleOverriddenEventMetadata_overrideStatus :: Lens.Lens' ApprovalRuleOverriddenEventMetadata (Core.Maybe OverrideStatus)
approvalRuleOverriddenEventMetadata_overrideStatus = Lens.lens (\ApprovalRuleOverriddenEventMetadata' {overrideStatus} -> overrideStatus) (\s@ApprovalRuleOverriddenEventMetadata' {} a -> s {overrideStatus = a} :: ApprovalRuleOverriddenEventMetadata)

instance
  Core.FromJSON
    ApprovalRuleOverriddenEventMetadata
  where
  parseJSON =
    Core.withObject
      "ApprovalRuleOverriddenEventMetadata"
      ( \x ->
          ApprovalRuleOverriddenEventMetadata'
            Core.<$> (x Core..:? "revisionId")
            Core.<*> (x Core..:? "overrideStatus")
      )

instance
  Core.Hashable
    ApprovalRuleOverriddenEventMetadata

instance
  Core.NFData
    ApprovalRuleOverriddenEventMetadata
