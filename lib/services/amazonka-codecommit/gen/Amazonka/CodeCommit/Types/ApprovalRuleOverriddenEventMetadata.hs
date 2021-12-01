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
-- Module      : Amazonka.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata where

import Amazonka.CodeCommit.Types.OverrideStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns information about an override event for approval rules for a
-- pull request.
--
-- /See:/ 'newApprovalRuleOverriddenEventMetadata' smart constructor.
data ApprovalRuleOverriddenEventMetadata = ApprovalRuleOverriddenEventMetadata'
  { -- | The status of the override event.
    overrideStatus :: Prelude.Maybe OverrideStatus,
    -- | The revision ID of the pull request when the override event occurred.
    revisionId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApprovalRuleOverriddenEventMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'overrideStatus', 'approvalRuleOverriddenEventMetadata_overrideStatus' - The status of the override event.
--
-- 'revisionId', 'approvalRuleOverriddenEventMetadata_revisionId' - The revision ID of the pull request when the override event occurred.
newApprovalRuleOverriddenEventMetadata ::
  ApprovalRuleOverriddenEventMetadata
newApprovalRuleOverriddenEventMetadata =
  ApprovalRuleOverriddenEventMetadata'
    { overrideStatus =
        Prelude.Nothing,
      revisionId = Prelude.Nothing
    }

-- | The status of the override event.
approvalRuleOverriddenEventMetadata_overrideStatus :: Lens.Lens' ApprovalRuleOverriddenEventMetadata (Prelude.Maybe OverrideStatus)
approvalRuleOverriddenEventMetadata_overrideStatus = Lens.lens (\ApprovalRuleOverriddenEventMetadata' {overrideStatus} -> overrideStatus) (\s@ApprovalRuleOverriddenEventMetadata' {} a -> s {overrideStatus = a} :: ApprovalRuleOverriddenEventMetadata)

-- | The revision ID of the pull request when the override event occurred.
approvalRuleOverriddenEventMetadata_revisionId :: Lens.Lens' ApprovalRuleOverriddenEventMetadata (Prelude.Maybe Prelude.Text)
approvalRuleOverriddenEventMetadata_revisionId = Lens.lens (\ApprovalRuleOverriddenEventMetadata' {revisionId} -> revisionId) (\s@ApprovalRuleOverriddenEventMetadata' {} a -> s {revisionId = a} :: ApprovalRuleOverriddenEventMetadata)

instance
  Core.FromJSON
    ApprovalRuleOverriddenEventMetadata
  where
  parseJSON =
    Core.withObject
      "ApprovalRuleOverriddenEventMetadata"
      ( \x ->
          ApprovalRuleOverriddenEventMetadata'
            Prelude.<$> (x Core..:? "overrideStatus")
            Prelude.<*> (x Core..:? "revisionId")
      )

instance
  Prelude.Hashable
    ApprovalRuleOverriddenEventMetadata
  where
  hashWithSalt
    salt'
    ApprovalRuleOverriddenEventMetadata' {..} =
      salt' `Prelude.hashWithSalt` revisionId
        `Prelude.hashWithSalt` overrideStatus

instance
  Prelude.NFData
    ApprovalRuleOverriddenEventMetadata
  where
  rnf ApprovalRuleOverriddenEventMetadata' {..} =
    Prelude.rnf overrideStatus
      `Prelude.seq` Prelude.rnf revisionId
