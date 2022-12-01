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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.ApprovalRuleOverriddenEventMetadata where

import Amazonka.CodeCommit.Types.OverrideStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns information about an override event for approval rules for a
-- pull request.
--
-- /See:/ 'newApprovalRuleOverriddenEventMetadata' smart constructor.
data ApprovalRuleOverriddenEventMetadata = ApprovalRuleOverriddenEventMetadata'
  { -- | The revision ID of the pull request when the override event occurred.
    revisionId :: Prelude.Maybe Prelude.Text,
    -- | The status of the override event.
    overrideStatus :: Prelude.Maybe OverrideStatus
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
-- 'revisionId', 'approvalRuleOverriddenEventMetadata_revisionId' - The revision ID of the pull request when the override event occurred.
--
-- 'overrideStatus', 'approvalRuleOverriddenEventMetadata_overrideStatus' - The status of the override event.
newApprovalRuleOverriddenEventMetadata ::
  ApprovalRuleOverriddenEventMetadata
newApprovalRuleOverriddenEventMetadata =
  ApprovalRuleOverriddenEventMetadata'
    { revisionId =
        Prelude.Nothing,
      overrideStatus = Prelude.Nothing
    }

-- | The revision ID of the pull request when the override event occurred.
approvalRuleOverriddenEventMetadata_revisionId :: Lens.Lens' ApprovalRuleOverriddenEventMetadata (Prelude.Maybe Prelude.Text)
approvalRuleOverriddenEventMetadata_revisionId = Lens.lens (\ApprovalRuleOverriddenEventMetadata' {revisionId} -> revisionId) (\s@ApprovalRuleOverriddenEventMetadata' {} a -> s {revisionId = a} :: ApprovalRuleOverriddenEventMetadata)

-- | The status of the override event.
approvalRuleOverriddenEventMetadata_overrideStatus :: Lens.Lens' ApprovalRuleOverriddenEventMetadata (Prelude.Maybe OverrideStatus)
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
            Prelude.<$> (x Core..:? "revisionId")
            Prelude.<*> (x Core..:? "overrideStatus")
      )

instance
  Prelude.Hashable
    ApprovalRuleOverriddenEventMetadata
  where
  hashWithSalt
    _salt
    ApprovalRuleOverriddenEventMetadata' {..} =
      _salt `Prelude.hashWithSalt` revisionId
        `Prelude.hashWithSalt` overrideStatus

instance
  Prelude.NFData
    ApprovalRuleOverriddenEventMetadata
  where
  rnf ApprovalRuleOverriddenEventMetadata' {..} =
    Prelude.rnf revisionId
      `Prelude.seq` Prelude.rnf overrideStatus
