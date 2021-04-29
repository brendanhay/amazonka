{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CodeCommit.Types.ApprovalRuleEventMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ApprovalRuleEventMetadata where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Returns information about an event for an approval rule.
--
-- /See:/ 'newApprovalRuleEventMetadata' smart constructor.
data ApprovalRuleEventMetadata = ApprovalRuleEventMetadata'
  { -- | The content of the approval rule.
    approvalRuleContent :: Prelude.Maybe Prelude.Text,
    -- | The system-generated ID of the approval rule.
    approvalRuleId :: Prelude.Maybe Prelude.Text,
    -- | The name of the approval rule.
    approvalRuleName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ApprovalRuleEventMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalRuleContent', 'approvalRuleEventMetadata_approvalRuleContent' - The content of the approval rule.
--
-- 'approvalRuleId', 'approvalRuleEventMetadata_approvalRuleId' - The system-generated ID of the approval rule.
--
-- 'approvalRuleName', 'approvalRuleEventMetadata_approvalRuleName' - The name of the approval rule.
newApprovalRuleEventMetadata ::
  ApprovalRuleEventMetadata
newApprovalRuleEventMetadata =
  ApprovalRuleEventMetadata'
    { approvalRuleContent =
        Prelude.Nothing,
      approvalRuleId = Prelude.Nothing,
      approvalRuleName = Prelude.Nothing
    }

-- | The content of the approval rule.
approvalRuleEventMetadata_approvalRuleContent :: Lens.Lens' ApprovalRuleEventMetadata (Prelude.Maybe Prelude.Text)
approvalRuleEventMetadata_approvalRuleContent = Lens.lens (\ApprovalRuleEventMetadata' {approvalRuleContent} -> approvalRuleContent) (\s@ApprovalRuleEventMetadata' {} a -> s {approvalRuleContent = a} :: ApprovalRuleEventMetadata)

-- | The system-generated ID of the approval rule.
approvalRuleEventMetadata_approvalRuleId :: Lens.Lens' ApprovalRuleEventMetadata (Prelude.Maybe Prelude.Text)
approvalRuleEventMetadata_approvalRuleId = Lens.lens (\ApprovalRuleEventMetadata' {approvalRuleId} -> approvalRuleId) (\s@ApprovalRuleEventMetadata' {} a -> s {approvalRuleId = a} :: ApprovalRuleEventMetadata)

-- | The name of the approval rule.
approvalRuleEventMetadata_approvalRuleName :: Lens.Lens' ApprovalRuleEventMetadata (Prelude.Maybe Prelude.Text)
approvalRuleEventMetadata_approvalRuleName = Lens.lens (\ApprovalRuleEventMetadata' {approvalRuleName} -> approvalRuleName) (\s@ApprovalRuleEventMetadata' {} a -> s {approvalRuleName = a} :: ApprovalRuleEventMetadata)

instance Prelude.FromJSON ApprovalRuleEventMetadata where
  parseJSON =
    Prelude.withObject
      "ApprovalRuleEventMetadata"
      ( \x ->
          ApprovalRuleEventMetadata'
            Prelude.<$> (x Prelude..:? "approvalRuleContent")
            Prelude.<*> (x Prelude..:? "approvalRuleId")
            Prelude.<*> (x Prelude..:? "approvalRuleName")
      )

instance Prelude.Hashable ApprovalRuleEventMetadata

instance Prelude.NFData ApprovalRuleEventMetadata
