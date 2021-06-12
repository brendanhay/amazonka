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
-- Module      : Network.AWS.CodeCommit.Types.ApprovalRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ApprovalRule where

import Network.AWS.CodeCommit.Types.OriginApprovalRuleTemplate
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns information about an approval rule.
--
-- /See:/ 'newApprovalRule' smart constructor.
data ApprovalRule = ApprovalRule'
  { -- | The date the approval rule was most recently changed, in timestamp
    -- format.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The content of the approval rule.
    approvalRuleContent :: Core.Maybe Core.Text,
    -- | The approval rule template used to create the rule.
    originApprovalRuleTemplate :: Core.Maybe OriginApprovalRuleTemplate,
    -- | The date the approval rule was created, in timestamp format.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The SHA-256 hash signature for the content of the approval rule.
    ruleContentSha256 :: Core.Maybe Core.Text,
    -- | The system-generated ID of the approval rule.
    approvalRuleId :: Core.Maybe Core.Text,
    -- | The name of the approval rule.
    approvalRuleName :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the user who made the most recent
    -- changes to the approval rule.
    lastModifiedUser :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApprovalRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'approvalRule_lastModifiedDate' - The date the approval rule was most recently changed, in timestamp
-- format.
--
-- 'approvalRuleContent', 'approvalRule_approvalRuleContent' - The content of the approval rule.
--
-- 'originApprovalRuleTemplate', 'approvalRule_originApprovalRuleTemplate' - The approval rule template used to create the rule.
--
-- 'creationDate', 'approvalRule_creationDate' - The date the approval rule was created, in timestamp format.
--
-- 'ruleContentSha256', 'approvalRule_ruleContentSha256' - The SHA-256 hash signature for the content of the approval rule.
--
-- 'approvalRuleId', 'approvalRule_approvalRuleId' - The system-generated ID of the approval rule.
--
-- 'approvalRuleName', 'approvalRule_approvalRuleName' - The name of the approval rule.
--
-- 'lastModifiedUser', 'approvalRule_lastModifiedUser' - The Amazon Resource Name (ARN) of the user who made the most recent
-- changes to the approval rule.
newApprovalRule ::
  ApprovalRule
newApprovalRule =
  ApprovalRule'
    { lastModifiedDate = Core.Nothing,
      approvalRuleContent = Core.Nothing,
      originApprovalRuleTemplate = Core.Nothing,
      creationDate = Core.Nothing,
      ruleContentSha256 = Core.Nothing,
      approvalRuleId = Core.Nothing,
      approvalRuleName = Core.Nothing,
      lastModifiedUser = Core.Nothing
    }

-- | The date the approval rule was most recently changed, in timestamp
-- format.
approvalRule_lastModifiedDate :: Lens.Lens' ApprovalRule (Core.Maybe Core.UTCTime)
approvalRule_lastModifiedDate = Lens.lens (\ApprovalRule' {lastModifiedDate} -> lastModifiedDate) (\s@ApprovalRule' {} a -> s {lastModifiedDate = a} :: ApprovalRule) Core.. Lens.mapping Core._Time

-- | The content of the approval rule.
approvalRule_approvalRuleContent :: Lens.Lens' ApprovalRule (Core.Maybe Core.Text)
approvalRule_approvalRuleContent = Lens.lens (\ApprovalRule' {approvalRuleContent} -> approvalRuleContent) (\s@ApprovalRule' {} a -> s {approvalRuleContent = a} :: ApprovalRule)

-- | The approval rule template used to create the rule.
approvalRule_originApprovalRuleTemplate :: Lens.Lens' ApprovalRule (Core.Maybe OriginApprovalRuleTemplate)
approvalRule_originApprovalRuleTemplate = Lens.lens (\ApprovalRule' {originApprovalRuleTemplate} -> originApprovalRuleTemplate) (\s@ApprovalRule' {} a -> s {originApprovalRuleTemplate = a} :: ApprovalRule)

-- | The date the approval rule was created, in timestamp format.
approvalRule_creationDate :: Lens.Lens' ApprovalRule (Core.Maybe Core.UTCTime)
approvalRule_creationDate = Lens.lens (\ApprovalRule' {creationDate} -> creationDate) (\s@ApprovalRule' {} a -> s {creationDate = a} :: ApprovalRule) Core.. Lens.mapping Core._Time

-- | The SHA-256 hash signature for the content of the approval rule.
approvalRule_ruleContentSha256 :: Lens.Lens' ApprovalRule (Core.Maybe Core.Text)
approvalRule_ruleContentSha256 = Lens.lens (\ApprovalRule' {ruleContentSha256} -> ruleContentSha256) (\s@ApprovalRule' {} a -> s {ruleContentSha256 = a} :: ApprovalRule)

-- | The system-generated ID of the approval rule.
approvalRule_approvalRuleId :: Lens.Lens' ApprovalRule (Core.Maybe Core.Text)
approvalRule_approvalRuleId = Lens.lens (\ApprovalRule' {approvalRuleId} -> approvalRuleId) (\s@ApprovalRule' {} a -> s {approvalRuleId = a} :: ApprovalRule)

-- | The name of the approval rule.
approvalRule_approvalRuleName :: Lens.Lens' ApprovalRule (Core.Maybe Core.Text)
approvalRule_approvalRuleName = Lens.lens (\ApprovalRule' {approvalRuleName} -> approvalRuleName) (\s@ApprovalRule' {} a -> s {approvalRuleName = a} :: ApprovalRule)

-- | The Amazon Resource Name (ARN) of the user who made the most recent
-- changes to the approval rule.
approvalRule_lastModifiedUser :: Lens.Lens' ApprovalRule (Core.Maybe Core.Text)
approvalRule_lastModifiedUser = Lens.lens (\ApprovalRule' {lastModifiedUser} -> lastModifiedUser) (\s@ApprovalRule' {} a -> s {lastModifiedUser = a} :: ApprovalRule)

instance Core.FromJSON ApprovalRule where
  parseJSON =
    Core.withObject
      "ApprovalRule"
      ( \x ->
          ApprovalRule'
            Core.<$> (x Core..:? "lastModifiedDate")
            Core.<*> (x Core..:? "approvalRuleContent")
            Core.<*> (x Core..:? "originApprovalRuleTemplate")
            Core.<*> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "ruleContentSha256")
            Core.<*> (x Core..:? "approvalRuleId")
            Core.<*> (x Core..:? "approvalRuleName")
            Core.<*> (x Core..:? "lastModifiedUser")
      )

instance Core.Hashable ApprovalRule

instance Core.NFData ApprovalRule
