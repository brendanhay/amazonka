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
-- Module      : Amazonka.CodeCommit.Types.ApprovalRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.ApprovalRule where

import Amazonka.CodeCommit.Types.OriginApprovalRuleTemplate
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about an approval rule.
--
-- /See:/ 'newApprovalRule' smart constructor.
data ApprovalRule = ApprovalRule'
  { -- | The Amazon Resource Name (ARN) of the user who made the most recent
    -- changes to the approval rule.
    lastModifiedUser :: Prelude.Maybe Prelude.Text,
    -- | The date the approval rule was most recently changed, in timestamp
    -- format.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The date the approval rule was created, in timestamp format.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the approval rule.
    approvalRuleName :: Prelude.Maybe Prelude.Text,
    -- | The SHA-256 hash signature for the content of the approval rule.
    ruleContentSha256 :: Prelude.Maybe Prelude.Text,
    -- | The content of the approval rule.
    approvalRuleContent :: Prelude.Maybe Prelude.Text,
    -- | The approval rule template used to create the rule.
    originApprovalRuleTemplate :: Prelude.Maybe OriginApprovalRuleTemplate,
    -- | The system-generated ID of the approval rule.
    approvalRuleId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApprovalRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedUser', 'approvalRule_lastModifiedUser' - The Amazon Resource Name (ARN) of the user who made the most recent
-- changes to the approval rule.
--
-- 'lastModifiedDate', 'approvalRule_lastModifiedDate' - The date the approval rule was most recently changed, in timestamp
-- format.
--
-- 'creationDate', 'approvalRule_creationDate' - The date the approval rule was created, in timestamp format.
--
-- 'approvalRuleName', 'approvalRule_approvalRuleName' - The name of the approval rule.
--
-- 'ruleContentSha256', 'approvalRule_ruleContentSha256' - The SHA-256 hash signature for the content of the approval rule.
--
-- 'approvalRuleContent', 'approvalRule_approvalRuleContent' - The content of the approval rule.
--
-- 'originApprovalRuleTemplate', 'approvalRule_originApprovalRuleTemplate' - The approval rule template used to create the rule.
--
-- 'approvalRuleId', 'approvalRule_approvalRuleId' - The system-generated ID of the approval rule.
newApprovalRule ::
  ApprovalRule
newApprovalRule =
  ApprovalRule'
    { lastModifiedUser = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      approvalRuleName = Prelude.Nothing,
      ruleContentSha256 = Prelude.Nothing,
      approvalRuleContent = Prelude.Nothing,
      originApprovalRuleTemplate = Prelude.Nothing,
      approvalRuleId = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the user who made the most recent
-- changes to the approval rule.
approvalRule_lastModifiedUser :: Lens.Lens' ApprovalRule (Prelude.Maybe Prelude.Text)
approvalRule_lastModifiedUser = Lens.lens (\ApprovalRule' {lastModifiedUser} -> lastModifiedUser) (\s@ApprovalRule' {} a -> s {lastModifiedUser = a} :: ApprovalRule)

-- | The date the approval rule was most recently changed, in timestamp
-- format.
approvalRule_lastModifiedDate :: Lens.Lens' ApprovalRule (Prelude.Maybe Prelude.UTCTime)
approvalRule_lastModifiedDate = Lens.lens (\ApprovalRule' {lastModifiedDate} -> lastModifiedDate) (\s@ApprovalRule' {} a -> s {lastModifiedDate = a} :: ApprovalRule) Prelude.. Lens.mapping Data._Time

-- | The date the approval rule was created, in timestamp format.
approvalRule_creationDate :: Lens.Lens' ApprovalRule (Prelude.Maybe Prelude.UTCTime)
approvalRule_creationDate = Lens.lens (\ApprovalRule' {creationDate} -> creationDate) (\s@ApprovalRule' {} a -> s {creationDate = a} :: ApprovalRule) Prelude.. Lens.mapping Data._Time

-- | The name of the approval rule.
approvalRule_approvalRuleName :: Lens.Lens' ApprovalRule (Prelude.Maybe Prelude.Text)
approvalRule_approvalRuleName = Lens.lens (\ApprovalRule' {approvalRuleName} -> approvalRuleName) (\s@ApprovalRule' {} a -> s {approvalRuleName = a} :: ApprovalRule)

-- | The SHA-256 hash signature for the content of the approval rule.
approvalRule_ruleContentSha256 :: Lens.Lens' ApprovalRule (Prelude.Maybe Prelude.Text)
approvalRule_ruleContentSha256 = Lens.lens (\ApprovalRule' {ruleContentSha256} -> ruleContentSha256) (\s@ApprovalRule' {} a -> s {ruleContentSha256 = a} :: ApprovalRule)

-- | The content of the approval rule.
approvalRule_approvalRuleContent :: Lens.Lens' ApprovalRule (Prelude.Maybe Prelude.Text)
approvalRule_approvalRuleContent = Lens.lens (\ApprovalRule' {approvalRuleContent} -> approvalRuleContent) (\s@ApprovalRule' {} a -> s {approvalRuleContent = a} :: ApprovalRule)

-- | The approval rule template used to create the rule.
approvalRule_originApprovalRuleTemplate :: Lens.Lens' ApprovalRule (Prelude.Maybe OriginApprovalRuleTemplate)
approvalRule_originApprovalRuleTemplate = Lens.lens (\ApprovalRule' {originApprovalRuleTemplate} -> originApprovalRuleTemplate) (\s@ApprovalRule' {} a -> s {originApprovalRuleTemplate = a} :: ApprovalRule)

-- | The system-generated ID of the approval rule.
approvalRule_approvalRuleId :: Lens.Lens' ApprovalRule (Prelude.Maybe Prelude.Text)
approvalRule_approvalRuleId = Lens.lens (\ApprovalRule' {approvalRuleId} -> approvalRuleId) (\s@ApprovalRule' {} a -> s {approvalRuleId = a} :: ApprovalRule)

instance Data.FromJSON ApprovalRule where
  parseJSON =
    Data.withObject
      "ApprovalRule"
      ( \x ->
          ApprovalRule'
            Prelude.<$> (x Data..:? "lastModifiedUser")
            Prelude.<*> (x Data..:? "lastModifiedDate")
            Prelude.<*> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "approvalRuleName")
            Prelude.<*> (x Data..:? "ruleContentSha256")
            Prelude.<*> (x Data..:? "approvalRuleContent")
            Prelude.<*> (x Data..:? "originApprovalRuleTemplate")
            Prelude.<*> (x Data..:? "approvalRuleId")
      )

instance Prelude.Hashable ApprovalRule where
  hashWithSalt _salt ApprovalRule' {..} =
    _salt `Prelude.hashWithSalt` lastModifiedUser
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` approvalRuleName
      `Prelude.hashWithSalt` ruleContentSha256
      `Prelude.hashWithSalt` approvalRuleContent
      `Prelude.hashWithSalt` originApprovalRuleTemplate
      `Prelude.hashWithSalt` approvalRuleId

instance Prelude.NFData ApprovalRule where
  rnf ApprovalRule' {..} =
    Prelude.rnf lastModifiedUser
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf approvalRuleName
      `Prelude.seq` Prelude.rnf ruleContentSha256
      `Prelude.seq` Prelude.rnf approvalRuleContent
      `Prelude.seq` Prelude.rnf originApprovalRuleTemplate
      `Prelude.seq` Prelude.rnf approvalRuleId
