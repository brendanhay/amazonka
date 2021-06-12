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
-- Module      : Network.AWS.CodeCommit.Types.ApprovalRuleTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.ApprovalRuleTemplate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns information about an approval rule template.
--
-- /See:/ 'newApprovalRuleTemplate' smart constructor.
data ApprovalRuleTemplate = ApprovalRuleTemplate'
  { -- | The date the approval rule template was most recently changed, in
    -- timestamp format.
    lastModifiedDate :: Core.Maybe Core.POSIX,
    -- | The system-generated ID of the approval rule template.
    approvalRuleTemplateId :: Core.Maybe Core.Text,
    -- | The name of the approval rule template.
    approvalRuleTemplateName :: Core.Maybe Core.Text,
    -- | The date the approval rule template was created, in timestamp format.
    creationDate :: Core.Maybe Core.POSIX,
    -- | The description of the approval rule template.
    approvalRuleTemplateDescription :: Core.Maybe Core.Text,
    -- | The SHA-256 hash signature for the content of the approval rule
    -- template.
    ruleContentSha256 :: Core.Maybe Core.Text,
    -- | The content of the approval rule template.
    approvalRuleTemplateContent :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the user who made the most recent
    -- changes to the approval rule template.
    lastModifiedUser :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ApprovalRuleTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'approvalRuleTemplate_lastModifiedDate' - The date the approval rule template was most recently changed, in
-- timestamp format.
--
-- 'approvalRuleTemplateId', 'approvalRuleTemplate_approvalRuleTemplateId' - The system-generated ID of the approval rule template.
--
-- 'approvalRuleTemplateName', 'approvalRuleTemplate_approvalRuleTemplateName' - The name of the approval rule template.
--
-- 'creationDate', 'approvalRuleTemplate_creationDate' - The date the approval rule template was created, in timestamp format.
--
-- 'approvalRuleTemplateDescription', 'approvalRuleTemplate_approvalRuleTemplateDescription' - The description of the approval rule template.
--
-- 'ruleContentSha256', 'approvalRuleTemplate_ruleContentSha256' - The SHA-256 hash signature for the content of the approval rule
-- template.
--
-- 'approvalRuleTemplateContent', 'approvalRuleTemplate_approvalRuleTemplateContent' - The content of the approval rule template.
--
-- 'lastModifiedUser', 'approvalRuleTemplate_lastModifiedUser' - The Amazon Resource Name (ARN) of the user who made the most recent
-- changes to the approval rule template.
newApprovalRuleTemplate ::
  ApprovalRuleTemplate
newApprovalRuleTemplate =
  ApprovalRuleTemplate'
    { lastModifiedDate =
        Core.Nothing,
      approvalRuleTemplateId = Core.Nothing,
      approvalRuleTemplateName = Core.Nothing,
      creationDate = Core.Nothing,
      approvalRuleTemplateDescription = Core.Nothing,
      ruleContentSha256 = Core.Nothing,
      approvalRuleTemplateContent = Core.Nothing,
      lastModifiedUser = Core.Nothing
    }

-- | The date the approval rule template was most recently changed, in
-- timestamp format.
approvalRuleTemplate_lastModifiedDate :: Lens.Lens' ApprovalRuleTemplate (Core.Maybe Core.UTCTime)
approvalRuleTemplate_lastModifiedDate = Lens.lens (\ApprovalRuleTemplate' {lastModifiedDate} -> lastModifiedDate) (\s@ApprovalRuleTemplate' {} a -> s {lastModifiedDate = a} :: ApprovalRuleTemplate) Core.. Lens.mapping Core._Time

-- | The system-generated ID of the approval rule template.
approvalRuleTemplate_approvalRuleTemplateId :: Lens.Lens' ApprovalRuleTemplate (Core.Maybe Core.Text)
approvalRuleTemplate_approvalRuleTemplateId = Lens.lens (\ApprovalRuleTemplate' {approvalRuleTemplateId} -> approvalRuleTemplateId) (\s@ApprovalRuleTemplate' {} a -> s {approvalRuleTemplateId = a} :: ApprovalRuleTemplate)

-- | The name of the approval rule template.
approvalRuleTemplate_approvalRuleTemplateName :: Lens.Lens' ApprovalRuleTemplate (Core.Maybe Core.Text)
approvalRuleTemplate_approvalRuleTemplateName = Lens.lens (\ApprovalRuleTemplate' {approvalRuleTemplateName} -> approvalRuleTemplateName) (\s@ApprovalRuleTemplate' {} a -> s {approvalRuleTemplateName = a} :: ApprovalRuleTemplate)

-- | The date the approval rule template was created, in timestamp format.
approvalRuleTemplate_creationDate :: Lens.Lens' ApprovalRuleTemplate (Core.Maybe Core.UTCTime)
approvalRuleTemplate_creationDate = Lens.lens (\ApprovalRuleTemplate' {creationDate} -> creationDate) (\s@ApprovalRuleTemplate' {} a -> s {creationDate = a} :: ApprovalRuleTemplate) Core.. Lens.mapping Core._Time

-- | The description of the approval rule template.
approvalRuleTemplate_approvalRuleTemplateDescription :: Lens.Lens' ApprovalRuleTemplate (Core.Maybe Core.Text)
approvalRuleTemplate_approvalRuleTemplateDescription = Lens.lens (\ApprovalRuleTemplate' {approvalRuleTemplateDescription} -> approvalRuleTemplateDescription) (\s@ApprovalRuleTemplate' {} a -> s {approvalRuleTemplateDescription = a} :: ApprovalRuleTemplate)

-- | The SHA-256 hash signature for the content of the approval rule
-- template.
approvalRuleTemplate_ruleContentSha256 :: Lens.Lens' ApprovalRuleTemplate (Core.Maybe Core.Text)
approvalRuleTemplate_ruleContentSha256 = Lens.lens (\ApprovalRuleTemplate' {ruleContentSha256} -> ruleContentSha256) (\s@ApprovalRuleTemplate' {} a -> s {ruleContentSha256 = a} :: ApprovalRuleTemplate)

-- | The content of the approval rule template.
approvalRuleTemplate_approvalRuleTemplateContent :: Lens.Lens' ApprovalRuleTemplate (Core.Maybe Core.Text)
approvalRuleTemplate_approvalRuleTemplateContent = Lens.lens (\ApprovalRuleTemplate' {approvalRuleTemplateContent} -> approvalRuleTemplateContent) (\s@ApprovalRuleTemplate' {} a -> s {approvalRuleTemplateContent = a} :: ApprovalRuleTemplate)

-- | The Amazon Resource Name (ARN) of the user who made the most recent
-- changes to the approval rule template.
approvalRuleTemplate_lastModifiedUser :: Lens.Lens' ApprovalRuleTemplate (Core.Maybe Core.Text)
approvalRuleTemplate_lastModifiedUser = Lens.lens (\ApprovalRuleTemplate' {lastModifiedUser} -> lastModifiedUser) (\s@ApprovalRuleTemplate' {} a -> s {lastModifiedUser = a} :: ApprovalRuleTemplate)

instance Core.FromJSON ApprovalRuleTemplate where
  parseJSON =
    Core.withObject
      "ApprovalRuleTemplate"
      ( \x ->
          ApprovalRuleTemplate'
            Core.<$> (x Core..:? "lastModifiedDate")
            Core.<*> (x Core..:? "approvalRuleTemplateId")
            Core.<*> (x Core..:? "approvalRuleTemplateName")
            Core.<*> (x Core..:? "creationDate")
            Core.<*> (x Core..:? "approvalRuleTemplateDescription")
            Core.<*> (x Core..:? "ruleContentSha256")
            Core.<*> (x Core..:? "approvalRuleTemplateContent")
            Core.<*> (x Core..:? "lastModifiedUser")
      )

instance Core.Hashable ApprovalRuleTemplate

instance Core.NFData ApprovalRuleTemplate
