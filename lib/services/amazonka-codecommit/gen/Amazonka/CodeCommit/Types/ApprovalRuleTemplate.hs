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
-- Module      : Amazonka.CodeCommit.Types.ApprovalRuleTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.ApprovalRuleTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about an approval rule template.
--
-- /See:/ 'newApprovalRuleTemplate' smart constructor.
data ApprovalRuleTemplate = ApprovalRuleTemplate'
  { -- | The content of the approval rule template.
    approvalRuleTemplateContent :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the user who made the most recent
    -- changes to the approval rule template.
    lastModifiedUser :: Prelude.Maybe Prelude.Text,
    -- | The date the approval rule template was most recently changed, in
    -- timestamp format.
    lastModifiedDate :: Prelude.Maybe Data.POSIX,
    -- | The date the approval rule template was created, in timestamp format.
    creationDate :: Prelude.Maybe Data.POSIX,
    -- | The name of the approval rule template.
    approvalRuleTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The SHA-256 hash signature for the content of the approval rule
    -- template.
    ruleContentSha256 :: Prelude.Maybe Prelude.Text,
    -- | The description of the approval rule template.
    approvalRuleTemplateDescription :: Prelude.Maybe Prelude.Text,
    -- | The system-generated ID of the approval rule template.
    approvalRuleTemplateId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApprovalRuleTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalRuleTemplateContent', 'approvalRuleTemplate_approvalRuleTemplateContent' - The content of the approval rule template.
--
-- 'lastModifiedUser', 'approvalRuleTemplate_lastModifiedUser' - The Amazon Resource Name (ARN) of the user who made the most recent
-- changes to the approval rule template.
--
-- 'lastModifiedDate', 'approvalRuleTemplate_lastModifiedDate' - The date the approval rule template was most recently changed, in
-- timestamp format.
--
-- 'creationDate', 'approvalRuleTemplate_creationDate' - The date the approval rule template was created, in timestamp format.
--
-- 'approvalRuleTemplateName', 'approvalRuleTemplate_approvalRuleTemplateName' - The name of the approval rule template.
--
-- 'ruleContentSha256', 'approvalRuleTemplate_ruleContentSha256' - The SHA-256 hash signature for the content of the approval rule
-- template.
--
-- 'approvalRuleTemplateDescription', 'approvalRuleTemplate_approvalRuleTemplateDescription' - The description of the approval rule template.
--
-- 'approvalRuleTemplateId', 'approvalRuleTemplate_approvalRuleTemplateId' - The system-generated ID of the approval rule template.
newApprovalRuleTemplate ::
  ApprovalRuleTemplate
newApprovalRuleTemplate =
  ApprovalRuleTemplate'
    { approvalRuleTemplateContent =
        Prelude.Nothing,
      lastModifiedUser = Prelude.Nothing,
      lastModifiedDate = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      approvalRuleTemplateName = Prelude.Nothing,
      ruleContentSha256 = Prelude.Nothing,
      approvalRuleTemplateDescription = Prelude.Nothing,
      approvalRuleTemplateId = Prelude.Nothing
    }

-- | The content of the approval rule template.
approvalRuleTemplate_approvalRuleTemplateContent :: Lens.Lens' ApprovalRuleTemplate (Prelude.Maybe Prelude.Text)
approvalRuleTemplate_approvalRuleTemplateContent = Lens.lens (\ApprovalRuleTemplate' {approvalRuleTemplateContent} -> approvalRuleTemplateContent) (\s@ApprovalRuleTemplate' {} a -> s {approvalRuleTemplateContent = a} :: ApprovalRuleTemplate)

-- | The Amazon Resource Name (ARN) of the user who made the most recent
-- changes to the approval rule template.
approvalRuleTemplate_lastModifiedUser :: Lens.Lens' ApprovalRuleTemplate (Prelude.Maybe Prelude.Text)
approvalRuleTemplate_lastModifiedUser = Lens.lens (\ApprovalRuleTemplate' {lastModifiedUser} -> lastModifiedUser) (\s@ApprovalRuleTemplate' {} a -> s {lastModifiedUser = a} :: ApprovalRuleTemplate)

-- | The date the approval rule template was most recently changed, in
-- timestamp format.
approvalRuleTemplate_lastModifiedDate :: Lens.Lens' ApprovalRuleTemplate (Prelude.Maybe Prelude.UTCTime)
approvalRuleTemplate_lastModifiedDate = Lens.lens (\ApprovalRuleTemplate' {lastModifiedDate} -> lastModifiedDate) (\s@ApprovalRuleTemplate' {} a -> s {lastModifiedDate = a} :: ApprovalRuleTemplate) Prelude.. Lens.mapping Data._Time

-- | The date the approval rule template was created, in timestamp format.
approvalRuleTemplate_creationDate :: Lens.Lens' ApprovalRuleTemplate (Prelude.Maybe Prelude.UTCTime)
approvalRuleTemplate_creationDate = Lens.lens (\ApprovalRuleTemplate' {creationDate} -> creationDate) (\s@ApprovalRuleTemplate' {} a -> s {creationDate = a} :: ApprovalRuleTemplate) Prelude.. Lens.mapping Data._Time

-- | The name of the approval rule template.
approvalRuleTemplate_approvalRuleTemplateName :: Lens.Lens' ApprovalRuleTemplate (Prelude.Maybe Prelude.Text)
approvalRuleTemplate_approvalRuleTemplateName = Lens.lens (\ApprovalRuleTemplate' {approvalRuleTemplateName} -> approvalRuleTemplateName) (\s@ApprovalRuleTemplate' {} a -> s {approvalRuleTemplateName = a} :: ApprovalRuleTemplate)

-- | The SHA-256 hash signature for the content of the approval rule
-- template.
approvalRuleTemplate_ruleContentSha256 :: Lens.Lens' ApprovalRuleTemplate (Prelude.Maybe Prelude.Text)
approvalRuleTemplate_ruleContentSha256 = Lens.lens (\ApprovalRuleTemplate' {ruleContentSha256} -> ruleContentSha256) (\s@ApprovalRuleTemplate' {} a -> s {ruleContentSha256 = a} :: ApprovalRuleTemplate)

-- | The description of the approval rule template.
approvalRuleTemplate_approvalRuleTemplateDescription :: Lens.Lens' ApprovalRuleTemplate (Prelude.Maybe Prelude.Text)
approvalRuleTemplate_approvalRuleTemplateDescription = Lens.lens (\ApprovalRuleTemplate' {approvalRuleTemplateDescription} -> approvalRuleTemplateDescription) (\s@ApprovalRuleTemplate' {} a -> s {approvalRuleTemplateDescription = a} :: ApprovalRuleTemplate)

-- | The system-generated ID of the approval rule template.
approvalRuleTemplate_approvalRuleTemplateId :: Lens.Lens' ApprovalRuleTemplate (Prelude.Maybe Prelude.Text)
approvalRuleTemplate_approvalRuleTemplateId = Lens.lens (\ApprovalRuleTemplate' {approvalRuleTemplateId} -> approvalRuleTemplateId) (\s@ApprovalRuleTemplate' {} a -> s {approvalRuleTemplateId = a} :: ApprovalRuleTemplate)

instance Data.FromJSON ApprovalRuleTemplate where
  parseJSON =
    Data.withObject
      "ApprovalRuleTemplate"
      ( \x ->
          ApprovalRuleTemplate'
            Prelude.<$> (x Data..:? "approvalRuleTemplateContent")
            Prelude.<*> (x Data..:? "lastModifiedUser")
            Prelude.<*> (x Data..:? "lastModifiedDate")
            Prelude.<*> (x Data..:? "creationDate")
            Prelude.<*> (x Data..:? "approvalRuleTemplateName")
            Prelude.<*> (x Data..:? "ruleContentSha256")
            Prelude.<*> (x Data..:? "approvalRuleTemplateDescription")
            Prelude.<*> (x Data..:? "approvalRuleTemplateId")
      )

instance Prelude.Hashable ApprovalRuleTemplate where
  hashWithSalt _salt ApprovalRuleTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` approvalRuleTemplateContent
      `Prelude.hashWithSalt` lastModifiedUser
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` approvalRuleTemplateName
      `Prelude.hashWithSalt` ruleContentSha256
      `Prelude.hashWithSalt` approvalRuleTemplateDescription
      `Prelude.hashWithSalt` approvalRuleTemplateId

instance Prelude.NFData ApprovalRuleTemplate where
  rnf ApprovalRuleTemplate' {..} =
    Prelude.rnf approvalRuleTemplateContent
      `Prelude.seq` Prelude.rnf lastModifiedUser
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf approvalRuleTemplateName
      `Prelude.seq` Prelude.rnf ruleContentSha256
      `Prelude.seq` Prelude.rnf approvalRuleTemplateDescription
      `Prelude.seq` Prelude.rnf approvalRuleTemplateId
