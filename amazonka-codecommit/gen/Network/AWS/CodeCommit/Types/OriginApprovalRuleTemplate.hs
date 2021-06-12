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
-- Module      : Network.AWS.CodeCommit.Types.OriginApprovalRuleTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeCommit.Types.OriginApprovalRuleTemplate where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Returns information about the template that created the approval rule
-- for a pull request.
--
-- /See:/ 'newOriginApprovalRuleTemplate' smart constructor.
data OriginApprovalRuleTemplate = OriginApprovalRuleTemplate'
  { -- | The ID of the template that created the approval rule.
    approvalRuleTemplateId :: Core.Maybe Core.Text,
    -- | The name of the template that created the approval rule.
    approvalRuleTemplateName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'OriginApprovalRuleTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalRuleTemplateId', 'originApprovalRuleTemplate_approvalRuleTemplateId' - The ID of the template that created the approval rule.
--
-- 'approvalRuleTemplateName', 'originApprovalRuleTemplate_approvalRuleTemplateName' - The name of the template that created the approval rule.
newOriginApprovalRuleTemplate ::
  OriginApprovalRuleTemplate
newOriginApprovalRuleTemplate =
  OriginApprovalRuleTemplate'
    { approvalRuleTemplateId =
        Core.Nothing,
      approvalRuleTemplateName = Core.Nothing
    }

-- | The ID of the template that created the approval rule.
originApprovalRuleTemplate_approvalRuleTemplateId :: Lens.Lens' OriginApprovalRuleTemplate (Core.Maybe Core.Text)
originApprovalRuleTemplate_approvalRuleTemplateId = Lens.lens (\OriginApprovalRuleTemplate' {approvalRuleTemplateId} -> approvalRuleTemplateId) (\s@OriginApprovalRuleTemplate' {} a -> s {approvalRuleTemplateId = a} :: OriginApprovalRuleTemplate)

-- | The name of the template that created the approval rule.
originApprovalRuleTemplate_approvalRuleTemplateName :: Lens.Lens' OriginApprovalRuleTemplate (Core.Maybe Core.Text)
originApprovalRuleTemplate_approvalRuleTemplateName = Lens.lens (\OriginApprovalRuleTemplate' {approvalRuleTemplateName} -> approvalRuleTemplateName) (\s@OriginApprovalRuleTemplate' {} a -> s {approvalRuleTemplateName = a} :: OriginApprovalRuleTemplate)

instance Core.FromJSON OriginApprovalRuleTemplate where
  parseJSON =
    Core.withObject
      "OriginApprovalRuleTemplate"
      ( \x ->
          OriginApprovalRuleTemplate'
            Core.<$> (x Core..:? "approvalRuleTemplateId")
            Core.<*> (x Core..:? "approvalRuleTemplateName")
      )

instance Core.Hashable OriginApprovalRuleTemplate

instance Core.NFData OriginApprovalRuleTemplate
