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
-- Module      : Amazonka.CodeCommit.Types.OriginApprovalRuleTemplate
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodeCommit.Types.OriginApprovalRuleTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns information about the template that created the approval rule
-- for a pull request.
--
-- /See:/ 'newOriginApprovalRuleTemplate' smart constructor.
data OriginApprovalRuleTemplate = OriginApprovalRuleTemplate'
  { -- | The name of the template that created the approval rule.
    approvalRuleTemplateName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the template that created the approval rule.
    approvalRuleTemplateId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OriginApprovalRuleTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approvalRuleTemplateName', 'originApprovalRuleTemplate_approvalRuleTemplateName' - The name of the template that created the approval rule.
--
-- 'approvalRuleTemplateId', 'originApprovalRuleTemplate_approvalRuleTemplateId' - The ID of the template that created the approval rule.
newOriginApprovalRuleTemplate ::
  OriginApprovalRuleTemplate
newOriginApprovalRuleTemplate =
  OriginApprovalRuleTemplate'
    { approvalRuleTemplateName =
        Prelude.Nothing,
      approvalRuleTemplateId = Prelude.Nothing
    }

-- | The name of the template that created the approval rule.
originApprovalRuleTemplate_approvalRuleTemplateName :: Lens.Lens' OriginApprovalRuleTemplate (Prelude.Maybe Prelude.Text)
originApprovalRuleTemplate_approvalRuleTemplateName = Lens.lens (\OriginApprovalRuleTemplate' {approvalRuleTemplateName} -> approvalRuleTemplateName) (\s@OriginApprovalRuleTemplate' {} a -> s {approvalRuleTemplateName = a} :: OriginApprovalRuleTemplate)

-- | The ID of the template that created the approval rule.
originApprovalRuleTemplate_approvalRuleTemplateId :: Lens.Lens' OriginApprovalRuleTemplate (Prelude.Maybe Prelude.Text)
originApprovalRuleTemplate_approvalRuleTemplateId = Lens.lens (\OriginApprovalRuleTemplate' {approvalRuleTemplateId} -> approvalRuleTemplateId) (\s@OriginApprovalRuleTemplate' {} a -> s {approvalRuleTemplateId = a} :: OriginApprovalRuleTemplate)

instance Data.FromJSON OriginApprovalRuleTemplate where
  parseJSON =
    Data.withObject
      "OriginApprovalRuleTemplate"
      ( \x ->
          OriginApprovalRuleTemplate'
            Prelude.<$> (x Data..:? "approvalRuleTemplateName")
            Prelude.<*> (x Data..:? "approvalRuleTemplateId")
      )

instance Prelude.Hashable OriginApprovalRuleTemplate where
  hashWithSalt _salt OriginApprovalRuleTemplate' {..} =
    _salt
      `Prelude.hashWithSalt` approvalRuleTemplateName
      `Prelude.hashWithSalt` approvalRuleTemplateId

instance Prelude.NFData OriginApprovalRuleTemplate where
  rnf OriginApprovalRuleTemplate' {..} =
    Prelude.rnf approvalRuleTemplateName
      `Prelude.seq` Prelude.rnf approvalRuleTemplateId
