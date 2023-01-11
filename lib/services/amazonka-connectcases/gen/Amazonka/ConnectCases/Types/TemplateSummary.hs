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
-- Module      : Amazonka.ConnectCases.Types.TemplateSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.TemplateSummary where

import Amazonka.ConnectCases.Types.TemplateStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Template summary information.
--
-- /See:/ 'newTemplateSummary' smart constructor.
data TemplateSummary = TemplateSummary'
  { -- | The template name.
    name :: Prelude.Text,
    -- | The status of the template.
    status :: TemplateStatus,
    -- | The Amazon Resource Name (ARN) of the template.
    templateArn :: Prelude.Text,
    -- | The unique identifier for the template.
    templateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'templateSummary_name' - The template name.
--
-- 'status', 'templateSummary_status' - The status of the template.
--
-- 'templateArn', 'templateSummary_templateArn' - The Amazon Resource Name (ARN) of the template.
--
-- 'templateId', 'templateSummary_templateId' - The unique identifier for the template.
newTemplateSummary ::
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  TemplateStatus ->
  -- | 'templateArn'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  TemplateSummary
newTemplateSummary
  pName_
  pStatus_
  pTemplateArn_
  pTemplateId_ =
    TemplateSummary'
      { name = pName_,
        status = pStatus_,
        templateArn = pTemplateArn_,
        templateId = pTemplateId_
      }

-- | The template name.
templateSummary_name :: Lens.Lens' TemplateSummary Prelude.Text
templateSummary_name = Lens.lens (\TemplateSummary' {name} -> name) (\s@TemplateSummary' {} a -> s {name = a} :: TemplateSummary)

-- | The status of the template.
templateSummary_status :: Lens.Lens' TemplateSummary TemplateStatus
templateSummary_status = Lens.lens (\TemplateSummary' {status} -> status) (\s@TemplateSummary' {} a -> s {status = a} :: TemplateSummary)

-- | The Amazon Resource Name (ARN) of the template.
templateSummary_templateArn :: Lens.Lens' TemplateSummary Prelude.Text
templateSummary_templateArn = Lens.lens (\TemplateSummary' {templateArn} -> templateArn) (\s@TemplateSummary' {} a -> s {templateArn = a} :: TemplateSummary)

-- | The unique identifier for the template.
templateSummary_templateId :: Lens.Lens' TemplateSummary Prelude.Text
templateSummary_templateId = Lens.lens (\TemplateSummary' {templateId} -> templateId) (\s@TemplateSummary' {} a -> s {templateId = a} :: TemplateSummary)

instance Data.FromJSON TemplateSummary where
  parseJSON =
    Data.withObject
      "TemplateSummary"
      ( \x ->
          TemplateSummary'
            Prelude.<$> (x Data..: "name")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "templateArn")
            Prelude.<*> (x Data..: "templateId")
      )

instance Prelude.Hashable TemplateSummary where
  hashWithSalt _salt TemplateSummary' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` templateArn
      `Prelude.hashWithSalt` templateId

instance Prelude.NFData TemplateSummary where
  rnf TemplateSummary' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf templateArn
      `Prelude.seq` Prelude.rnf templateId
