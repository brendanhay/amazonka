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
-- Module      : Amazonka.ConnectCases.Types.CaseSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.CaseSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Case summary information.
--
-- /See:/ 'newCaseSummary' smart constructor.
data CaseSummary = CaseSummary'
  { -- | A unique identifier of the case.
    caseId :: Prelude.Text,
    -- | A unique identifier of a template.
    templateId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CaseSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'caseId', 'caseSummary_caseId' - A unique identifier of the case.
--
-- 'templateId', 'caseSummary_templateId' - A unique identifier of a template.
newCaseSummary ::
  -- | 'caseId'
  Prelude.Text ->
  -- | 'templateId'
  Prelude.Text ->
  CaseSummary
newCaseSummary pCaseId_ pTemplateId_ =
  CaseSummary'
    { caseId = pCaseId_,
      templateId = pTemplateId_
    }

-- | A unique identifier of the case.
caseSummary_caseId :: Lens.Lens' CaseSummary Prelude.Text
caseSummary_caseId = Lens.lens (\CaseSummary' {caseId} -> caseId) (\s@CaseSummary' {} a -> s {caseId = a} :: CaseSummary)

-- | A unique identifier of a template.
caseSummary_templateId :: Lens.Lens' CaseSummary Prelude.Text
caseSummary_templateId = Lens.lens (\CaseSummary' {templateId} -> templateId) (\s@CaseSummary' {} a -> s {templateId = a} :: CaseSummary)

instance Data.FromJSON CaseSummary where
  parseJSON =
    Data.withObject
      "CaseSummary"
      ( \x ->
          CaseSummary'
            Prelude.<$> (x Data..: "caseId")
            Prelude.<*> (x Data..: "templateId")
      )

instance Prelude.Hashable CaseSummary where
  hashWithSalt _salt CaseSummary' {..} =
    _salt
      `Prelude.hashWithSalt` caseId
      `Prelude.hashWithSalt` templateId

instance Prelude.NFData CaseSummary where
  rnf CaseSummary' {..} =
    Prelude.rnf caseId
      `Prelude.seq` Prelude.rnf templateId
