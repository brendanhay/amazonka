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
-- Module      : Amazonka.QuickSight.Types.AnalysisSourceTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.AnalysisSourceTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataSetReference

-- | The source template of an analysis.
--
-- /See:/ 'newAnalysisSourceTemplate' smart constructor.
data AnalysisSourceTemplate = AnalysisSourceTemplate'
  { -- | The dataset references of the source template of an analysis.
    dataSetReferences :: Prelude.NonEmpty DataSetReference,
    -- | The Amazon Resource Name (ARN) of the source template of an analysis.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnalysisSourceTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dataSetReferences', 'analysisSourceTemplate_dataSetReferences' - The dataset references of the source template of an analysis.
--
-- 'arn', 'analysisSourceTemplate_arn' - The Amazon Resource Name (ARN) of the source template of an analysis.
newAnalysisSourceTemplate ::
  -- | 'dataSetReferences'
  Prelude.NonEmpty DataSetReference ->
  -- | 'arn'
  Prelude.Text ->
  AnalysisSourceTemplate
newAnalysisSourceTemplate pDataSetReferences_ pArn_ =
  AnalysisSourceTemplate'
    { dataSetReferences =
        Lens.coerced Lens.# pDataSetReferences_,
      arn = pArn_
    }

-- | The dataset references of the source template of an analysis.
analysisSourceTemplate_dataSetReferences :: Lens.Lens' AnalysisSourceTemplate (Prelude.NonEmpty DataSetReference)
analysisSourceTemplate_dataSetReferences = Lens.lens (\AnalysisSourceTemplate' {dataSetReferences} -> dataSetReferences) (\s@AnalysisSourceTemplate' {} a -> s {dataSetReferences = a} :: AnalysisSourceTemplate) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of the source template of an analysis.
analysisSourceTemplate_arn :: Lens.Lens' AnalysisSourceTemplate Prelude.Text
analysisSourceTemplate_arn = Lens.lens (\AnalysisSourceTemplate' {arn} -> arn) (\s@AnalysisSourceTemplate' {} a -> s {arn = a} :: AnalysisSourceTemplate)

instance Prelude.Hashable AnalysisSourceTemplate where
  hashWithSalt _salt AnalysisSourceTemplate' {..} =
    _salt `Prelude.hashWithSalt` dataSetReferences
      `Prelude.hashWithSalt` arn

instance Prelude.NFData AnalysisSourceTemplate where
  rnf AnalysisSourceTemplate' {..} =
    Prelude.rnf dataSetReferences
      `Prelude.seq` Prelude.rnf arn

instance Data.ToJSON AnalysisSourceTemplate where
  toJSON AnalysisSourceTemplate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DataSetReferences" Data..= dataSetReferences),
            Prelude.Just ("Arn" Data..= arn)
          ]
      )
