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
-- Module      : Amazonka.SageMaker.Types.TabularResolvedAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TabularResolvedAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.ProblemType

-- | The resolved attributes specific to the @TABULAR@ problem type.
--
-- /See:/ 'newTabularResolvedAttributes' smart constructor.
data TabularResolvedAttributes = TabularResolvedAttributes'
  { -- | The type of supervised learning problem available for the model
    -- candidates of the AutoML job V2 (Binary Classification, Multiclass
    -- Classification, Regression). For more information, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-datasets-problem-types.html#autopilot-problem-types Amazon SageMaker Autopilot problem types>.
    problemType :: Prelude.Maybe ProblemType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TabularResolvedAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'problemType', 'tabularResolvedAttributes_problemType' - The type of supervised learning problem available for the model
-- candidates of the AutoML job V2 (Binary Classification, Multiclass
-- Classification, Regression). For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-datasets-problem-types.html#autopilot-problem-types Amazon SageMaker Autopilot problem types>.
newTabularResolvedAttributes ::
  TabularResolvedAttributes
newTabularResolvedAttributes =
  TabularResolvedAttributes'
    { problemType =
        Prelude.Nothing
    }

-- | The type of supervised learning problem available for the model
-- candidates of the AutoML job V2 (Binary Classification, Multiclass
-- Classification, Regression). For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/autopilot-datasets-problem-types.html#autopilot-problem-types Amazon SageMaker Autopilot problem types>.
tabularResolvedAttributes_problemType :: Lens.Lens' TabularResolvedAttributes (Prelude.Maybe ProblemType)
tabularResolvedAttributes_problemType = Lens.lens (\TabularResolvedAttributes' {problemType} -> problemType) (\s@TabularResolvedAttributes' {} a -> s {problemType = a} :: TabularResolvedAttributes)

instance Data.FromJSON TabularResolvedAttributes where
  parseJSON =
    Data.withObject
      "TabularResolvedAttributes"
      ( \x ->
          TabularResolvedAttributes'
            Prelude.<$> (x Data..:? "ProblemType")
      )

instance Prelude.Hashable TabularResolvedAttributes where
  hashWithSalt _salt TabularResolvedAttributes' {..} =
    _salt `Prelude.hashWithSalt` problemType

instance Prelude.NFData TabularResolvedAttributes where
  rnf TabularResolvedAttributes' {..} =
    Prelude.rnf problemType
