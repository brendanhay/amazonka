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
-- Module      : Amazonka.SageMaker.Types.EdgeModelSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.EdgeModelSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Summary of model on edge device.
--
-- /See:/ 'newEdgeModelSummary' smart constructor.
data EdgeModelSummary = EdgeModelSummary'
  { -- | The name of the model.
    modelName :: Prelude.Text,
    -- | The version model.
    modelVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EdgeModelSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelName', 'edgeModelSummary_modelName' - The name of the model.
--
-- 'modelVersion', 'edgeModelSummary_modelVersion' - The version model.
newEdgeModelSummary ::
  -- | 'modelName'
  Prelude.Text ->
  -- | 'modelVersion'
  Prelude.Text ->
  EdgeModelSummary
newEdgeModelSummary pModelName_ pModelVersion_ =
  EdgeModelSummary'
    { modelName = pModelName_,
      modelVersion = pModelVersion_
    }

-- | The name of the model.
edgeModelSummary_modelName :: Lens.Lens' EdgeModelSummary Prelude.Text
edgeModelSummary_modelName = Lens.lens (\EdgeModelSummary' {modelName} -> modelName) (\s@EdgeModelSummary' {} a -> s {modelName = a} :: EdgeModelSummary)

-- | The version model.
edgeModelSummary_modelVersion :: Lens.Lens' EdgeModelSummary Prelude.Text
edgeModelSummary_modelVersion = Lens.lens (\EdgeModelSummary' {modelVersion} -> modelVersion) (\s@EdgeModelSummary' {} a -> s {modelVersion = a} :: EdgeModelSummary)

instance Core.FromJSON EdgeModelSummary where
  parseJSON =
    Core.withObject
      "EdgeModelSummary"
      ( \x ->
          EdgeModelSummary'
            Prelude.<$> (x Core..: "ModelName")
            Prelude.<*> (x Core..: "ModelVersion")
      )

instance Prelude.Hashable EdgeModelSummary where
  hashWithSalt _salt EdgeModelSummary' {..} =
    _salt `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` modelVersion

instance Prelude.NFData EdgeModelSummary where
  rnf EdgeModelSummary' {..} =
    Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf modelVersion
