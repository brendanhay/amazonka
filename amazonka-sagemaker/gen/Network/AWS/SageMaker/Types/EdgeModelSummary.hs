{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.EdgeModelSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.EdgeModelSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Summary of model on edge device.
--
-- /See:/ 'newEdgeModelSummary' smart constructor.
data EdgeModelSummary = EdgeModelSummary'
  { -- | The name of the model.
    modelName :: Prelude.Text,
    -- | The version model.
    modelVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON EdgeModelSummary where
  parseJSON =
    Prelude.withObject
      "EdgeModelSummary"
      ( \x ->
          EdgeModelSummary'
            Prelude.<$> (x Prelude..: "ModelName")
            Prelude.<*> (x Prelude..: "ModelVersion")
      )

instance Prelude.Hashable EdgeModelSummary

instance Prelude.NFData EdgeModelSummary
