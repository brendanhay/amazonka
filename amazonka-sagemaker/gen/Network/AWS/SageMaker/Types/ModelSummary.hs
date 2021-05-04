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
-- Module      : Network.AWS.SageMaker.Types.ModelSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ModelSummary where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides summary information about a model.
--
-- /See:/ 'newModelSummary' smart constructor.
data ModelSummary = ModelSummary'
  { -- | The name of the model that you want a summary for.
    modelName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the model.
    modelArn :: Prelude.Text,
    -- | A timestamp that indicates when the model was created.
    creationTime :: Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModelSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelName', 'modelSummary_modelName' - The name of the model that you want a summary for.
--
-- 'modelArn', 'modelSummary_modelArn' - The Amazon Resource Name (ARN) of the model.
--
-- 'creationTime', 'modelSummary_creationTime' - A timestamp that indicates when the model was created.
newModelSummary ::
  -- | 'modelName'
  Prelude.Text ->
  -- | 'modelArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  ModelSummary
newModelSummary pModelName_ pModelArn_ pCreationTime_ =
  ModelSummary'
    { modelName = pModelName_,
      modelArn = pModelArn_,
      creationTime = Prelude._Time Lens.# pCreationTime_
    }

-- | The name of the model that you want a summary for.
modelSummary_modelName :: Lens.Lens' ModelSummary Prelude.Text
modelSummary_modelName = Lens.lens (\ModelSummary' {modelName} -> modelName) (\s@ModelSummary' {} a -> s {modelName = a} :: ModelSummary)

-- | The Amazon Resource Name (ARN) of the model.
modelSummary_modelArn :: Lens.Lens' ModelSummary Prelude.Text
modelSummary_modelArn = Lens.lens (\ModelSummary' {modelArn} -> modelArn) (\s@ModelSummary' {} a -> s {modelArn = a} :: ModelSummary)

-- | A timestamp that indicates when the model was created.
modelSummary_creationTime :: Lens.Lens' ModelSummary Prelude.UTCTime
modelSummary_creationTime = Lens.lens (\ModelSummary' {creationTime} -> creationTime) (\s@ModelSummary' {} a -> s {creationTime = a} :: ModelSummary) Prelude.. Prelude._Time

instance Prelude.FromJSON ModelSummary where
  parseJSON =
    Prelude.withObject
      "ModelSummary"
      ( \x ->
          ModelSummary'
            Prelude.<$> (x Prelude..: "ModelName")
            Prelude.<*> (x Prelude..: "ModelArn")
            Prelude.<*> (x Prelude..: "CreationTime")
      )

instance Prelude.Hashable ModelSummary

instance Prelude.NFData ModelSummary
