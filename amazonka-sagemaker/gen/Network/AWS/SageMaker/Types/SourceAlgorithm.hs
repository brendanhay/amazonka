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
-- Module      : Network.AWS.SageMaker.Types.SourceAlgorithm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SourceAlgorithm where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies an algorithm that was used to create the model package. The
-- algorithm must be either an algorithm resource in your Amazon SageMaker
-- account or an algorithm in AWS Marketplace that you are subscribed to.
--
-- /See:/ 'newSourceAlgorithm' smart constructor.
data SourceAlgorithm = SourceAlgorithm'
  { -- | The Amazon S3 path where the model artifacts, which result from model
    -- training, are stored. This path must point to a single @gzip@ compressed
    -- tar archive (@.tar.gz@ suffix).
    --
    -- The model artifacts must be in an S3 bucket that is in the same region
    -- as the algorithm.
    modelDataUrl :: Core.Maybe Core.Text,
    -- | The name of an algorithm that was used to create the model package. The
    -- algorithm must be either an algorithm resource in your Amazon SageMaker
    -- account or an algorithm in AWS Marketplace that you are subscribed to.
    algorithmName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SourceAlgorithm' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'modelDataUrl', 'sourceAlgorithm_modelDataUrl' - The Amazon S3 path where the model artifacts, which result from model
-- training, are stored. This path must point to a single @gzip@ compressed
-- tar archive (@.tar.gz@ suffix).
--
-- The model artifacts must be in an S3 bucket that is in the same region
-- as the algorithm.
--
-- 'algorithmName', 'sourceAlgorithm_algorithmName' - The name of an algorithm that was used to create the model package. The
-- algorithm must be either an algorithm resource in your Amazon SageMaker
-- account or an algorithm in AWS Marketplace that you are subscribed to.
newSourceAlgorithm ::
  -- | 'algorithmName'
  Core.Text ->
  SourceAlgorithm
newSourceAlgorithm pAlgorithmName_ =
  SourceAlgorithm'
    { modelDataUrl = Core.Nothing,
      algorithmName = pAlgorithmName_
    }

-- | The Amazon S3 path where the model artifacts, which result from model
-- training, are stored. This path must point to a single @gzip@ compressed
-- tar archive (@.tar.gz@ suffix).
--
-- The model artifacts must be in an S3 bucket that is in the same region
-- as the algorithm.
sourceAlgorithm_modelDataUrl :: Lens.Lens' SourceAlgorithm (Core.Maybe Core.Text)
sourceAlgorithm_modelDataUrl = Lens.lens (\SourceAlgorithm' {modelDataUrl} -> modelDataUrl) (\s@SourceAlgorithm' {} a -> s {modelDataUrl = a} :: SourceAlgorithm)

-- | The name of an algorithm that was used to create the model package. The
-- algorithm must be either an algorithm resource in your Amazon SageMaker
-- account or an algorithm in AWS Marketplace that you are subscribed to.
sourceAlgorithm_algorithmName :: Lens.Lens' SourceAlgorithm Core.Text
sourceAlgorithm_algorithmName = Lens.lens (\SourceAlgorithm' {algorithmName} -> algorithmName) (\s@SourceAlgorithm' {} a -> s {algorithmName = a} :: SourceAlgorithm)

instance Core.FromJSON SourceAlgorithm where
  parseJSON =
    Core.withObject
      "SourceAlgorithm"
      ( \x ->
          SourceAlgorithm'
            Core.<$> (x Core..:? "ModelDataUrl")
            Core.<*> (x Core..: "AlgorithmName")
      )

instance Core.Hashable SourceAlgorithm

instance Core.NFData SourceAlgorithm

instance Core.ToJSON SourceAlgorithm where
  toJSON SourceAlgorithm' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ModelDataUrl" Core..=) Core.<$> modelDataUrl,
            Core.Just ("AlgorithmName" Core..= algorithmName)
          ]
      )
