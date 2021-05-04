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
-- Module      : Network.AWS.SageMaker.Types.SourceAlgorithm
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.SourceAlgorithm where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    modelDataUrl :: Prelude.Maybe Prelude.Text,
    -- | The name of an algorithm that was used to create the model package. The
    -- algorithm must be either an algorithm resource in your Amazon SageMaker
    -- account or an algorithm in AWS Marketplace that you are subscribed to.
    algorithmName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  SourceAlgorithm
newSourceAlgorithm pAlgorithmName_ =
  SourceAlgorithm'
    { modelDataUrl = Prelude.Nothing,
      algorithmName = pAlgorithmName_
    }

-- | The Amazon S3 path where the model artifacts, which result from model
-- training, are stored. This path must point to a single @gzip@ compressed
-- tar archive (@.tar.gz@ suffix).
--
-- The model artifacts must be in an S3 bucket that is in the same region
-- as the algorithm.
sourceAlgorithm_modelDataUrl :: Lens.Lens' SourceAlgorithm (Prelude.Maybe Prelude.Text)
sourceAlgorithm_modelDataUrl = Lens.lens (\SourceAlgorithm' {modelDataUrl} -> modelDataUrl) (\s@SourceAlgorithm' {} a -> s {modelDataUrl = a} :: SourceAlgorithm)

-- | The name of an algorithm that was used to create the model package. The
-- algorithm must be either an algorithm resource in your Amazon SageMaker
-- account or an algorithm in AWS Marketplace that you are subscribed to.
sourceAlgorithm_algorithmName :: Lens.Lens' SourceAlgorithm Prelude.Text
sourceAlgorithm_algorithmName = Lens.lens (\SourceAlgorithm' {algorithmName} -> algorithmName) (\s@SourceAlgorithm' {} a -> s {algorithmName = a} :: SourceAlgorithm)

instance Prelude.FromJSON SourceAlgorithm where
  parseJSON =
    Prelude.withObject
      "SourceAlgorithm"
      ( \x ->
          SourceAlgorithm'
            Prelude.<$> (x Prelude..:? "ModelDataUrl")
            Prelude.<*> (x Prelude..: "AlgorithmName")
      )

instance Prelude.Hashable SourceAlgorithm

instance Prelude.NFData SourceAlgorithm

instance Prelude.ToJSON SourceAlgorithm where
  toJSON SourceAlgorithm' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ModelDataUrl" Prelude..=)
              Prelude.<$> modelDataUrl,
            Prelude.Just
              ("AlgorithmName" Prelude..= algorithmName)
          ]
      )
