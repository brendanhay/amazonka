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
-- Module      : Amazonka.SageMaker.Types.SourceAlgorithm
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.SourceAlgorithm where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Specifies an algorithm that was used to create the model package. The
-- algorithm must be either an algorithm resource in your SageMaker account
-- or an algorithm in Amazon Web Services Marketplace that you are
-- subscribed to.
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
    -- algorithm must be either an algorithm resource in your SageMaker account
    -- or an algorithm in Amazon Web Services Marketplace that you are
    -- subscribed to.
    algorithmName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- algorithm must be either an algorithm resource in your SageMaker account
-- or an algorithm in Amazon Web Services Marketplace that you are
-- subscribed to.
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
-- algorithm must be either an algorithm resource in your SageMaker account
-- or an algorithm in Amazon Web Services Marketplace that you are
-- subscribed to.
sourceAlgorithm_algorithmName :: Lens.Lens' SourceAlgorithm Prelude.Text
sourceAlgorithm_algorithmName = Lens.lens (\SourceAlgorithm' {algorithmName} -> algorithmName) (\s@SourceAlgorithm' {} a -> s {algorithmName = a} :: SourceAlgorithm)

instance Core.FromJSON SourceAlgorithm where
  parseJSON =
    Core.withObject
      "SourceAlgorithm"
      ( \x ->
          SourceAlgorithm'
            Prelude.<$> (x Core..:? "ModelDataUrl")
            Prelude.<*> (x Core..: "AlgorithmName")
      )

instance Prelude.Hashable SourceAlgorithm where
  hashWithSalt _salt SourceAlgorithm' {..} =
    _salt `Prelude.hashWithSalt` modelDataUrl
      `Prelude.hashWithSalt` algorithmName

instance Prelude.NFData SourceAlgorithm where
  rnf SourceAlgorithm' {..} =
    Prelude.rnf modelDataUrl
      `Prelude.seq` Prelude.rnf algorithmName

instance Core.ToJSON SourceAlgorithm where
  toJSON SourceAlgorithm' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ModelDataUrl" Core..=) Prelude.<$> modelDataUrl,
            Prelude.Just
              ("AlgorithmName" Core..= algorithmName)
          ]
      )
