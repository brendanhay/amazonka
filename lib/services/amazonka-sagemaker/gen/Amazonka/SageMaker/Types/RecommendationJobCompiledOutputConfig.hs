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
-- Module      : Amazonka.SageMaker.Types.RecommendationJobCompiledOutputConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.RecommendationJobCompiledOutputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the output configuration for the compiled
-- model.
--
-- /See:/ 'newRecommendationJobCompiledOutputConfig' smart constructor.
data RecommendationJobCompiledOutputConfig = RecommendationJobCompiledOutputConfig'
  { -- | Identifies the Amazon S3 bucket where you want SageMaker to store the
    -- compiled model artifacts.
    s3OutputUri :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RecommendationJobCompiledOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3OutputUri', 'recommendationJobCompiledOutputConfig_s3OutputUri' - Identifies the Amazon S3 bucket where you want SageMaker to store the
-- compiled model artifacts.
newRecommendationJobCompiledOutputConfig ::
  RecommendationJobCompiledOutputConfig
newRecommendationJobCompiledOutputConfig =
  RecommendationJobCompiledOutputConfig'
    { s3OutputUri =
        Prelude.Nothing
    }

-- | Identifies the Amazon S3 bucket where you want SageMaker to store the
-- compiled model artifacts.
recommendationJobCompiledOutputConfig_s3OutputUri :: Lens.Lens' RecommendationJobCompiledOutputConfig (Prelude.Maybe Prelude.Text)
recommendationJobCompiledOutputConfig_s3OutputUri = Lens.lens (\RecommendationJobCompiledOutputConfig' {s3OutputUri} -> s3OutputUri) (\s@RecommendationJobCompiledOutputConfig' {} a -> s {s3OutputUri = a} :: RecommendationJobCompiledOutputConfig)

instance
  Prelude.Hashable
    RecommendationJobCompiledOutputConfig
  where
  hashWithSalt
    _salt
    RecommendationJobCompiledOutputConfig' {..} =
      _salt `Prelude.hashWithSalt` s3OutputUri

instance
  Prelude.NFData
    RecommendationJobCompiledOutputConfig
  where
  rnf RecommendationJobCompiledOutputConfig' {..} =
    Prelude.rnf s3OutputUri

instance
  Core.ToJSON
    RecommendationJobCompiledOutputConfig
  where
  toJSON RecommendationJobCompiledOutputConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [("S3OutputUri" Core..=) Prelude.<$> s3OutputUri]
      )
