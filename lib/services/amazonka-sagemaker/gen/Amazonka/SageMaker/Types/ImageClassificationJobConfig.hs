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
-- Module      : Amazonka.SageMaker.Types.ImageClassificationJobConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.ImageClassificationJobConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLJobCompletionCriteria

-- | Stores the configuration information for the image classification
-- problem of an AutoML job V2.
--
-- /See:/ 'newImageClassificationJobConfig' smart constructor.
data ImageClassificationJobConfig = ImageClassificationJobConfig'
  { -- | How long a job is allowed to run, or how many candidates a job is
    -- allowed to generate.
    completionCriteria :: Prelude.Maybe AutoMLJobCompletionCriteria
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageClassificationJobConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionCriteria', 'imageClassificationJobConfig_completionCriteria' - How long a job is allowed to run, or how many candidates a job is
-- allowed to generate.
newImageClassificationJobConfig ::
  ImageClassificationJobConfig
newImageClassificationJobConfig =
  ImageClassificationJobConfig'
    { completionCriteria =
        Prelude.Nothing
    }

-- | How long a job is allowed to run, or how many candidates a job is
-- allowed to generate.
imageClassificationJobConfig_completionCriteria :: Lens.Lens' ImageClassificationJobConfig (Prelude.Maybe AutoMLJobCompletionCriteria)
imageClassificationJobConfig_completionCriteria = Lens.lens (\ImageClassificationJobConfig' {completionCriteria} -> completionCriteria) (\s@ImageClassificationJobConfig' {} a -> s {completionCriteria = a} :: ImageClassificationJobConfig)

instance Data.FromJSON ImageClassificationJobConfig where
  parseJSON =
    Data.withObject
      "ImageClassificationJobConfig"
      ( \x ->
          ImageClassificationJobConfig'
            Prelude.<$> (x Data..:? "CompletionCriteria")
      )

instance
  Prelude.Hashable
    ImageClassificationJobConfig
  where
  hashWithSalt _salt ImageClassificationJobConfig' {..} =
    _salt `Prelude.hashWithSalt` completionCriteria

instance Prelude.NFData ImageClassificationJobConfig where
  rnf ImageClassificationJobConfig' {..} =
    Prelude.rnf completionCriteria

instance Data.ToJSON ImageClassificationJobConfig where
  toJSON ImageClassificationJobConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CompletionCriteria" Data..=)
              Prelude.<$> completionCriteria
          ]
      )
