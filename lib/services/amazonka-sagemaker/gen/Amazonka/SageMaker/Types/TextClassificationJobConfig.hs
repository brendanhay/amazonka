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
-- Module      : Amazonka.SageMaker.Types.TextClassificationJobConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.TextClassificationJobConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.AutoMLJobCompletionCriteria

-- | Stores the configuration information for the text classification problem
-- of an AutoML job V2.
--
-- /See:/ 'newTextClassificationJobConfig' smart constructor.
data TextClassificationJobConfig = TextClassificationJobConfig'
  { -- | How long a job is allowed to run, or how many candidates a job is
    -- allowed to generate.
    completionCriteria :: Prelude.Maybe AutoMLJobCompletionCriteria,
    -- | The name of the column used to provide the sentences to be classified.
    -- It should not be the same as the target column (Required).
    contentColumn :: Prelude.Maybe Prelude.Text,
    -- | The name of the column used to provide the class labels. It should not
    -- be same as the content column (Required).
    targetLabelColumn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextClassificationJobConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'completionCriteria', 'textClassificationJobConfig_completionCriteria' - How long a job is allowed to run, or how many candidates a job is
-- allowed to generate.
--
-- 'contentColumn', 'textClassificationJobConfig_contentColumn' - The name of the column used to provide the sentences to be classified.
-- It should not be the same as the target column (Required).
--
-- 'targetLabelColumn', 'textClassificationJobConfig_targetLabelColumn' - The name of the column used to provide the class labels. It should not
-- be same as the content column (Required).
newTextClassificationJobConfig ::
  TextClassificationJobConfig
newTextClassificationJobConfig =
  TextClassificationJobConfig'
    { completionCriteria =
        Prelude.Nothing,
      contentColumn = Prelude.Nothing,
      targetLabelColumn = Prelude.Nothing
    }

-- | How long a job is allowed to run, or how many candidates a job is
-- allowed to generate.
textClassificationJobConfig_completionCriteria :: Lens.Lens' TextClassificationJobConfig (Prelude.Maybe AutoMLJobCompletionCriteria)
textClassificationJobConfig_completionCriteria = Lens.lens (\TextClassificationJobConfig' {completionCriteria} -> completionCriteria) (\s@TextClassificationJobConfig' {} a -> s {completionCriteria = a} :: TextClassificationJobConfig)

-- | The name of the column used to provide the sentences to be classified.
-- It should not be the same as the target column (Required).
textClassificationJobConfig_contentColumn :: Lens.Lens' TextClassificationJobConfig (Prelude.Maybe Prelude.Text)
textClassificationJobConfig_contentColumn = Lens.lens (\TextClassificationJobConfig' {contentColumn} -> contentColumn) (\s@TextClassificationJobConfig' {} a -> s {contentColumn = a} :: TextClassificationJobConfig)

-- | The name of the column used to provide the class labels. It should not
-- be same as the content column (Required).
textClassificationJobConfig_targetLabelColumn :: Lens.Lens' TextClassificationJobConfig (Prelude.Maybe Prelude.Text)
textClassificationJobConfig_targetLabelColumn = Lens.lens (\TextClassificationJobConfig' {targetLabelColumn} -> targetLabelColumn) (\s@TextClassificationJobConfig' {} a -> s {targetLabelColumn = a} :: TextClassificationJobConfig)

instance Data.FromJSON TextClassificationJobConfig where
  parseJSON =
    Data.withObject
      "TextClassificationJobConfig"
      ( \x ->
          TextClassificationJobConfig'
            Prelude.<$> (x Data..:? "CompletionCriteria")
            Prelude.<*> (x Data..:? "ContentColumn")
            Prelude.<*> (x Data..:? "TargetLabelColumn")
      )

instance Prelude.Hashable TextClassificationJobConfig where
  hashWithSalt _salt TextClassificationJobConfig' {..} =
    _salt
      `Prelude.hashWithSalt` completionCriteria
      `Prelude.hashWithSalt` contentColumn
      `Prelude.hashWithSalt` targetLabelColumn

instance Prelude.NFData TextClassificationJobConfig where
  rnf TextClassificationJobConfig' {..} =
    Prelude.rnf completionCriteria
      `Prelude.seq` Prelude.rnf contentColumn
      `Prelude.seq` Prelude.rnf targetLabelColumn

instance Data.ToJSON TextClassificationJobConfig where
  toJSON TextClassificationJobConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CompletionCriteria" Data..=)
              Prelude.<$> completionCriteria,
            ("ContentColumn" Data..=) Prelude.<$> contentColumn,
            ("TargetLabelColumn" Data..=)
              Prelude.<$> targetLabelColumn
          ]
      )
