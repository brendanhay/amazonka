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
-- Module      : Amazonka.Comprehend.Types.TaskConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.TaskConfig where

import Amazonka.Comprehend.Types.DocumentClassificationConfig
import Amazonka.Comprehend.Types.EntityRecognitionConfig
import Amazonka.Comprehend.Types.LanguageCode
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Configuration about the custom classifier associated with the flywheel.
--
-- /See:/ 'newTaskConfig' smart constructor.
data TaskConfig = TaskConfig'
  { -- | Configuration required for a classification model.
    documentClassificationConfig :: Prelude.Maybe DocumentClassificationConfig,
    -- | Configuration required for an entity recognition model.
    entityRecognitionConfig :: Prelude.Maybe EntityRecognitionConfig,
    -- | Language code for the language that the model supports.
    languageCode :: LanguageCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TaskConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'documentClassificationConfig', 'taskConfig_documentClassificationConfig' - Configuration required for a classification model.
--
-- 'entityRecognitionConfig', 'taskConfig_entityRecognitionConfig' - Configuration required for an entity recognition model.
--
-- 'languageCode', 'taskConfig_languageCode' - Language code for the language that the model supports.
newTaskConfig ::
  -- | 'languageCode'
  LanguageCode ->
  TaskConfig
newTaskConfig pLanguageCode_ =
  TaskConfig'
    { documentClassificationConfig =
        Prelude.Nothing,
      entityRecognitionConfig = Prelude.Nothing,
      languageCode = pLanguageCode_
    }

-- | Configuration required for a classification model.
taskConfig_documentClassificationConfig :: Lens.Lens' TaskConfig (Prelude.Maybe DocumentClassificationConfig)
taskConfig_documentClassificationConfig = Lens.lens (\TaskConfig' {documentClassificationConfig} -> documentClassificationConfig) (\s@TaskConfig' {} a -> s {documentClassificationConfig = a} :: TaskConfig)

-- | Configuration required for an entity recognition model.
taskConfig_entityRecognitionConfig :: Lens.Lens' TaskConfig (Prelude.Maybe EntityRecognitionConfig)
taskConfig_entityRecognitionConfig = Lens.lens (\TaskConfig' {entityRecognitionConfig} -> entityRecognitionConfig) (\s@TaskConfig' {} a -> s {entityRecognitionConfig = a} :: TaskConfig)

-- | Language code for the language that the model supports.
taskConfig_languageCode :: Lens.Lens' TaskConfig LanguageCode
taskConfig_languageCode = Lens.lens (\TaskConfig' {languageCode} -> languageCode) (\s@TaskConfig' {} a -> s {languageCode = a} :: TaskConfig)

instance Data.FromJSON TaskConfig where
  parseJSON =
    Data.withObject
      "TaskConfig"
      ( \x ->
          TaskConfig'
            Prelude.<$> (x Data..:? "DocumentClassificationConfig")
            Prelude.<*> (x Data..:? "EntityRecognitionConfig")
            Prelude.<*> (x Data..: "LanguageCode")
      )

instance Prelude.Hashable TaskConfig where
  hashWithSalt _salt TaskConfig' {..} =
    _salt
      `Prelude.hashWithSalt` documentClassificationConfig
      `Prelude.hashWithSalt` entityRecognitionConfig
      `Prelude.hashWithSalt` languageCode

instance Prelude.NFData TaskConfig where
  rnf TaskConfig' {..} =
    Prelude.rnf documentClassificationConfig
      `Prelude.seq` Prelude.rnf entityRecognitionConfig
      `Prelude.seq` Prelude.rnf languageCode

instance Data.ToJSON TaskConfig where
  toJSON TaskConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DocumentClassificationConfig" Data..=)
              Prelude.<$> documentClassificationConfig,
            ("EntityRecognitionConfig" Data..=)
              Prelude.<$> entityRecognitionConfig,
            Prelude.Just ("LanguageCode" Data..= languageCode)
          ]
      )
