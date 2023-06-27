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
-- Module      : Amazonka.Comprehend.Types.DatasetEntityRecognizerInputDataConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DatasetEntityRecognizerInputDataConfig where

import Amazonka.Comprehend.Types.DatasetEntityRecognizerAnnotations
import Amazonka.Comprehend.Types.DatasetEntityRecognizerDocuments
import Amazonka.Comprehend.Types.DatasetEntityRecognizerEntityList
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the format and location of the input data. You must provide
-- either the @Annotations@ parameter or the @EntityList@ parameter.
--
-- /See:/ 'newDatasetEntityRecognizerInputDataConfig' smart constructor.
data DatasetEntityRecognizerInputDataConfig = DatasetEntityRecognizerInputDataConfig'
  { -- | The S3 location of the annotation documents for your custom entity
    -- recognizer.
    annotations :: Prelude.Maybe DatasetEntityRecognizerAnnotations,
    -- | The S3 location of the entity list for your custom entity recognizer.
    entityList :: Prelude.Maybe DatasetEntityRecognizerEntityList,
    -- | The format and location of the training documents for your custom entity
    -- recognizer.
    documents :: DatasetEntityRecognizerDocuments
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetEntityRecognizerInputDataConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'annotations', 'datasetEntityRecognizerInputDataConfig_annotations' - The S3 location of the annotation documents for your custom entity
-- recognizer.
--
-- 'entityList', 'datasetEntityRecognizerInputDataConfig_entityList' - The S3 location of the entity list for your custom entity recognizer.
--
-- 'documents', 'datasetEntityRecognizerInputDataConfig_documents' - The format and location of the training documents for your custom entity
-- recognizer.
newDatasetEntityRecognizerInputDataConfig ::
  -- | 'documents'
  DatasetEntityRecognizerDocuments ->
  DatasetEntityRecognizerInputDataConfig
newDatasetEntityRecognizerInputDataConfig pDocuments_ =
  DatasetEntityRecognizerInputDataConfig'
    { annotations =
        Prelude.Nothing,
      entityList = Prelude.Nothing,
      documents = pDocuments_
    }

-- | The S3 location of the annotation documents for your custom entity
-- recognizer.
datasetEntityRecognizerInputDataConfig_annotations :: Lens.Lens' DatasetEntityRecognizerInputDataConfig (Prelude.Maybe DatasetEntityRecognizerAnnotations)
datasetEntityRecognizerInputDataConfig_annotations = Lens.lens (\DatasetEntityRecognizerInputDataConfig' {annotations} -> annotations) (\s@DatasetEntityRecognizerInputDataConfig' {} a -> s {annotations = a} :: DatasetEntityRecognizerInputDataConfig)

-- | The S3 location of the entity list for your custom entity recognizer.
datasetEntityRecognizerInputDataConfig_entityList :: Lens.Lens' DatasetEntityRecognizerInputDataConfig (Prelude.Maybe DatasetEntityRecognizerEntityList)
datasetEntityRecognizerInputDataConfig_entityList = Lens.lens (\DatasetEntityRecognizerInputDataConfig' {entityList} -> entityList) (\s@DatasetEntityRecognizerInputDataConfig' {} a -> s {entityList = a} :: DatasetEntityRecognizerInputDataConfig)

-- | The format and location of the training documents for your custom entity
-- recognizer.
datasetEntityRecognizerInputDataConfig_documents :: Lens.Lens' DatasetEntityRecognizerInputDataConfig DatasetEntityRecognizerDocuments
datasetEntityRecognizerInputDataConfig_documents = Lens.lens (\DatasetEntityRecognizerInputDataConfig' {documents} -> documents) (\s@DatasetEntityRecognizerInputDataConfig' {} a -> s {documents = a} :: DatasetEntityRecognizerInputDataConfig)

instance
  Prelude.Hashable
    DatasetEntityRecognizerInputDataConfig
  where
  hashWithSalt
    _salt
    DatasetEntityRecognizerInputDataConfig' {..} =
      _salt
        `Prelude.hashWithSalt` annotations
        `Prelude.hashWithSalt` entityList
        `Prelude.hashWithSalt` documents

instance
  Prelude.NFData
    DatasetEntityRecognizerInputDataConfig
  where
  rnf DatasetEntityRecognizerInputDataConfig' {..} =
    Prelude.rnf annotations
      `Prelude.seq` Prelude.rnf entityList
      `Prelude.seq` Prelude.rnf documents

instance
  Data.ToJSON
    DatasetEntityRecognizerInputDataConfig
  where
  toJSON DatasetEntityRecognizerInputDataConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Annotations" Data..=) Prelude.<$> annotations,
            ("EntityList" Data..=) Prelude.<$> entityList,
            Prelude.Just ("Documents" Data..= documents)
          ]
      )
