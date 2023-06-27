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
-- Module      : Amazonka.Comprehend.Types.DatasetEntityRecognizerAnnotations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DatasetEntityRecognizerAnnotations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the annotations associated with a entity recognizer.
--
-- /See:/ 'newDatasetEntityRecognizerAnnotations' smart constructor.
data DatasetEntityRecognizerAnnotations = DatasetEntityRecognizerAnnotations'
  { -- | Specifies the Amazon S3 location where the training documents for an
    -- entity recognizer are located. The URI must be in the same Region as the
    -- API endpoint that you are calling.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetEntityRecognizerAnnotations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Uri', 'datasetEntityRecognizerAnnotations_s3Uri' - Specifies the Amazon S3 location where the training documents for an
-- entity recognizer are located. The URI must be in the same Region as the
-- API endpoint that you are calling.
newDatasetEntityRecognizerAnnotations ::
  -- | 's3Uri'
  Prelude.Text ->
  DatasetEntityRecognizerAnnotations
newDatasetEntityRecognizerAnnotations pS3Uri_ =
  DatasetEntityRecognizerAnnotations'
    { s3Uri =
        pS3Uri_
    }

-- | Specifies the Amazon S3 location where the training documents for an
-- entity recognizer are located. The URI must be in the same Region as the
-- API endpoint that you are calling.
datasetEntityRecognizerAnnotations_s3Uri :: Lens.Lens' DatasetEntityRecognizerAnnotations Prelude.Text
datasetEntityRecognizerAnnotations_s3Uri = Lens.lens (\DatasetEntityRecognizerAnnotations' {s3Uri} -> s3Uri) (\s@DatasetEntityRecognizerAnnotations' {} a -> s {s3Uri = a} :: DatasetEntityRecognizerAnnotations)

instance
  Prelude.Hashable
    DatasetEntityRecognizerAnnotations
  where
  hashWithSalt
    _salt
    DatasetEntityRecognizerAnnotations' {..} =
      _salt `Prelude.hashWithSalt` s3Uri

instance
  Prelude.NFData
    DatasetEntityRecognizerAnnotations
  where
  rnf DatasetEntityRecognizerAnnotations' {..} =
    Prelude.rnf s3Uri

instance
  Data.ToJSON
    DatasetEntityRecognizerAnnotations
  where
  toJSON DatasetEntityRecognizerAnnotations' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3Uri" Data..= s3Uri)]
      )
