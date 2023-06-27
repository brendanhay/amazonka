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
-- Module      : Amazonka.Comprehend.Types.DatasetEntityRecognizerEntityList
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.DatasetEntityRecognizerEntityList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the dataset entity list for an entity recognizer model.
--
-- For more information on how the input file is formatted, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/prep-training-data-cer.html Preparing training data>
-- in the Comprehend Developer Guide.
--
-- /See:/ 'newDatasetEntityRecognizerEntityList' smart constructor.
data DatasetEntityRecognizerEntityList = DatasetEntityRecognizerEntityList'
  { -- | Specifies the Amazon S3 location where the entity list is located.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DatasetEntityRecognizerEntityList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Uri', 'datasetEntityRecognizerEntityList_s3Uri' - Specifies the Amazon S3 location where the entity list is located.
newDatasetEntityRecognizerEntityList ::
  -- | 's3Uri'
  Prelude.Text ->
  DatasetEntityRecognizerEntityList
newDatasetEntityRecognizerEntityList pS3Uri_ =
  DatasetEntityRecognizerEntityList' {s3Uri = pS3Uri_}

-- | Specifies the Amazon S3 location where the entity list is located.
datasetEntityRecognizerEntityList_s3Uri :: Lens.Lens' DatasetEntityRecognizerEntityList Prelude.Text
datasetEntityRecognizerEntityList_s3Uri = Lens.lens (\DatasetEntityRecognizerEntityList' {s3Uri} -> s3Uri) (\s@DatasetEntityRecognizerEntityList' {} a -> s {s3Uri = a} :: DatasetEntityRecognizerEntityList)

instance
  Prelude.Hashable
    DatasetEntityRecognizerEntityList
  where
  hashWithSalt
    _salt
    DatasetEntityRecognizerEntityList' {..} =
      _salt `Prelude.hashWithSalt` s3Uri

instance
  Prelude.NFData
    DatasetEntityRecognizerEntityList
  where
  rnf DatasetEntityRecognizerEntityList' {..} =
    Prelude.rnf s3Uri

instance
  Data.ToJSON
    DatasetEntityRecognizerEntityList
  where
  toJSON DatasetEntityRecognizerEntityList' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3Uri" Data..= s3Uri)]
      )
