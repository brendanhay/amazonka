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
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerDocuments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerDocuments where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the training documents submitted with an entity recognizer.
--
-- /See:/ 'newEntityRecognizerDocuments' smart constructor.
data EntityRecognizerDocuments = EntityRecognizerDocuments'
  { -- | Specifies the Amazon S3 location where the training documents for an
    -- entity recognizer are located. The URI must be in the same region as the
    -- API endpoint that you are calling.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EntityRecognizerDocuments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Uri', 'entityRecognizerDocuments_s3Uri' - Specifies the Amazon S3 location where the training documents for an
-- entity recognizer are located. The URI must be in the same region as the
-- API endpoint that you are calling.
newEntityRecognizerDocuments ::
  -- | 's3Uri'
  Prelude.Text ->
  EntityRecognizerDocuments
newEntityRecognizerDocuments pS3Uri_ =
  EntityRecognizerDocuments' {s3Uri = pS3Uri_}

-- | Specifies the Amazon S3 location where the training documents for an
-- entity recognizer are located. The URI must be in the same region as the
-- API endpoint that you are calling.
entityRecognizerDocuments_s3Uri :: Lens.Lens' EntityRecognizerDocuments Prelude.Text
entityRecognizerDocuments_s3Uri = Lens.lens (\EntityRecognizerDocuments' {s3Uri} -> s3Uri) (\s@EntityRecognizerDocuments' {} a -> s {s3Uri = a} :: EntityRecognizerDocuments)

instance Prelude.FromJSON EntityRecognizerDocuments where
  parseJSON =
    Prelude.withObject
      "EntityRecognizerDocuments"
      ( \x ->
          EntityRecognizerDocuments'
            Prelude.<$> (x Prelude..: "S3Uri")
      )

instance Prelude.Hashable EntityRecognizerDocuments

instance Prelude.NFData EntityRecognizerDocuments

instance Prelude.ToJSON EntityRecognizerDocuments where
  toJSON EntityRecognizerDocuments' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3Uri" Prelude..= s3Uri)]
      )
