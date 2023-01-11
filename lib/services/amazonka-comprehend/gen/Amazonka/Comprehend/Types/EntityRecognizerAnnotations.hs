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
-- Module      : Amazonka.Comprehend.Types.EntityRecognizerAnnotations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Comprehend.Types.EntityRecognizerAnnotations where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the annotations associated with a entity recognizer.
--
-- /See:/ 'newEntityRecognizerAnnotations' smart constructor.
data EntityRecognizerAnnotations = EntityRecognizerAnnotations'
  { -- | Specifies the Amazon S3 location where the test annotations for an
    -- entity recognizer are located. The URI must be in the same region as the
    -- API endpoint that you are calling.
    testS3Uri :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Amazon S3 location where the annotations for an entity
    -- recognizer are located. The URI must be in the same region as the API
    -- endpoint that you are calling.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EntityRecognizerAnnotations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'testS3Uri', 'entityRecognizerAnnotations_testS3Uri' - Specifies the Amazon S3 location where the test annotations for an
-- entity recognizer are located. The URI must be in the same region as the
-- API endpoint that you are calling.
--
-- 's3Uri', 'entityRecognizerAnnotations_s3Uri' - Specifies the Amazon S3 location where the annotations for an entity
-- recognizer are located. The URI must be in the same region as the API
-- endpoint that you are calling.
newEntityRecognizerAnnotations ::
  -- | 's3Uri'
  Prelude.Text ->
  EntityRecognizerAnnotations
newEntityRecognizerAnnotations pS3Uri_ =
  EntityRecognizerAnnotations'
    { testS3Uri =
        Prelude.Nothing,
      s3Uri = pS3Uri_
    }

-- | Specifies the Amazon S3 location where the test annotations for an
-- entity recognizer are located. The URI must be in the same region as the
-- API endpoint that you are calling.
entityRecognizerAnnotations_testS3Uri :: Lens.Lens' EntityRecognizerAnnotations (Prelude.Maybe Prelude.Text)
entityRecognizerAnnotations_testS3Uri = Lens.lens (\EntityRecognizerAnnotations' {testS3Uri} -> testS3Uri) (\s@EntityRecognizerAnnotations' {} a -> s {testS3Uri = a} :: EntityRecognizerAnnotations)

-- | Specifies the Amazon S3 location where the annotations for an entity
-- recognizer are located. The URI must be in the same region as the API
-- endpoint that you are calling.
entityRecognizerAnnotations_s3Uri :: Lens.Lens' EntityRecognizerAnnotations Prelude.Text
entityRecognizerAnnotations_s3Uri = Lens.lens (\EntityRecognizerAnnotations' {s3Uri} -> s3Uri) (\s@EntityRecognizerAnnotations' {} a -> s {s3Uri = a} :: EntityRecognizerAnnotations)

instance Data.FromJSON EntityRecognizerAnnotations where
  parseJSON =
    Data.withObject
      "EntityRecognizerAnnotations"
      ( \x ->
          EntityRecognizerAnnotations'
            Prelude.<$> (x Data..:? "TestS3Uri")
            Prelude.<*> (x Data..: "S3Uri")
      )

instance Prelude.Hashable EntityRecognizerAnnotations where
  hashWithSalt _salt EntityRecognizerAnnotations' {..} =
    _salt `Prelude.hashWithSalt` testS3Uri
      `Prelude.hashWithSalt` s3Uri

instance Prelude.NFData EntityRecognizerAnnotations where
  rnf EntityRecognizerAnnotations' {..} =
    Prelude.rnf testS3Uri
      `Prelude.seq` Prelude.rnf s3Uri

instance Data.ToJSON EntityRecognizerAnnotations where
  toJSON EntityRecognizerAnnotations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TestS3Uri" Data..=) Prelude.<$> testS3Uri,
            Prelude.Just ("S3Uri" Data..= s3Uri)
          ]
      )
