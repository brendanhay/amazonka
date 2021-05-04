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
-- Module      : Network.AWS.Comprehend.Types.EntityRecognizerAnnotations
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.EntityRecognizerAnnotations where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the annotations associated with a entity recognizer.
--
-- /See:/ 'newEntityRecognizerAnnotations' smart constructor.
data EntityRecognizerAnnotations = EntityRecognizerAnnotations'
  { -- | Specifies the Amazon S3 location where the annotations for an entity
    -- recognizer are located. The URI must be in the same region as the API
    -- endpoint that you are calling.
    s3Uri :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EntityRecognizerAnnotations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Uri', 'entityRecognizerAnnotations_s3Uri' - Specifies the Amazon S3 location where the annotations for an entity
-- recognizer are located. The URI must be in the same region as the API
-- endpoint that you are calling.
newEntityRecognizerAnnotations ::
  -- | 's3Uri'
  Prelude.Text ->
  EntityRecognizerAnnotations
newEntityRecognizerAnnotations pS3Uri_ =
  EntityRecognizerAnnotations' {s3Uri = pS3Uri_}

-- | Specifies the Amazon S3 location where the annotations for an entity
-- recognizer are located. The URI must be in the same region as the API
-- endpoint that you are calling.
entityRecognizerAnnotations_s3Uri :: Lens.Lens' EntityRecognizerAnnotations Prelude.Text
entityRecognizerAnnotations_s3Uri = Lens.lens (\EntityRecognizerAnnotations' {s3Uri} -> s3Uri) (\s@EntityRecognizerAnnotations' {} a -> s {s3Uri = a} :: EntityRecognizerAnnotations)

instance Prelude.FromJSON EntityRecognizerAnnotations where
  parseJSON =
    Prelude.withObject
      "EntityRecognizerAnnotations"
      ( \x ->
          EntityRecognizerAnnotations'
            Prelude.<$> (x Prelude..: "S3Uri")
      )

instance Prelude.Hashable EntityRecognizerAnnotations

instance Prelude.NFData EntityRecognizerAnnotations

instance Prelude.ToJSON EntityRecognizerAnnotations where
  toJSON EntityRecognizerAnnotations' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("S3Uri" Prelude..= s3Uri)]
      )
