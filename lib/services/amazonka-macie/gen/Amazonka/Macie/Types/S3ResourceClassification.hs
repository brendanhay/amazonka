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
-- Module      : Amazonka.Macie.Types.S3ResourceClassification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Macie.Types.S3ResourceClassification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Macie.Types.ClassificationType
import qualified Amazonka.Prelude as Prelude

-- | (Discontinued) The S3 resources that you want to associate with Amazon
-- Macie Classic for monitoring and data classification. This data type is
-- used as a request parameter in the @AssociateS3Resources@ action and a
-- response parameter in the @ListS3Resources@ action.
--
-- /See:/ 'newS3ResourceClassification' smart constructor.
data S3ResourceClassification = S3ResourceClassification'
  { -- | (Discontinued) The prefix of the S3 bucket that you want to associate
    -- with Amazon Macie Classic.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | (Discontinued) The name of the S3 bucket that you want to associate with
    -- Amazon Macie Classic.
    bucketName :: Prelude.Text,
    -- | (Discontinued) The classification type that you want to specify for the
    -- resource associated with Amazon Macie Classic.
    classificationType :: ClassificationType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ResourceClassification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 's3ResourceClassification_prefix' - (Discontinued) The prefix of the S3 bucket that you want to associate
-- with Amazon Macie Classic.
--
-- 'bucketName', 's3ResourceClassification_bucketName' - (Discontinued) The name of the S3 bucket that you want to associate with
-- Amazon Macie Classic.
--
-- 'classificationType', 's3ResourceClassification_classificationType' - (Discontinued) The classification type that you want to specify for the
-- resource associated with Amazon Macie Classic.
newS3ResourceClassification ::
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'classificationType'
  ClassificationType ->
  S3ResourceClassification
newS3ResourceClassification
  pBucketName_
  pClassificationType_ =
    S3ResourceClassification'
      { prefix = Prelude.Nothing,
        bucketName = pBucketName_,
        classificationType = pClassificationType_
      }

-- | (Discontinued) The prefix of the S3 bucket that you want to associate
-- with Amazon Macie Classic.
s3ResourceClassification_prefix :: Lens.Lens' S3ResourceClassification (Prelude.Maybe Prelude.Text)
s3ResourceClassification_prefix = Lens.lens (\S3ResourceClassification' {prefix} -> prefix) (\s@S3ResourceClassification' {} a -> s {prefix = a} :: S3ResourceClassification)

-- | (Discontinued) The name of the S3 bucket that you want to associate with
-- Amazon Macie Classic.
s3ResourceClassification_bucketName :: Lens.Lens' S3ResourceClassification Prelude.Text
s3ResourceClassification_bucketName = Lens.lens (\S3ResourceClassification' {bucketName} -> bucketName) (\s@S3ResourceClassification' {} a -> s {bucketName = a} :: S3ResourceClassification)

-- | (Discontinued) The classification type that you want to specify for the
-- resource associated with Amazon Macie Classic.
s3ResourceClassification_classificationType :: Lens.Lens' S3ResourceClassification ClassificationType
s3ResourceClassification_classificationType = Lens.lens (\S3ResourceClassification' {classificationType} -> classificationType) (\s@S3ResourceClassification' {} a -> s {classificationType = a} :: S3ResourceClassification)

instance Data.FromJSON S3ResourceClassification where
  parseJSON =
    Data.withObject
      "S3ResourceClassification"
      ( \x ->
          S3ResourceClassification'
            Prelude.<$> (x Data..:? "prefix")
            Prelude.<*> (x Data..: "bucketName")
            Prelude.<*> (x Data..: "classificationType")
      )

instance Prelude.Hashable S3ResourceClassification where
  hashWithSalt _salt S3ResourceClassification' {..} =
    _salt `Prelude.hashWithSalt` prefix
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` classificationType

instance Prelude.NFData S3ResourceClassification where
  rnf S3ResourceClassification' {..} =
    Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf classificationType

instance Data.ToJSON S3ResourceClassification where
  toJSON S3ResourceClassification' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("prefix" Data..=) Prelude.<$> prefix,
            Prelude.Just ("bucketName" Data..= bucketName),
            Prelude.Just
              ("classificationType" Data..= classificationType)
          ]
      )
