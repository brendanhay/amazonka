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
-- Module      : Amazonka.Macie.Types.S3ResourceClassificationUpdate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Macie.Types.S3ResourceClassificationUpdate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Macie.Types.ClassificationTypeUpdate
import qualified Amazonka.Prelude as Prelude

-- | (Discontinued) The S3 resources whose classification types you want to
-- update. This data type is used as a request parameter in the
-- @UpdateS3Resources@ action.
--
-- /See:/ 'newS3ResourceClassificationUpdate' smart constructor.
data S3ResourceClassificationUpdate = S3ResourceClassificationUpdate'
  { -- | (Discontinued) The prefix of the S3 bucket whose classification types
    -- you want to update.
    prefix :: Prelude.Maybe Prelude.Text,
    -- | (Discontinued) The name of the S3 bucket whose classification types you
    -- want to update.
    bucketName :: Prelude.Text,
    -- | (Discontinued) The classification type that you want to update for the
    -- resource associated with Amazon Macie Classic.
    classificationTypeUpdate :: ClassificationTypeUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3ResourceClassificationUpdate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefix', 's3ResourceClassificationUpdate_prefix' - (Discontinued) The prefix of the S3 bucket whose classification types
-- you want to update.
--
-- 'bucketName', 's3ResourceClassificationUpdate_bucketName' - (Discontinued) The name of the S3 bucket whose classification types you
-- want to update.
--
-- 'classificationTypeUpdate', 's3ResourceClassificationUpdate_classificationTypeUpdate' - (Discontinued) The classification type that you want to update for the
-- resource associated with Amazon Macie Classic.
newS3ResourceClassificationUpdate ::
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'classificationTypeUpdate'
  ClassificationTypeUpdate ->
  S3ResourceClassificationUpdate
newS3ResourceClassificationUpdate
  pBucketName_
  pClassificationTypeUpdate_ =
    S3ResourceClassificationUpdate'
      { prefix =
          Prelude.Nothing,
        bucketName = pBucketName_,
        classificationTypeUpdate =
          pClassificationTypeUpdate_
      }

-- | (Discontinued) The prefix of the S3 bucket whose classification types
-- you want to update.
s3ResourceClassificationUpdate_prefix :: Lens.Lens' S3ResourceClassificationUpdate (Prelude.Maybe Prelude.Text)
s3ResourceClassificationUpdate_prefix = Lens.lens (\S3ResourceClassificationUpdate' {prefix} -> prefix) (\s@S3ResourceClassificationUpdate' {} a -> s {prefix = a} :: S3ResourceClassificationUpdate)

-- | (Discontinued) The name of the S3 bucket whose classification types you
-- want to update.
s3ResourceClassificationUpdate_bucketName :: Lens.Lens' S3ResourceClassificationUpdate Prelude.Text
s3ResourceClassificationUpdate_bucketName = Lens.lens (\S3ResourceClassificationUpdate' {bucketName} -> bucketName) (\s@S3ResourceClassificationUpdate' {} a -> s {bucketName = a} :: S3ResourceClassificationUpdate)

-- | (Discontinued) The classification type that you want to update for the
-- resource associated with Amazon Macie Classic.
s3ResourceClassificationUpdate_classificationTypeUpdate :: Lens.Lens' S3ResourceClassificationUpdate ClassificationTypeUpdate
s3ResourceClassificationUpdate_classificationTypeUpdate = Lens.lens (\S3ResourceClassificationUpdate' {classificationTypeUpdate} -> classificationTypeUpdate) (\s@S3ResourceClassificationUpdate' {} a -> s {classificationTypeUpdate = a} :: S3ResourceClassificationUpdate)

instance
  Prelude.Hashable
    S3ResourceClassificationUpdate
  where
  hashWithSalt
    _salt
    S3ResourceClassificationUpdate' {..} =
      _salt `Prelude.hashWithSalt` prefix
        `Prelude.hashWithSalt` bucketName
        `Prelude.hashWithSalt` classificationTypeUpdate

instance
  Prelude.NFData
    S3ResourceClassificationUpdate
  where
  rnf S3ResourceClassificationUpdate' {..} =
    Prelude.rnf prefix
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf classificationTypeUpdate

instance Data.ToJSON S3ResourceClassificationUpdate where
  toJSON S3ResourceClassificationUpdate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("prefix" Data..=) Prelude.<$> prefix,
            Prelude.Just ("bucketName" Data..= bucketName),
            Prelude.Just
              ( "classificationTypeUpdate"
                  Data..= classificationTypeUpdate
              )
          ]
      )
