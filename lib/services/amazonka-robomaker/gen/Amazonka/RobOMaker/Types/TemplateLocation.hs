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
-- Module      : Amazonka.RobOMaker.Types.TemplateLocation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RobOMaker.Types.TemplateLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a template location.
--
-- /See:/ 'newTemplateLocation' smart constructor.
data TemplateLocation = TemplateLocation'
  { -- | The Amazon S3 bucket name.
    s3Bucket :: Prelude.Text,
    -- | The list of S3 keys identifying the data source files.
    s3Key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'templateLocation_s3Bucket' - The Amazon S3 bucket name.
--
-- 's3Key', 'templateLocation_s3Key' - The list of S3 keys identifying the data source files.
newTemplateLocation ::
  -- | 's3Bucket'
  Prelude.Text ->
  -- | 's3Key'
  Prelude.Text ->
  TemplateLocation
newTemplateLocation pS3Bucket_ pS3Key_ =
  TemplateLocation'
    { s3Bucket = pS3Bucket_,
      s3Key = pS3Key_
    }

-- | The Amazon S3 bucket name.
templateLocation_s3Bucket :: Lens.Lens' TemplateLocation Prelude.Text
templateLocation_s3Bucket = Lens.lens (\TemplateLocation' {s3Bucket} -> s3Bucket) (\s@TemplateLocation' {} a -> s {s3Bucket = a} :: TemplateLocation)

-- | The list of S3 keys identifying the data source files.
templateLocation_s3Key :: Lens.Lens' TemplateLocation Prelude.Text
templateLocation_s3Key = Lens.lens (\TemplateLocation' {s3Key} -> s3Key) (\s@TemplateLocation' {} a -> s {s3Key = a} :: TemplateLocation)

instance Prelude.Hashable TemplateLocation where
  hashWithSalt _salt TemplateLocation' {..} =
    _salt
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Key

instance Prelude.NFData TemplateLocation where
  rnf TemplateLocation' {..} =
    Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Key

instance Data.ToJSON TemplateLocation where
  toJSON TemplateLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("s3Bucket" Data..= s3Bucket),
            Prelude.Just ("s3Key" Data..= s3Key)
          ]
      )
