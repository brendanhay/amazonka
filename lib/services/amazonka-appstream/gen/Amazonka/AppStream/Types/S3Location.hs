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
-- Module      : Amazonka.AppStream.Types.S3Location
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppStream.Types.S3Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the S3 location.
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The S3 bucket of the S3 object.
    s3Bucket :: Prelude.Text,
    -- | The S3 key of the S3 object.
    s3Key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 's3Location_s3Bucket' - The S3 bucket of the S3 object.
--
-- 's3Key', 's3Location_s3Key' - The S3 key of the S3 object.
newS3Location ::
  -- | 's3Bucket'
  Prelude.Text ->
  -- | 's3Key'
  Prelude.Text ->
  S3Location
newS3Location pS3Bucket_ pS3Key_ =
  S3Location' {s3Bucket = pS3Bucket_, s3Key = pS3Key_}

-- | The S3 bucket of the S3 object.
s3Location_s3Bucket :: Lens.Lens' S3Location Prelude.Text
s3Location_s3Bucket = Lens.lens (\S3Location' {s3Bucket} -> s3Bucket) (\s@S3Location' {} a -> s {s3Bucket = a} :: S3Location)

-- | The S3 key of the S3 object.
s3Location_s3Key :: Lens.Lens' S3Location Prelude.Text
s3Location_s3Key = Lens.lens (\S3Location' {s3Key} -> s3Key) (\s@S3Location' {} a -> s {s3Key = a} :: S3Location)

instance Data.FromJSON S3Location where
  parseJSON =
    Data.withObject
      "S3Location"
      ( \x ->
          S3Location'
            Prelude.<$> (x Data..: "S3Bucket")
            Prelude.<*> (x Data..: "S3Key")
      )

instance Prelude.Hashable S3Location where
  hashWithSalt _salt S3Location' {..} =
    _salt
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Key

instance Prelude.NFData S3Location where
  rnf S3Location' {..} =
    Prelude.rnf s3Bucket `Prelude.seq`
      Prelude.rnf s3Key

instance Data.ToJSON S3Location where
  toJSON S3Location' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("S3Bucket" Data..= s3Bucket),
            Prelude.Just ("S3Key" Data..= s3Key)
          ]
      )
