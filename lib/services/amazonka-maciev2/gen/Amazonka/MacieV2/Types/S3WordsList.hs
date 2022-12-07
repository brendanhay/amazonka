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
-- Module      : Amazonka.MacieV2.Types.S3WordsList
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.S3WordsList where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about an S3 object that lists specific text to
-- ignore.
--
-- /See:/ 'newS3WordsList' smart constructor.
data S3WordsList = S3WordsList'
  { -- | The full name of the S3 bucket that contains the object.
    bucketName :: Prelude.Text,
    -- | The full name (key) of the object.
    objectKey :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3WordsList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 's3WordsList_bucketName' - The full name of the S3 bucket that contains the object.
--
-- 'objectKey', 's3WordsList_objectKey' - The full name (key) of the object.
newS3WordsList ::
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'objectKey'
  Prelude.Text ->
  S3WordsList
newS3WordsList pBucketName_ pObjectKey_ =
  S3WordsList'
    { bucketName = pBucketName_,
      objectKey = pObjectKey_
    }

-- | The full name of the S3 bucket that contains the object.
s3WordsList_bucketName :: Lens.Lens' S3WordsList Prelude.Text
s3WordsList_bucketName = Lens.lens (\S3WordsList' {bucketName} -> bucketName) (\s@S3WordsList' {} a -> s {bucketName = a} :: S3WordsList)

-- | The full name (key) of the object.
s3WordsList_objectKey :: Lens.Lens' S3WordsList Prelude.Text
s3WordsList_objectKey = Lens.lens (\S3WordsList' {objectKey} -> objectKey) (\s@S3WordsList' {} a -> s {objectKey = a} :: S3WordsList)

instance Data.FromJSON S3WordsList where
  parseJSON =
    Data.withObject
      "S3WordsList"
      ( \x ->
          S3WordsList'
            Prelude.<$> (x Data..: "bucketName")
            Prelude.<*> (x Data..: "objectKey")
      )

instance Prelude.Hashable S3WordsList where
  hashWithSalt _salt S3WordsList' {..} =
    _salt `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` objectKey

instance Prelude.NFData S3WordsList where
  rnf S3WordsList' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf objectKey

instance Data.ToJSON S3WordsList where
  toJSON S3WordsList' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("bucketName" Data..= bucketName),
            Prelude.Just ("objectKey" Data..= objectKey)
          ]
      )
