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
-- Module      : Amazonka.AmplifyBackend.Types.S3BucketInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.S3BucketInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the metadata of the S3 bucket.
--
-- /See:/ 'newS3BucketInfo' smart constructor.
data S3BucketInfo = S3BucketInfo'
  { -- | The name of the S3 bucket.
    name :: Prelude.Maybe Prelude.Text,
    -- | The creation date of the S3 bucket.
    creationDate :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3BucketInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 's3BucketInfo_name' - The name of the S3 bucket.
--
-- 'creationDate', 's3BucketInfo_creationDate' - The creation date of the S3 bucket.
newS3BucketInfo ::
  S3BucketInfo
newS3BucketInfo =
  S3BucketInfo'
    { name = Prelude.Nothing,
      creationDate = Prelude.Nothing
    }

-- | The name of the S3 bucket.
s3BucketInfo_name :: Lens.Lens' S3BucketInfo (Prelude.Maybe Prelude.Text)
s3BucketInfo_name = Lens.lens (\S3BucketInfo' {name} -> name) (\s@S3BucketInfo' {} a -> s {name = a} :: S3BucketInfo)

-- | The creation date of the S3 bucket.
s3BucketInfo_creationDate :: Lens.Lens' S3BucketInfo (Prelude.Maybe Prelude.Text)
s3BucketInfo_creationDate = Lens.lens (\S3BucketInfo' {creationDate} -> creationDate) (\s@S3BucketInfo' {} a -> s {creationDate = a} :: S3BucketInfo)

instance Data.FromJSON S3BucketInfo where
  parseJSON =
    Data.withObject
      "S3BucketInfo"
      ( \x ->
          S3BucketInfo'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "creationDate")
      )

instance Prelude.Hashable S3BucketInfo where
  hashWithSalt _salt S3BucketInfo' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` creationDate

instance Prelude.NFData S3BucketInfo where
  rnf S3BucketInfo' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf creationDate
