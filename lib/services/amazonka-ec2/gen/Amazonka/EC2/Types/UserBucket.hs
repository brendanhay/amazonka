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
-- Module      : Amazonka.EC2.Types.UserBucket
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.UserBucket where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import qualified Amazonka.Prelude as Prelude

-- | Describes the Amazon S3 bucket for the disk image.
--
-- /See:/ 'newUserBucket' smart constructor.
data UserBucket = UserBucket'
  { -- | The name of the Amazon S3 bucket where the disk image is located.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The file name of the disk image.
    s3Key :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserBucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 's3Bucket', 'userBucket_s3Bucket' - The name of the Amazon S3 bucket where the disk image is located.
--
-- 's3Key', 'userBucket_s3Key' - The file name of the disk image.
newUserBucket ::
  UserBucket
newUserBucket =
  UserBucket'
    { s3Bucket = Prelude.Nothing,
      s3Key = Prelude.Nothing
    }

-- | The name of the Amazon S3 bucket where the disk image is located.
userBucket_s3Bucket :: Lens.Lens' UserBucket (Prelude.Maybe Prelude.Text)
userBucket_s3Bucket = Lens.lens (\UserBucket' {s3Bucket} -> s3Bucket) (\s@UserBucket' {} a -> s {s3Bucket = a} :: UserBucket)

-- | The file name of the disk image.
userBucket_s3Key :: Lens.Lens' UserBucket (Prelude.Maybe Prelude.Text)
userBucket_s3Key = Lens.lens (\UserBucket' {s3Key} -> s3Key) (\s@UserBucket' {} a -> s {s3Key = a} :: UserBucket)

instance Prelude.Hashable UserBucket where
  hashWithSalt _salt UserBucket' {..} =
    _salt
      `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Key

instance Prelude.NFData UserBucket where
  rnf UserBucket' {..} =
    Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Key

instance Data.ToQuery UserBucket where
  toQuery UserBucket' {..} =
    Prelude.mconcat
      ["S3Bucket" Data.=: s3Bucket, "S3Key" Data.=: s3Key]
