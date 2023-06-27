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
-- Module      : Amazonka.GuardDuty.Types.S3BucketDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.S3BucketDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GuardDuty.Types.DefaultServerSideEncryption
import Amazonka.GuardDuty.Types.Owner
import Amazonka.GuardDuty.Types.PublicAccess
import Amazonka.GuardDuty.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Contains information on the S3 bucket.
--
-- /See:/ 'newS3BucketDetail' smart constructor.
data S3BucketDetail = S3BucketDetail'
  { -- | The Amazon Resource Name (ARN) of the S3 bucket.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The date and time the bucket was created at.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | Describes the server side encryption method used in the S3 bucket.
    defaultServerSideEncryption :: Prelude.Maybe DefaultServerSideEncryption,
    -- | The name of the S3 bucket.
    name :: Prelude.Maybe Prelude.Text,
    -- | The owner of the S3 bucket.
    owner :: Prelude.Maybe Owner,
    -- | Describes the public access policies that apply to the S3 bucket.
    publicAccess :: Prelude.Maybe PublicAccess,
    -- | All tags attached to the S3 bucket
    tags :: Prelude.Maybe [Tag],
    -- | Describes whether the bucket is a source or destination bucket.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'S3BucketDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 's3BucketDetail_arn' - The Amazon Resource Name (ARN) of the S3 bucket.
--
-- 'createdAt', 's3BucketDetail_createdAt' - The date and time the bucket was created at.
--
-- 'defaultServerSideEncryption', 's3BucketDetail_defaultServerSideEncryption' - Describes the server side encryption method used in the S3 bucket.
--
-- 'name', 's3BucketDetail_name' - The name of the S3 bucket.
--
-- 'owner', 's3BucketDetail_owner' - The owner of the S3 bucket.
--
-- 'publicAccess', 's3BucketDetail_publicAccess' - Describes the public access policies that apply to the S3 bucket.
--
-- 'tags', 's3BucketDetail_tags' - All tags attached to the S3 bucket
--
-- 'type'', 's3BucketDetail_type' - Describes whether the bucket is a source or destination bucket.
newS3BucketDetail ::
  S3BucketDetail
newS3BucketDetail =
  S3BucketDetail'
    { arn = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      defaultServerSideEncryption = Prelude.Nothing,
      name = Prelude.Nothing,
      owner = Prelude.Nothing,
      publicAccess = Prelude.Nothing,
      tags = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the S3 bucket.
s3BucketDetail_arn :: Lens.Lens' S3BucketDetail (Prelude.Maybe Prelude.Text)
s3BucketDetail_arn = Lens.lens (\S3BucketDetail' {arn} -> arn) (\s@S3BucketDetail' {} a -> s {arn = a} :: S3BucketDetail)

-- | The date and time the bucket was created at.
s3BucketDetail_createdAt :: Lens.Lens' S3BucketDetail (Prelude.Maybe Prelude.UTCTime)
s3BucketDetail_createdAt = Lens.lens (\S3BucketDetail' {createdAt} -> createdAt) (\s@S3BucketDetail' {} a -> s {createdAt = a} :: S3BucketDetail) Prelude.. Lens.mapping Data._Time

-- | Describes the server side encryption method used in the S3 bucket.
s3BucketDetail_defaultServerSideEncryption :: Lens.Lens' S3BucketDetail (Prelude.Maybe DefaultServerSideEncryption)
s3BucketDetail_defaultServerSideEncryption = Lens.lens (\S3BucketDetail' {defaultServerSideEncryption} -> defaultServerSideEncryption) (\s@S3BucketDetail' {} a -> s {defaultServerSideEncryption = a} :: S3BucketDetail)

-- | The name of the S3 bucket.
s3BucketDetail_name :: Lens.Lens' S3BucketDetail (Prelude.Maybe Prelude.Text)
s3BucketDetail_name = Lens.lens (\S3BucketDetail' {name} -> name) (\s@S3BucketDetail' {} a -> s {name = a} :: S3BucketDetail)

-- | The owner of the S3 bucket.
s3BucketDetail_owner :: Lens.Lens' S3BucketDetail (Prelude.Maybe Owner)
s3BucketDetail_owner = Lens.lens (\S3BucketDetail' {owner} -> owner) (\s@S3BucketDetail' {} a -> s {owner = a} :: S3BucketDetail)

-- | Describes the public access policies that apply to the S3 bucket.
s3BucketDetail_publicAccess :: Lens.Lens' S3BucketDetail (Prelude.Maybe PublicAccess)
s3BucketDetail_publicAccess = Lens.lens (\S3BucketDetail' {publicAccess} -> publicAccess) (\s@S3BucketDetail' {} a -> s {publicAccess = a} :: S3BucketDetail)

-- | All tags attached to the S3 bucket
s3BucketDetail_tags :: Lens.Lens' S3BucketDetail (Prelude.Maybe [Tag])
s3BucketDetail_tags = Lens.lens (\S3BucketDetail' {tags} -> tags) (\s@S3BucketDetail' {} a -> s {tags = a} :: S3BucketDetail) Prelude.. Lens.mapping Lens.coerced

-- | Describes whether the bucket is a source or destination bucket.
s3BucketDetail_type :: Lens.Lens' S3BucketDetail (Prelude.Maybe Prelude.Text)
s3BucketDetail_type = Lens.lens (\S3BucketDetail' {type'} -> type') (\s@S3BucketDetail' {} a -> s {type' = a} :: S3BucketDetail)

instance Data.FromJSON S3BucketDetail where
  parseJSON =
    Data.withObject
      "S3BucketDetail"
      ( \x ->
          S3BucketDetail'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "defaultServerSideEncryption")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "owner")
            Prelude.<*> (x Data..:? "publicAccess")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "type")
      )

instance Prelude.Hashable S3BucketDetail where
  hashWithSalt _salt S3BucketDetail' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` defaultServerSideEncryption
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` publicAccess
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` type'

instance Prelude.NFData S3BucketDetail where
  rnf S3BucketDetail' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf defaultServerSideEncryption
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf publicAccess
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf type'
