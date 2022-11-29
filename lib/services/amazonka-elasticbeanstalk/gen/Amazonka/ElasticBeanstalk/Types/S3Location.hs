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
-- Module      : Amazonka.ElasticBeanstalk.Types.S3Location
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticBeanstalk.Types.S3Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The bucket and key of an item stored in Amazon S3.
--
-- /See:/ 'newS3Location' smart constructor.
data S3Location = S3Location'
  { -- | The Amazon S3 bucket where the data is located.
    s3Bucket :: Prelude.Maybe Prelude.Text,
    -- | The Amazon S3 key where the data is located.
    s3Key :: Prelude.Maybe Prelude.Text
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
-- 's3Bucket', 's3Location_s3Bucket' - The Amazon S3 bucket where the data is located.
--
-- 's3Key', 's3Location_s3Key' - The Amazon S3 key where the data is located.
newS3Location ::
  S3Location
newS3Location =
  S3Location'
    { s3Bucket = Prelude.Nothing,
      s3Key = Prelude.Nothing
    }

-- | The Amazon S3 bucket where the data is located.
s3Location_s3Bucket :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_s3Bucket = Lens.lens (\S3Location' {s3Bucket} -> s3Bucket) (\s@S3Location' {} a -> s {s3Bucket = a} :: S3Location)

-- | The Amazon S3 key where the data is located.
s3Location_s3Key :: Lens.Lens' S3Location (Prelude.Maybe Prelude.Text)
s3Location_s3Key = Lens.lens (\S3Location' {s3Key} -> s3Key) (\s@S3Location' {} a -> s {s3Key = a} :: S3Location)

instance Core.FromXML S3Location where
  parseXML x =
    S3Location'
      Prelude.<$> (x Core..@? "S3Bucket")
      Prelude.<*> (x Core..@? "S3Key")

instance Prelude.Hashable S3Location where
  hashWithSalt _salt S3Location' {..} =
    _salt `Prelude.hashWithSalt` s3Bucket
      `Prelude.hashWithSalt` s3Key

instance Prelude.NFData S3Location where
  rnf S3Location' {..} =
    Prelude.rnf s3Bucket
      `Prelude.seq` Prelude.rnf s3Key

instance Core.ToQuery S3Location where
  toQuery S3Location' {..} =
    Prelude.mconcat
      ["S3Bucket" Core.=: s3Bucket, "S3Key" Core.=: s3Key]
