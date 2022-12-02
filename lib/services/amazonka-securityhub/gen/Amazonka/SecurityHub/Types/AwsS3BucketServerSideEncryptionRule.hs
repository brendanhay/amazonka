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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketServerSideEncryptionRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketServerSideEncryptionRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsS3BucketServerSideEncryptionByDefault

-- | An encryption rule to apply to the S3 bucket.
--
-- /See:/ 'newAwsS3BucketServerSideEncryptionRule' smart constructor.
data AwsS3BucketServerSideEncryptionRule = AwsS3BucketServerSideEncryptionRule'
  { -- | Specifies the default server-side encryption to apply to new objects in
    -- the bucket. If a @PUT@ object request doesn\'t specify any server-side
    -- encryption, this default encryption is applied.
    applyServerSideEncryptionByDefault :: Prelude.Maybe AwsS3BucketServerSideEncryptionByDefault
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketServerSideEncryptionRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applyServerSideEncryptionByDefault', 'awsS3BucketServerSideEncryptionRule_applyServerSideEncryptionByDefault' - Specifies the default server-side encryption to apply to new objects in
-- the bucket. If a @PUT@ object request doesn\'t specify any server-side
-- encryption, this default encryption is applied.
newAwsS3BucketServerSideEncryptionRule ::
  AwsS3BucketServerSideEncryptionRule
newAwsS3BucketServerSideEncryptionRule =
  AwsS3BucketServerSideEncryptionRule'
    { applyServerSideEncryptionByDefault =
        Prelude.Nothing
    }

-- | Specifies the default server-side encryption to apply to new objects in
-- the bucket. If a @PUT@ object request doesn\'t specify any server-side
-- encryption, this default encryption is applied.
awsS3BucketServerSideEncryptionRule_applyServerSideEncryptionByDefault :: Lens.Lens' AwsS3BucketServerSideEncryptionRule (Prelude.Maybe AwsS3BucketServerSideEncryptionByDefault)
awsS3BucketServerSideEncryptionRule_applyServerSideEncryptionByDefault = Lens.lens (\AwsS3BucketServerSideEncryptionRule' {applyServerSideEncryptionByDefault} -> applyServerSideEncryptionByDefault) (\s@AwsS3BucketServerSideEncryptionRule' {} a -> s {applyServerSideEncryptionByDefault = a} :: AwsS3BucketServerSideEncryptionRule)

instance
  Data.FromJSON
    AwsS3BucketServerSideEncryptionRule
  where
  parseJSON =
    Data.withObject
      "AwsS3BucketServerSideEncryptionRule"
      ( \x ->
          AwsS3BucketServerSideEncryptionRule'
            Prelude.<$> (x Data..:? "ApplyServerSideEncryptionByDefault")
      )

instance
  Prelude.Hashable
    AwsS3BucketServerSideEncryptionRule
  where
  hashWithSalt
    _salt
    AwsS3BucketServerSideEncryptionRule' {..} =
      _salt
        `Prelude.hashWithSalt` applyServerSideEncryptionByDefault

instance
  Prelude.NFData
    AwsS3BucketServerSideEncryptionRule
  where
  rnf AwsS3BucketServerSideEncryptionRule' {..} =
    Prelude.rnf applyServerSideEncryptionByDefault

instance
  Data.ToJSON
    AwsS3BucketServerSideEncryptionRule
  where
  toJSON AwsS3BucketServerSideEncryptionRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ApplyServerSideEncryptionByDefault" Data..=)
              Prelude.<$> applyServerSideEncryptionByDefault
          ]
      )
