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
-- Module      : Amazonka.S3.Types.ServerSideEncryptionRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.ServerSideEncryptionRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal
import Amazonka.S3.Types.ServerSideEncryptionByDefault

-- | Specifies the default server-side encryption configuration.
--
-- /See:/ 'newServerSideEncryptionRule' smart constructor.
data ServerSideEncryptionRule = ServerSideEncryptionRule'
  { -- | Specifies the default server-side encryption to apply to new objects in
    -- the bucket. If a PUT Object request doesn\'t specify any server-side
    -- encryption, this default encryption will be applied.
    applyServerSideEncryptionByDefault :: Prelude.Maybe ServerSideEncryptionByDefault,
    -- | Specifies whether Amazon S3 should use an S3 Bucket Key with server-side
    -- encryption using KMS (SSE-KMS) for new objects in the bucket. Existing
    -- objects are not affected. Setting the @BucketKeyEnabled@ element to
    -- @true@ causes Amazon S3 to use an S3 Bucket Key. By default, S3 Bucket
    -- Key is not enabled.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-key.html Amazon S3 Bucket Keys>
    -- in the /Amazon S3 User Guide/.
    bucketKeyEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ServerSideEncryptionRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applyServerSideEncryptionByDefault', 'serverSideEncryptionRule_applyServerSideEncryptionByDefault' - Specifies the default server-side encryption to apply to new objects in
-- the bucket. If a PUT Object request doesn\'t specify any server-side
-- encryption, this default encryption will be applied.
--
-- 'bucketKeyEnabled', 'serverSideEncryptionRule_bucketKeyEnabled' - Specifies whether Amazon S3 should use an S3 Bucket Key with server-side
-- encryption using KMS (SSE-KMS) for new objects in the bucket. Existing
-- objects are not affected. Setting the @BucketKeyEnabled@ element to
-- @true@ causes Amazon S3 to use an S3 Bucket Key. By default, S3 Bucket
-- Key is not enabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-key.html Amazon S3 Bucket Keys>
-- in the /Amazon S3 User Guide/.
newServerSideEncryptionRule ::
  ServerSideEncryptionRule
newServerSideEncryptionRule =
  ServerSideEncryptionRule'
    { applyServerSideEncryptionByDefault =
        Prelude.Nothing,
      bucketKeyEnabled = Prelude.Nothing
    }

-- | Specifies the default server-side encryption to apply to new objects in
-- the bucket. If a PUT Object request doesn\'t specify any server-side
-- encryption, this default encryption will be applied.
serverSideEncryptionRule_applyServerSideEncryptionByDefault :: Lens.Lens' ServerSideEncryptionRule (Prelude.Maybe ServerSideEncryptionByDefault)
serverSideEncryptionRule_applyServerSideEncryptionByDefault = Lens.lens (\ServerSideEncryptionRule' {applyServerSideEncryptionByDefault} -> applyServerSideEncryptionByDefault) (\s@ServerSideEncryptionRule' {} a -> s {applyServerSideEncryptionByDefault = a} :: ServerSideEncryptionRule)

-- | Specifies whether Amazon S3 should use an S3 Bucket Key with server-side
-- encryption using KMS (SSE-KMS) for new objects in the bucket. Existing
-- objects are not affected. Setting the @BucketKeyEnabled@ element to
-- @true@ causes Amazon S3 to use an S3 Bucket Key. By default, S3 Bucket
-- Key is not enabled.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-key.html Amazon S3 Bucket Keys>
-- in the /Amazon S3 User Guide/.
serverSideEncryptionRule_bucketKeyEnabled :: Lens.Lens' ServerSideEncryptionRule (Prelude.Maybe Prelude.Bool)
serverSideEncryptionRule_bucketKeyEnabled = Lens.lens (\ServerSideEncryptionRule' {bucketKeyEnabled} -> bucketKeyEnabled) (\s@ServerSideEncryptionRule' {} a -> s {bucketKeyEnabled = a} :: ServerSideEncryptionRule)

instance Data.FromXML ServerSideEncryptionRule where
  parseXML x =
    ServerSideEncryptionRule'
      Prelude.<$> (x Data..@? "ApplyServerSideEncryptionByDefault")
      Prelude.<*> (x Data..@? "BucketKeyEnabled")

instance Prelude.Hashable ServerSideEncryptionRule where
  hashWithSalt _salt ServerSideEncryptionRule' {..} =
    _salt
      `Prelude.hashWithSalt` applyServerSideEncryptionByDefault
      `Prelude.hashWithSalt` bucketKeyEnabled

instance Prelude.NFData ServerSideEncryptionRule where
  rnf ServerSideEncryptionRule' {..} =
    Prelude.rnf applyServerSideEncryptionByDefault `Prelude.seq`
      Prelude.rnf bucketKeyEnabled

instance Data.ToXML ServerSideEncryptionRule where
  toXML ServerSideEncryptionRule' {..} =
    Prelude.mconcat
      [ "ApplyServerSideEncryptionByDefault"
          Data.@= applyServerSideEncryptionByDefault,
        "BucketKeyEnabled" Data.@= bucketKeyEnabled
      ]
