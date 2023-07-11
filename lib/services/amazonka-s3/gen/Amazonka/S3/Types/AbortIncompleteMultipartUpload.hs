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
-- Module      : Amazonka.S3.Types.AbortIncompleteMultipartUpload
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.S3.Types.AbortIncompleteMultipartUpload where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.S3.Internal

-- | Specifies the days since the initiation of an incomplete multipart
-- upload that Amazon S3 will wait before permanently removing all parts of
-- the upload. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy>
-- in the /Amazon S3 User Guide/.
--
-- /See:/ 'newAbortIncompleteMultipartUpload' smart constructor.
data AbortIncompleteMultipartUpload = AbortIncompleteMultipartUpload'
  { -- | Specifies the number of days after which Amazon S3 aborts an incomplete
    -- multipart upload.
    daysAfterInitiation :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AbortIncompleteMultipartUpload' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'daysAfterInitiation', 'abortIncompleteMultipartUpload_daysAfterInitiation' - Specifies the number of days after which Amazon S3 aborts an incomplete
-- multipart upload.
newAbortIncompleteMultipartUpload ::
  AbortIncompleteMultipartUpload
newAbortIncompleteMultipartUpload =
  AbortIncompleteMultipartUpload'
    { daysAfterInitiation =
        Prelude.Nothing
    }

-- | Specifies the number of days after which Amazon S3 aborts an incomplete
-- multipart upload.
abortIncompleteMultipartUpload_daysAfterInitiation :: Lens.Lens' AbortIncompleteMultipartUpload (Prelude.Maybe Prelude.Int)
abortIncompleteMultipartUpload_daysAfterInitiation = Lens.lens (\AbortIncompleteMultipartUpload' {daysAfterInitiation} -> daysAfterInitiation) (\s@AbortIncompleteMultipartUpload' {} a -> s {daysAfterInitiation = a} :: AbortIncompleteMultipartUpload)

instance Data.FromXML AbortIncompleteMultipartUpload where
  parseXML x =
    AbortIncompleteMultipartUpload'
      Prelude.<$> (x Data..@? "DaysAfterInitiation")

instance
  Prelude.Hashable
    AbortIncompleteMultipartUpload
  where
  hashWithSalt
    _salt
    AbortIncompleteMultipartUpload' {..} =
      _salt `Prelude.hashWithSalt` daysAfterInitiation

instance
  Prelude.NFData
    AbortIncompleteMultipartUpload
  where
  rnf AbortIncompleteMultipartUpload' {..} =
    Prelude.rnf daysAfterInitiation

instance Data.ToXML AbortIncompleteMultipartUpload where
  toXML AbortIncompleteMultipartUpload' {..} =
    Prelude.mconcat
      ["DaysAfterInitiation" Data.@= daysAfterInitiation]
