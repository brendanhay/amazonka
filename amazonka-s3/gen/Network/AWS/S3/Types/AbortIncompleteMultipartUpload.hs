{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.S3.Types.AbortIncompleteMultipartUpload
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.AbortIncompleteMultipartUpload where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal

-- | Specifies the days since the initiation of an incomplete multipart
-- upload that Amazon S3 will wait before permanently removing all parts of
-- the upload. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/mpuoverview.html#mpu-abort-incomplete-mpu-lifecycle-config Aborting Incomplete Multipart Uploads Using a Bucket Lifecycle Policy>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- /See:/ 'newAbortIncompleteMultipartUpload' smart constructor.
data AbortIncompleteMultipartUpload = AbortIncompleteMultipartUpload'
  { -- | Specifies the number of days after which Amazon S3 aborts an incomplete
    -- multipart upload.
    daysAfterInitiation :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance
  Prelude.FromXML
    AbortIncompleteMultipartUpload
  where
  parseXML x =
    AbortIncompleteMultipartUpload'
      Prelude.<$> (x Prelude..@? "DaysAfterInitiation")

instance
  Prelude.Hashable
    AbortIncompleteMultipartUpload

instance
  Prelude.NFData
    AbortIncompleteMultipartUpload

instance Prelude.ToXML AbortIncompleteMultipartUpload where
  toXML AbortIncompleteMultipartUpload' {..} =
    Prelude.mconcat
      [ "DaysAfterInitiation"
          Prelude.@= daysAfterInitiation
      ]
