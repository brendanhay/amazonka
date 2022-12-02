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
-- Module      : Amazonka.IoTSiteWise.Types.ErrorReportLocation
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.ErrorReportLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The Amazon S3 destination where errors associated with the job creation
-- request are saved.
--
-- /See:/ 'newErrorReportLocation' smart constructor.
data ErrorReportLocation = ErrorReportLocation'
  { -- | The name of the Amazon S3 bucket to which errors associated with the
    -- bulk import job are sent.
    bucket :: Prelude.Text,
    -- | Amazon S3 uses the prefix as a folder name to organize data in the
    -- bucket. Each Amazon S3 object has a key that is its unique identifier in
    -- the bucket. Each object in a bucket has exactly one key. The prefix must
    -- end with a forward slash (\/). For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-prefixes.html Organizing objects using prefixes>
    -- in the /Amazon Simple Storage Service User Guide/.
    prefix :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ErrorReportLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 'errorReportLocation_bucket' - The name of the Amazon S3 bucket to which errors associated with the
-- bulk import job are sent.
--
-- 'prefix', 'errorReportLocation_prefix' - Amazon S3 uses the prefix as a folder name to organize data in the
-- bucket. Each Amazon S3 object has a key that is its unique identifier in
-- the bucket. Each object in a bucket has exactly one key. The prefix must
-- end with a forward slash (\/). For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-prefixes.html Organizing objects using prefixes>
-- in the /Amazon Simple Storage Service User Guide/.
newErrorReportLocation ::
  -- | 'bucket'
  Prelude.Text ->
  -- | 'prefix'
  Prelude.Text ->
  ErrorReportLocation
newErrorReportLocation pBucket_ pPrefix_ =
  ErrorReportLocation'
    { bucket = pBucket_,
      prefix = pPrefix_
    }

-- | The name of the Amazon S3 bucket to which errors associated with the
-- bulk import job are sent.
errorReportLocation_bucket :: Lens.Lens' ErrorReportLocation Prelude.Text
errorReportLocation_bucket = Lens.lens (\ErrorReportLocation' {bucket} -> bucket) (\s@ErrorReportLocation' {} a -> s {bucket = a} :: ErrorReportLocation)

-- | Amazon S3 uses the prefix as a folder name to organize data in the
-- bucket. Each Amazon S3 object has a key that is its unique identifier in
-- the bucket. Each object in a bucket has exactly one key. The prefix must
-- end with a forward slash (\/). For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-prefixes.html Organizing objects using prefixes>
-- in the /Amazon Simple Storage Service User Guide/.
errorReportLocation_prefix :: Lens.Lens' ErrorReportLocation Prelude.Text
errorReportLocation_prefix = Lens.lens (\ErrorReportLocation' {prefix} -> prefix) (\s@ErrorReportLocation' {} a -> s {prefix = a} :: ErrorReportLocation)

instance Data.FromJSON ErrorReportLocation where
  parseJSON =
    Data.withObject
      "ErrorReportLocation"
      ( \x ->
          ErrorReportLocation'
            Prelude.<$> (x Data..: "bucket")
            Prelude.<*> (x Data..: "prefix")
      )

instance Prelude.Hashable ErrorReportLocation where
  hashWithSalt _salt ErrorReportLocation' {..} =
    _salt `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` prefix

instance Prelude.NFData ErrorReportLocation where
  rnf ErrorReportLocation' {..} =
    Prelude.rnf bucket `Prelude.seq` Prelude.rnf prefix

instance Data.ToJSON ErrorReportLocation where
  toJSON ErrorReportLocation' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("bucket" Data..= bucket),
            Prelude.Just ("prefix" Data..= prefix)
          ]
      )
