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
-- Module      : Network.AWS.SESv2.Types.FailureInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESv2.Types.FailureInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An object that contains the failure details about an import job.
--
-- /See:/ 'newFailureInfo' smart constructor.
data FailureInfo = FailureInfo'
  { -- | An Amazon S3 presigned URL that contains all the failed records and
    -- related information.
    failedRecordsS3Url :: Prelude.Maybe Prelude.Text,
    -- | A message about why the import job failed.
    errorMessage :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FailureInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedRecordsS3Url', 'failureInfo_failedRecordsS3Url' - An Amazon S3 presigned URL that contains all the failed records and
-- related information.
--
-- 'errorMessage', 'failureInfo_errorMessage' - A message about why the import job failed.
newFailureInfo ::
  FailureInfo
newFailureInfo =
  FailureInfo'
    { failedRecordsS3Url = Prelude.Nothing,
      errorMessage = Prelude.Nothing
    }

-- | An Amazon S3 presigned URL that contains all the failed records and
-- related information.
failureInfo_failedRecordsS3Url :: Lens.Lens' FailureInfo (Prelude.Maybe Prelude.Text)
failureInfo_failedRecordsS3Url = Lens.lens (\FailureInfo' {failedRecordsS3Url} -> failedRecordsS3Url) (\s@FailureInfo' {} a -> s {failedRecordsS3Url = a} :: FailureInfo)

-- | A message about why the import job failed.
failureInfo_errorMessage :: Lens.Lens' FailureInfo (Prelude.Maybe Prelude.Text)
failureInfo_errorMessage = Lens.lens (\FailureInfo' {errorMessage} -> errorMessage) (\s@FailureInfo' {} a -> s {errorMessage = a} :: FailureInfo)

instance Core.FromJSON FailureInfo where
  parseJSON =
    Core.withObject
      "FailureInfo"
      ( \x ->
          FailureInfo'
            Prelude.<$> (x Core..:? "FailedRecordsS3Url")
            Prelude.<*> (x Core..:? "ErrorMessage")
      )

instance Prelude.Hashable FailureInfo

instance Prelude.NFData FailureInfo
