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
-- Module      : Amazonka.SESV2.Types.FailureInfo
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.FailureInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | An object that contains the failure details about an import job.
--
-- /See:/ 'newFailureInfo' smart constructor.
data FailureInfo = FailureInfo'
  { -- | A message about why the import job failed.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | An Amazon S3 presigned URL that contains all the failed records and
    -- related information.
    failedRecordsS3Url :: Prelude.Maybe Prelude.Text
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
-- 'errorMessage', 'failureInfo_errorMessage' - A message about why the import job failed.
--
-- 'failedRecordsS3Url', 'failureInfo_failedRecordsS3Url' - An Amazon S3 presigned URL that contains all the failed records and
-- related information.
newFailureInfo ::
  FailureInfo
newFailureInfo =
  FailureInfo'
    { errorMessage = Prelude.Nothing,
      failedRecordsS3Url = Prelude.Nothing
    }

-- | A message about why the import job failed.
failureInfo_errorMessage :: Lens.Lens' FailureInfo (Prelude.Maybe Prelude.Text)
failureInfo_errorMessage = Lens.lens (\FailureInfo' {errorMessage} -> errorMessage) (\s@FailureInfo' {} a -> s {errorMessage = a} :: FailureInfo)

-- | An Amazon S3 presigned URL that contains all the failed records and
-- related information.
failureInfo_failedRecordsS3Url :: Lens.Lens' FailureInfo (Prelude.Maybe Prelude.Text)
failureInfo_failedRecordsS3Url = Lens.lens (\FailureInfo' {failedRecordsS3Url} -> failedRecordsS3Url) (\s@FailureInfo' {} a -> s {failedRecordsS3Url = a} :: FailureInfo)

instance Core.FromJSON FailureInfo where
  parseJSON =
    Core.withObject
      "FailureInfo"
      ( \x ->
          FailureInfo'
            Prelude.<$> (x Core..:? "ErrorMessage")
            Prelude.<*> (x Core..:? "FailedRecordsS3Url")
      )

instance Prelude.Hashable FailureInfo where
  hashWithSalt _salt FailureInfo' {..} =
    _salt `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` failedRecordsS3Url

instance Prelude.NFData FailureInfo where
  rnf FailureInfo' {..} =
    Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf failedRecordsS3Url
