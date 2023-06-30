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
-- Module      : Amazonka.AlexaBusiness.Types.BusinessReportS3Location
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AlexaBusiness.Types.BusinessReportS3Location where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The S3 location of the output reports.
--
-- /See:/ 'newBusinessReportS3Location' smart constructor.
data BusinessReportS3Location = BusinessReportS3Location'
  { -- | The S3 bucket name of the output reports.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The path of the business report.
    path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BusinessReportS3Location' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'businessReportS3Location_bucketName' - The S3 bucket name of the output reports.
--
-- 'path', 'businessReportS3Location_path' - The path of the business report.
newBusinessReportS3Location ::
  BusinessReportS3Location
newBusinessReportS3Location =
  BusinessReportS3Location'
    { bucketName =
        Prelude.Nothing,
      path = Prelude.Nothing
    }

-- | The S3 bucket name of the output reports.
businessReportS3Location_bucketName :: Lens.Lens' BusinessReportS3Location (Prelude.Maybe Prelude.Text)
businessReportS3Location_bucketName = Lens.lens (\BusinessReportS3Location' {bucketName} -> bucketName) (\s@BusinessReportS3Location' {} a -> s {bucketName = a} :: BusinessReportS3Location)

-- | The path of the business report.
businessReportS3Location_path :: Lens.Lens' BusinessReportS3Location (Prelude.Maybe Prelude.Text)
businessReportS3Location_path = Lens.lens (\BusinessReportS3Location' {path} -> path) (\s@BusinessReportS3Location' {} a -> s {path = a} :: BusinessReportS3Location)

instance Data.FromJSON BusinessReportS3Location where
  parseJSON =
    Data.withObject
      "BusinessReportS3Location"
      ( \x ->
          BusinessReportS3Location'
            Prelude.<$> (x Data..:? "BucketName")
            Prelude.<*> (x Data..:? "Path")
      )

instance Prelude.Hashable BusinessReportS3Location where
  hashWithSalt _salt BusinessReportS3Location' {..} =
    _salt
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` path

instance Prelude.NFData BusinessReportS3Location where
  rnf BusinessReportS3Location' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf path
