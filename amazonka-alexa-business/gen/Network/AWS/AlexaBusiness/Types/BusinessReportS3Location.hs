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
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportS3Location
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportS3Location where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The S3 location of the output reports.
--
-- /See:/ 'newBusinessReportS3Location' smart constructor.
data BusinessReportS3Location = BusinessReportS3Location'
  { -- | The S3 bucket name of the output reports.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The path of the business report.
    path :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON BusinessReportS3Location where
  parseJSON =
    Prelude.withObject
      "BusinessReportS3Location"
      ( \x ->
          BusinessReportS3Location'
            Prelude.<$> (x Prelude..:? "BucketName")
            Prelude.<*> (x Prelude..:? "Path")
      )

instance Prelude.Hashable BusinessReportS3Location

instance Prelude.NFData BusinessReportS3Location
