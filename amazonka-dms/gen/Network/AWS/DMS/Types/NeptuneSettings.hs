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
-- Module      : Network.AWS.DMS.Types.NeptuneSettings
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.NeptuneSettings where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides information that defines an Amazon Neptune endpoint.
--
-- /See:/ 'newNeptuneSettings' smart constructor.
data NeptuneSettings = NeptuneSettings'
  { -- | The number of milliseconds for AWS DMS to wait to retry a bulk-load of
    -- migrated graph data to the Neptune target database before raising an
    -- error. The default is 250.
    errorRetryDuration :: Prelude.Maybe Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the service role that you created for
    -- the Neptune target endpoint. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.ServiceRole Creating an IAM Service Role for Accessing Amazon Neptune as a Target>
    -- in the /AWS Database Migration Service User Guide./
    serviceAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The maximum size in kilobytes of migrated graph data stored in a .csv
    -- file before AWS DMS bulk-loads the data to the Neptune target database.
    -- The default is 1,048,576 KB. If the bulk load is successful, AWS DMS
    -- clears the bucket, ready to store the next batch of migrated graph data.
    maxFileSize :: Prelude.Maybe Prelude.Int,
    -- | The number of times for AWS DMS to retry a bulk load of migrated graph
    -- data to the Neptune target database before raising an error. The default
    -- is 5.
    maxRetryCount :: Prelude.Maybe Prelude.Int,
    -- | If you want AWS Identity and Access Management (IAM) authorization
    -- enabled for this endpoint, set this parameter to @true@. Then attach the
    -- appropriate IAM policy document to your service role specified by
    -- @ServiceAccessRoleArn@. The default is @false@.
    iamAuthEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the Amazon S3 bucket where AWS DMS can temporarily store
    -- migrated graph data in .csv files before bulk-loading it to the Neptune
    -- target database. AWS DMS maps the SQL source data to graph data before
    -- storing it in these .csv files.
    s3BucketName :: Prelude.Text,
    -- | A folder path where you want AWS DMS to store migrated graph data in the
    -- S3 bucket specified by @S3BucketName@
    s3BucketFolder :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'NeptuneSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'errorRetryDuration', 'neptuneSettings_errorRetryDuration' - The number of milliseconds for AWS DMS to wait to retry a bulk-load of
-- migrated graph data to the Neptune target database before raising an
-- error. The default is 250.
--
-- 'serviceAccessRoleArn', 'neptuneSettings_serviceAccessRoleArn' - The Amazon Resource Name (ARN) of the service role that you created for
-- the Neptune target endpoint. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.ServiceRole Creating an IAM Service Role for Accessing Amazon Neptune as a Target>
-- in the /AWS Database Migration Service User Guide./
--
-- 'maxFileSize', 'neptuneSettings_maxFileSize' - The maximum size in kilobytes of migrated graph data stored in a .csv
-- file before AWS DMS bulk-loads the data to the Neptune target database.
-- The default is 1,048,576 KB. If the bulk load is successful, AWS DMS
-- clears the bucket, ready to store the next batch of migrated graph data.
--
-- 'maxRetryCount', 'neptuneSettings_maxRetryCount' - The number of times for AWS DMS to retry a bulk load of migrated graph
-- data to the Neptune target database before raising an error. The default
-- is 5.
--
-- 'iamAuthEnabled', 'neptuneSettings_iamAuthEnabled' - If you want AWS Identity and Access Management (IAM) authorization
-- enabled for this endpoint, set this parameter to @true@. Then attach the
-- appropriate IAM policy document to your service role specified by
-- @ServiceAccessRoleArn@. The default is @false@.
--
-- 's3BucketName', 'neptuneSettings_s3BucketName' - The name of the Amazon S3 bucket where AWS DMS can temporarily store
-- migrated graph data in .csv files before bulk-loading it to the Neptune
-- target database. AWS DMS maps the SQL source data to graph data before
-- storing it in these .csv files.
--
-- 's3BucketFolder', 'neptuneSettings_s3BucketFolder' - A folder path where you want AWS DMS to store migrated graph data in the
-- S3 bucket specified by @S3BucketName@
newNeptuneSettings ::
  -- | 's3BucketName'
  Prelude.Text ->
  -- | 's3BucketFolder'
  Prelude.Text ->
  NeptuneSettings
newNeptuneSettings pS3BucketName_ pS3BucketFolder_ =
  NeptuneSettings'
    { errorRetryDuration =
        Prelude.Nothing,
      serviceAccessRoleArn = Prelude.Nothing,
      maxFileSize = Prelude.Nothing,
      maxRetryCount = Prelude.Nothing,
      iamAuthEnabled = Prelude.Nothing,
      s3BucketName = pS3BucketName_,
      s3BucketFolder = pS3BucketFolder_
    }

-- | The number of milliseconds for AWS DMS to wait to retry a bulk-load of
-- migrated graph data to the Neptune target database before raising an
-- error. The default is 250.
neptuneSettings_errorRetryDuration :: Lens.Lens' NeptuneSettings (Prelude.Maybe Prelude.Int)
neptuneSettings_errorRetryDuration = Lens.lens (\NeptuneSettings' {errorRetryDuration} -> errorRetryDuration) (\s@NeptuneSettings' {} a -> s {errorRetryDuration = a} :: NeptuneSettings)

-- | The Amazon Resource Name (ARN) of the service role that you created for
-- the Neptune target endpoint. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.ServiceRole Creating an IAM Service Role for Accessing Amazon Neptune as a Target>
-- in the /AWS Database Migration Service User Guide./
neptuneSettings_serviceAccessRoleArn :: Lens.Lens' NeptuneSettings (Prelude.Maybe Prelude.Text)
neptuneSettings_serviceAccessRoleArn = Lens.lens (\NeptuneSettings' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@NeptuneSettings' {} a -> s {serviceAccessRoleArn = a} :: NeptuneSettings)

-- | The maximum size in kilobytes of migrated graph data stored in a .csv
-- file before AWS DMS bulk-loads the data to the Neptune target database.
-- The default is 1,048,576 KB. If the bulk load is successful, AWS DMS
-- clears the bucket, ready to store the next batch of migrated graph data.
neptuneSettings_maxFileSize :: Lens.Lens' NeptuneSettings (Prelude.Maybe Prelude.Int)
neptuneSettings_maxFileSize = Lens.lens (\NeptuneSettings' {maxFileSize} -> maxFileSize) (\s@NeptuneSettings' {} a -> s {maxFileSize = a} :: NeptuneSettings)

-- | The number of times for AWS DMS to retry a bulk load of migrated graph
-- data to the Neptune target database before raising an error. The default
-- is 5.
neptuneSettings_maxRetryCount :: Lens.Lens' NeptuneSettings (Prelude.Maybe Prelude.Int)
neptuneSettings_maxRetryCount = Lens.lens (\NeptuneSettings' {maxRetryCount} -> maxRetryCount) (\s@NeptuneSettings' {} a -> s {maxRetryCount = a} :: NeptuneSettings)

-- | If you want AWS Identity and Access Management (IAM) authorization
-- enabled for this endpoint, set this parameter to @true@. Then attach the
-- appropriate IAM policy document to your service role specified by
-- @ServiceAccessRoleArn@. The default is @false@.
neptuneSettings_iamAuthEnabled :: Lens.Lens' NeptuneSettings (Prelude.Maybe Prelude.Bool)
neptuneSettings_iamAuthEnabled = Lens.lens (\NeptuneSettings' {iamAuthEnabled} -> iamAuthEnabled) (\s@NeptuneSettings' {} a -> s {iamAuthEnabled = a} :: NeptuneSettings)

-- | The name of the Amazon S3 bucket where AWS DMS can temporarily store
-- migrated graph data in .csv files before bulk-loading it to the Neptune
-- target database. AWS DMS maps the SQL source data to graph data before
-- storing it in these .csv files.
neptuneSettings_s3BucketName :: Lens.Lens' NeptuneSettings Prelude.Text
neptuneSettings_s3BucketName = Lens.lens (\NeptuneSettings' {s3BucketName} -> s3BucketName) (\s@NeptuneSettings' {} a -> s {s3BucketName = a} :: NeptuneSettings)

-- | A folder path where you want AWS DMS to store migrated graph data in the
-- S3 bucket specified by @S3BucketName@
neptuneSettings_s3BucketFolder :: Lens.Lens' NeptuneSettings Prelude.Text
neptuneSettings_s3BucketFolder = Lens.lens (\NeptuneSettings' {s3BucketFolder} -> s3BucketFolder) (\s@NeptuneSettings' {} a -> s {s3BucketFolder = a} :: NeptuneSettings)

instance Prelude.FromJSON NeptuneSettings where
  parseJSON =
    Prelude.withObject
      "NeptuneSettings"
      ( \x ->
          NeptuneSettings'
            Prelude.<$> (x Prelude..:? "ErrorRetryDuration")
            Prelude.<*> (x Prelude..:? "ServiceAccessRoleArn")
            Prelude.<*> (x Prelude..:? "MaxFileSize")
            Prelude.<*> (x Prelude..:? "MaxRetryCount")
            Prelude.<*> (x Prelude..:? "IamAuthEnabled")
            Prelude.<*> (x Prelude..: "S3BucketName")
            Prelude.<*> (x Prelude..: "S3BucketFolder")
      )

instance Prelude.Hashable NeptuneSettings

instance Prelude.NFData NeptuneSettings

instance Prelude.ToJSON NeptuneSettings where
  toJSON NeptuneSettings' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("ErrorRetryDuration" Prelude..=)
              Prelude.<$> errorRetryDuration,
            ("ServiceAccessRoleArn" Prelude..=)
              Prelude.<$> serviceAccessRoleArn,
            ("MaxFileSize" Prelude..=) Prelude.<$> maxFileSize,
            ("MaxRetryCount" Prelude..=)
              Prelude.<$> maxRetryCount,
            ("IamAuthEnabled" Prelude..=)
              Prelude.<$> iamAuthEnabled,
            Prelude.Just
              ("S3BucketName" Prelude..= s3BucketName),
            Prelude.Just
              ("S3BucketFolder" Prelude..= s3BucketFolder)
          ]
      )
