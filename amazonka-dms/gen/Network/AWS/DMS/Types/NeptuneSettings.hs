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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Provides information that defines an Amazon Neptune endpoint.
--
-- /See:/ 'newNeptuneSettings' smart constructor.
data NeptuneSettings = NeptuneSettings'
  { -- | The number of milliseconds for AWS DMS to wait to retry a bulk-load of
    -- migrated graph data to the Neptune target database before raising an
    -- error. The default is 250.
    errorRetryDuration :: Core.Maybe Core.Int,
    -- | The Amazon Resource Name (ARN) of the service role that you created for
    -- the Neptune target endpoint. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.ServiceRole Creating an IAM Service Role for Accessing Amazon Neptune as a Target>
    -- in the /AWS Database Migration Service User Guide./
    serviceAccessRoleArn :: Core.Maybe Core.Text,
    -- | The maximum size in kilobytes of migrated graph data stored in a .csv
    -- file before AWS DMS bulk-loads the data to the Neptune target database.
    -- The default is 1,048,576 KB. If the bulk load is successful, AWS DMS
    -- clears the bucket, ready to store the next batch of migrated graph data.
    maxFileSize :: Core.Maybe Core.Int,
    -- | The number of times for AWS DMS to retry a bulk load of migrated graph
    -- data to the Neptune target database before raising an error. The default
    -- is 5.
    maxRetryCount :: Core.Maybe Core.Int,
    -- | If you want AWS Identity and Access Management (IAM) authorization
    -- enabled for this endpoint, set this parameter to @true@. Then attach the
    -- appropriate IAM policy document to your service role specified by
    -- @ServiceAccessRoleArn@. The default is @false@.
    iamAuthEnabled :: Core.Maybe Core.Bool,
    -- | The name of the Amazon S3 bucket where AWS DMS can temporarily store
    -- migrated graph data in .csv files before bulk-loading it to the Neptune
    -- target database. AWS DMS maps the SQL source data to graph data before
    -- storing it in these .csv files.
    s3BucketName :: Core.Text,
    -- | A folder path where you want AWS DMS to store migrated graph data in the
    -- S3 bucket specified by @S3BucketName@
    s3BucketFolder :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  -- | 's3BucketFolder'
  Core.Text ->
  NeptuneSettings
newNeptuneSettings pS3BucketName_ pS3BucketFolder_ =
  NeptuneSettings'
    { errorRetryDuration = Core.Nothing,
      serviceAccessRoleArn = Core.Nothing,
      maxFileSize = Core.Nothing,
      maxRetryCount = Core.Nothing,
      iamAuthEnabled = Core.Nothing,
      s3BucketName = pS3BucketName_,
      s3BucketFolder = pS3BucketFolder_
    }

-- | The number of milliseconds for AWS DMS to wait to retry a bulk-load of
-- migrated graph data to the Neptune target database before raising an
-- error. The default is 250.
neptuneSettings_errorRetryDuration :: Lens.Lens' NeptuneSettings (Core.Maybe Core.Int)
neptuneSettings_errorRetryDuration = Lens.lens (\NeptuneSettings' {errorRetryDuration} -> errorRetryDuration) (\s@NeptuneSettings' {} a -> s {errorRetryDuration = a} :: NeptuneSettings)

-- | The Amazon Resource Name (ARN) of the service role that you created for
-- the Neptune target endpoint. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Target.Neptune.html#CHAP_Target.Neptune.ServiceRole Creating an IAM Service Role for Accessing Amazon Neptune as a Target>
-- in the /AWS Database Migration Service User Guide./
neptuneSettings_serviceAccessRoleArn :: Lens.Lens' NeptuneSettings (Core.Maybe Core.Text)
neptuneSettings_serviceAccessRoleArn = Lens.lens (\NeptuneSettings' {serviceAccessRoleArn} -> serviceAccessRoleArn) (\s@NeptuneSettings' {} a -> s {serviceAccessRoleArn = a} :: NeptuneSettings)

-- | The maximum size in kilobytes of migrated graph data stored in a .csv
-- file before AWS DMS bulk-loads the data to the Neptune target database.
-- The default is 1,048,576 KB. If the bulk load is successful, AWS DMS
-- clears the bucket, ready to store the next batch of migrated graph data.
neptuneSettings_maxFileSize :: Lens.Lens' NeptuneSettings (Core.Maybe Core.Int)
neptuneSettings_maxFileSize = Lens.lens (\NeptuneSettings' {maxFileSize} -> maxFileSize) (\s@NeptuneSettings' {} a -> s {maxFileSize = a} :: NeptuneSettings)

-- | The number of times for AWS DMS to retry a bulk load of migrated graph
-- data to the Neptune target database before raising an error. The default
-- is 5.
neptuneSettings_maxRetryCount :: Lens.Lens' NeptuneSettings (Core.Maybe Core.Int)
neptuneSettings_maxRetryCount = Lens.lens (\NeptuneSettings' {maxRetryCount} -> maxRetryCount) (\s@NeptuneSettings' {} a -> s {maxRetryCount = a} :: NeptuneSettings)

-- | If you want AWS Identity and Access Management (IAM) authorization
-- enabled for this endpoint, set this parameter to @true@. Then attach the
-- appropriate IAM policy document to your service role specified by
-- @ServiceAccessRoleArn@. The default is @false@.
neptuneSettings_iamAuthEnabled :: Lens.Lens' NeptuneSettings (Core.Maybe Core.Bool)
neptuneSettings_iamAuthEnabled = Lens.lens (\NeptuneSettings' {iamAuthEnabled} -> iamAuthEnabled) (\s@NeptuneSettings' {} a -> s {iamAuthEnabled = a} :: NeptuneSettings)

-- | The name of the Amazon S3 bucket where AWS DMS can temporarily store
-- migrated graph data in .csv files before bulk-loading it to the Neptune
-- target database. AWS DMS maps the SQL source data to graph data before
-- storing it in these .csv files.
neptuneSettings_s3BucketName :: Lens.Lens' NeptuneSettings Core.Text
neptuneSettings_s3BucketName = Lens.lens (\NeptuneSettings' {s3BucketName} -> s3BucketName) (\s@NeptuneSettings' {} a -> s {s3BucketName = a} :: NeptuneSettings)

-- | A folder path where you want AWS DMS to store migrated graph data in the
-- S3 bucket specified by @S3BucketName@
neptuneSettings_s3BucketFolder :: Lens.Lens' NeptuneSettings Core.Text
neptuneSettings_s3BucketFolder = Lens.lens (\NeptuneSettings' {s3BucketFolder} -> s3BucketFolder) (\s@NeptuneSettings' {} a -> s {s3BucketFolder = a} :: NeptuneSettings)

instance Core.FromJSON NeptuneSettings where
  parseJSON =
    Core.withObject
      "NeptuneSettings"
      ( \x ->
          NeptuneSettings'
            Core.<$> (x Core..:? "ErrorRetryDuration")
            Core.<*> (x Core..:? "ServiceAccessRoleArn")
            Core.<*> (x Core..:? "MaxFileSize")
            Core.<*> (x Core..:? "MaxRetryCount")
            Core.<*> (x Core..:? "IamAuthEnabled")
            Core.<*> (x Core..: "S3BucketName")
            Core.<*> (x Core..: "S3BucketFolder")
      )

instance Core.Hashable NeptuneSettings

instance Core.NFData NeptuneSettings

instance Core.ToJSON NeptuneSettings where
  toJSON NeptuneSettings' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ErrorRetryDuration" Core..=)
              Core.<$> errorRetryDuration,
            ("ServiceAccessRoleArn" Core..=)
              Core.<$> serviceAccessRoleArn,
            ("MaxFileSize" Core..=) Core.<$> maxFileSize,
            ("MaxRetryCount" Core..=) Core.<$> maxRetryCount,
            ("IamAuthEnabled" Core..=) Core.<$> iamAuthEnabled,
            Core.Just ("S3BucketName" Core..= s3BucketName),
            Core.Just ("S3BucketFolder" Core..= s3BucketFolder)
          ]
      )
