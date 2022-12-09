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
-- Module      : Amazonka.SecurityHub.Types.AwsS3BucketDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsS3BucketDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsS3AccountPublicAccessBlockDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketLifecycleConfigurationDetails
import Amazonka.SecurityHub.Types.AwsS3BucketBucketVersioningConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketLoggingConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketNotificationConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketServerSideEncryptionConfiguration
import Amazonka.SecurityHub.Types.AwsS3BucketWebsiteConfiguration

-- | The details of an Amazon S3 bucket.
--
-- /See:/ 'newAwsS3BucketDetails' smart constructor.
data AwsS3BucketDetails = AwsS3BucketDetails'
  { -- | The access control list for the S3 bucket.
    accessControlList :: Prelude.Maybe Prelude.Text,
    -- | The lifecycle configuration for objects in the S3 bucket.
    bucketLifecycleConfiguration :: Prelude.Maybe AwsS3BucketBucketLifecycleConfigurationDetails,
    -- | The logging configuration for the S3 bucket.
    bucketLoggingConfiguration :: Prelude.Maybe AwsS3BucketLoggingConfiguration,
    -- | The notification configuration for the S3 bucket.
    bucketNotificationConfiguration :: Prelude.Maybe AwsS3BucketNotificationConfiguration,
    -- | The versioning state of an S3 bucket.
    bucketVersioningConfiguration :: Prelude.Maybe AwsS3BucketBucketVersioningConfiguration,
    -- | The website configuration parameters for the S3 bucket.
    bucketWebsiteConfiguration :: Prelude.Maybe AwsS3BucketWebsiteConfiguration,
    -- | Indicates when the S3 bucket was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createdAt :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account identifier of the account that owns the
    -- S3 bucket.
    ownerAccountId :: Prelude.Maybe Prelude.Text,
    -- | The canonical user ID of the owner of the S3 bucket.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The display name of the owner of the S3 bucket.
    ownerName :: Prelude.Maybe Prelude.Text,
    -- | Provides information about the Amazon S3 Public Access Block
    -- configuration for the S3 bucket.
    publicAccessBlockConfiguration :: Prelude.Maybe AwsS3AccountPublicAccessBlockDetails,
    -- | The encryption rules that are applied to the S3 bucket.
    serverSideEncryptionConfiguration :: Prelude.Maybe AwsS3BucketServerSideEncryptionConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsS3BucketDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessControlList', 'awsS3BucketDetails_accessControlList' - The access control list for the S3 bucket.
--
-- 'bucketLifecycleConfiguration', 'awsS3BucketDetails_bucketLifecycleConfiguration' - The lifecycle configuration for objects in the S3 bucket.
--
-- 'bucketLoggingConfiguration', 'awsS3BucketDetails_bucketLoggingConfiguration' - The logging configuration for the S3 bucket.
--
-- 'bucketNotificationConfiguration', 'awsS3BucketDetails_bucketNotificationConfiguration' - The notification configuration for the S3 bucket.
--
-- 'bucketVersioningConfiguration', 'awsS3BucketDetails_bucketVersioningConfiguration' - The versioning state of an S3 bucket.
--
-- 'bucketWebsiteConfiguration', 'awsS3BucketDetails_bucketWebsiteConfiguration' - The website configuration parameters for the S3 bucket.
--
-- 'createdAt', 'awsS3BucketDetails_createdAt' - Indicates when the S3 bucket was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'ownerAccountId', 'awsS3BucketDetails_ownerAccountId' - The Amazon Web Services account identifier of the account that owns the
-- S3 bucket.
--
-- 'ownerId', 'awsS3BucketDetails_ownerId' - The canonical user ID of the owner of the S3 bucket.
--
-- 'ownerName', 'awsS3BucketDetails_ownerName' - The display name of the owner of the S3 bucket.
--
-- 'publicAccessBlockConfiguration', 'awsS3BucketDetails_publicAccessBlockConfiguration' - Provides information about the Amazon S3 Public Access Block
-- configuration for the S3 bucket.
--
-- 'serverSideEncryptionConfiguration', 'awsS3BucketDetails_serverSideEncryptionConfiguration' - The encryption rules that are applied to the S3 bucket.
newAwsS3BucketDetails ::
  AwsS3BucketDetails
newAwsS3BucketDetails =
  AwsS3BucketDetails'
    { accessControlList =
        Prelude.Nothing,
      bucketLifecycleConfiguration = Prelude.Nothing,
      bucketLoggingConfiguration = Prelude.Nothing,
      bucketNotificationConfiguration = Prelude.Nothing,
      bucketVersioningConfiguration = Prelude.Nothing,
      bucketWebsiteConfiguration = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      ownerAccountId = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      ownerName = Prelude.Nothing,
      publicAccessBlockConfiguration = Prelude.Nothing,
      serverSideEncryptionConfiguration = Prelude.Nothing
    }

-- | The access control list for the S3 bucket.
awsS3BucketDetails_accessControlList :: Lens.Lens' AwsS3BucketDetails (Prelude.Maybe Prelude.Text)
awsS3BucketDetails_accessControlList = Lens.lens (\AwsS3BucketDetails' {accessControlList} -> accessControlList) (\s@AwsS3BucketDetails' {} a -> s {accessControlList = a} :: AwsS3BucketDetails)

-- | The lifecycle configuration for objects in the S3 bucket.
awsS3BucketDetails_bucketLifecycleConfiguration :: Lens.Lens' AwsS3BucketDetails (Prelude.Maybe AwsS3BucketBucketLifecycleConfigurationDetails)
awsS3BucketDetails_bucketLifecycleConfiguration = Lens.lens (\AwsS3BucketDetails' {bucketLifecycleConfiguration} -> bucketLifecycleConfiguration) (\s@AwsS3BucketDetails' {} a -> s {bucketLifecycleConfiguration = a} :: AwsS3BucketDetails)

-- | The logging configuration for the S3 bucket.
awsS3BucketDetails_bucketLoggingConfiguration :: Lens.Lens' AwsS3BucketDetails (Prelude.Maybe AwsS3BucketLoggingConfiguration)
awsS3BucketDetails_bucketLoggingConfiguration = Lens.lens (\AwsS3BucketDetails' {bucketLoggingConfiguration} -> bucketLoggingConfiguration) (\s@AwsS3BucketDetails' {} a -> s {bucketLoggingConfiguration = a} :: AwsS3BucketDetails)

-- | The notification configuration for the S3 bucket.
awsS3BucketDetails_bucketNotificationConfiguration :: Lens.Lens' AwsS3BucketDetails (Prelude.Maybe AwsS3BucketNotificationConfiguration)
awsS3BucketDetails_bucketNotificationConfiguration = Lens.lens (\AwsS3BucketDetails' {bucketNotificationConfiguration} -> bucketNotificationConfiguration) (\s@AwsS3BucketDetails' {} a -> s {bucketNotificationConfiguration = a} :: AwsS3BucketDetails)

-- | The versioning state of an S3 bucket.
awsS3BucketDetails_bucketVersioningConfiguration :: Lens.Lens' AwsS3BucketDetails (Prelude.Maybe AwsS3BucketBucketVersioningConfiguration)
awsS3BucketDetails_bucketVersioningConfiguration = Lens.lens (\AwsS3BucketDetails' {bucketVersioningConfiguration} -> bucketVersioningConfiguration) (\s@AwsS3BucketDetails' {} a -> s {bucketVersioningConfiguration = a} :: AwsS3BucketDetails)

-- | The website configuration parameters for the S3 bucket.
awsS3BucketDetails_bucketWebsiteConfiguration :: Lens.Lens' AwsS3BucketDetails (Prelude.Maybe AwsS3BucketWebsiteConfiguration)
awsS3BucketDetails_bucketWebsiteConfiguration = Lens.lens (\AwsS3BucketDetails' {bucketWebsiteConfiguration} -> bucketWebsiteConfiguration) (\s@AwsS3BucketDetails' {} a -> s {bucketWebsiteConfiguration = a} :: AwsS3BucketDetails)

-- | Indicates when the S3 bucket was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsS3BucketDetails_createdAt :: Lens.Lens' AwsS3BucketDetails (Prelude.Maybe Prelude.Text)
awsS3BucketDetails_createdAt = Lens.lens (\AwsS3BucketDetails' {createdAt} -> createdAt) (\s@AwsS3BucketDetails' {} a -> s {createdAt = a} :: AwsS3BucketDetails)

-- | The Amazon Web Services account identifier of the account that owns the
-- S3 bucket.
awsS3BucketDetails_ownerAccountId :: Lens.Lens' AwsS3BucketDetails (Prelude.Maybe Prelude.Text)
awsS3BucketDetails_ownerAccountId = Lens.lens (\AwsS3BucketDetails' {ownerAccountId} -> ownerAccountId) (\s@AwsS3BucketDetails' {} a -> s {ownerAccountId = a} :: AwsS3BucketDetails)

-- | The canonical user ID of the owner of the S3 bucket.
awsS3BucketDetails_ownerId :: Lens.Lens' AwsS3BucketDetails (Prelude.Maybe Prelude.Text)
awsS3BucketDetails_ownerId = Lens.lens (\AwsS3BucketDetails' {ownerId} -> ownerId) (\s@AwsS3BucketDetails' {} a -> s {ownerId = a} :: AwsS3BucketDetails)

-- | The display name of the owner of the S3 bucket.
awsS3BucketDetails_ownerName :: Lens.Lens' AwsS3BucketDetails (Prelude.Maybe Prelude.Text)
awsS3BucketDetails_ownerName = Lens.lens (\AwsS3BucketDetails' {ownerName} -> ownerName) (\s@AwsS3BucketDetails' {} a -> s {ownerName = a} :: AwsS3BucketDetails)

-- | Provides information about the Amazon S3 Public Access Block
-- configuration for the S3 bucket.
awsS3BucketDetails_publicAccessBlockConfiguration :: Lens.Lens' AwsS3BucketDetails (Prelude.Maybe AwsS3AccountPublicAccessBlockDetails)
awsS3BucketDetails_publicAccessBlockConfiguration = Lens.lens (\AwsS3BucketDetails' {publicAccessBlockConfiguration} -> publicAccessBlockConfiguration) (\s@AwsS3BucketDetails' {} a -> s {publicAccessBlockConfiguration = a} :: AwsS3BucketDetails)

-- | The encryption rules that are applied to the S3 bucket.
awsS3BucketDetails_serverSideEncryptionConfiguration :: Lens.Lens' AwsS3BucketDetails (Prelude.Maybe AwsS3BucketServerSideEncryptionConfiguration)
awsS3BucketDetails_serverSideEncryptionConfiguration = Lens.lens (\AwsS3BucketDetails' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@AwsS3BucketDetails' {} a -> s {serverSideEncryptionConfiguration = a} :: AwsS3BucketDetails)

instance Data.FromJSON AwsS3BucketDetails where
  parseJSON =
    Data.withObject
      "AwsS3BucketDetails"
      ( \x ->
          AwsS3BucketDetails'
            Prelude.<$> (x Data..:? "AccessControlList")
            Prelude.<*> (x Data..:? "BucketLifecycleConfiguration")
            Prelude.<*> (x Data..:? "BucketLoggingConfiguration")
            Prelude.<*> (x Data..:? "BucketNotificationConfiguration")
            Prelude.<*> (x Data..:? "BucketVersioningConfiguration")
            Prelude.<*> (x Data..:? "BucketWebsiteConfiguration")
            Prelude.<*> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "OwnerAccountId")
            Prelude.<*> (x Data..:? "OwnerId")
            Prelude.<*> (x Data..:? "OwnerName")
            Prelude.<*> (x Data..:? "PublicAccessBlockConfiguration")
            Prelude.<*> (x Data..:? "ServerSideEncryptionConfiguration")
      )

instance Prelude.Hashable AwsS3BucketDetails where
  hashWithSalt _salt AwsS3BucketDetails' {..} =
    _salt `Prelude.hashWithSalt` accessControlList
      `Prelude.hashWithSalt` bucketLifecycleConfiguration
      `Prelude.hashWithSalt` bucketLoggingConfiguration
      `Prelude.hashWithSalt` bucketNotificationConfiguration
      `Prelude.hashWithSalt` bucketVersioningConfiguration
      `Prelude.hashWithSalt` bucketWebsiteConfiguration
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` ownerAccountId
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` ownerName
      `Prelude.hashWithSalt` publicAccessBlockConfiguration
      `Prelude.hashWithSalt` serverSideEncryptionConfiguration

instance Prelude.NFData AwsS3BucketDetails where
  rnf AwsS3BucketDetails' {..} =
    Prelude.rnf accessControlList
      `Prelude.seq` Prelude.rnf bucketLifecycleConfiguration
      `Prelude.seq` Prelude.rnf bucketLoggingConfiguration
      `Prelude.seq` Prelude.rnf bucketNotificationConfiguration
      `Prelude.seq` Prelude.rnf bucketVersioningConfiguration
      `Prelude.seq` Prelude.rnf bucketWebsiteConfiguration
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf ownerAccountId
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf ownerName
      `Prelude.seq` Prelude.rnf publicAccessBlockConfiguration
      `Prelude.seq` Prelude.rnf serverSideEncryptionConfiguration

instance Data.ToJSON AwsS3BucketDetails where
  toJSON AwsS3BucketDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccessControlList" Data..=)
              Prelude.<$> accessControlList,
            ("BucketLifecycleConfiguration" Data..=)
              Prelude.<$> bucketLifecycleConfiguration,
            ("BucketLoggingConfiguration" Data..=)
              Prelude.<$> bucketLoggingConfiguration,
            ("BucketNotificationConfiguration" Data..=)
              Prelude.<$> bucketNotificationConfiguration,
            ("BucketVersioningConfiguration" Data..=)
              Prelude.<$> bucketVersioningConfiguration,
            ("BucketWebsiteConfiguration" Data..=)
              Prelude.<$> bucketWebsiteConfiguration,
            ("CreatedAt" Data..=) Prelude.<$> createdAt,
            ("OwnerAccountId" Data..=)
              Prelude.<$> ownerAccountId,
            ("OwnerId" Data..=) Prelude.<$> ownerId,
            ("OwnerName" Data..=) Prelude.<$> ownerName,
            ("PublicAccessBlockConfiguration" Data..=)
              Prelude.<$> publicAccessBlockConfiguration,
            ("ServerSideEncryptionConfiguration" Data..=)
              Prelude.<$> serverSideEncryptionConfiguration
          ]
      )
