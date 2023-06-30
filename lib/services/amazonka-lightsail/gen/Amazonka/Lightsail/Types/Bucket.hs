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
-- Module      : Amazonka.Lightsail.Types.Bucket
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.Bucket where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types.AccessRules
import Amazonka.Lightsail.Types.BucketAccessLogConfig
import Amazonka.Lightsail.Types.BucketState
import Amazonka.Lightsail.Types.ResourceLocation
import Amazonka.Lightsail.Types.ResourceReceivingAccess
import Amazonka.Lightsail.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes an Amazon Lightsail bucket.
--
-- /See:/ 'newBucket' smart constructor.
data Bucket = Bucket'
  { -- | Indicates whether the bundle that is currently applied to a bucket can
    -- be changed to another bundle.
    --
    -- You can update a bucket\'s bundle only one time within a monthly Amazon
    -- Web Services billing cycle.
    --
    -- Use the
    -- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_UpdateBucketBundle.html UpdateBucketBundle>
    -- action to change a bucket\'s bundle.
    ableToUpdateBundle :: Prelude.Maybe Prelude.Bool,
    -- | An object that describes the access log configuration for the bucket.
    accessLogConfig :: Prelude.Maybe BucketAccessLogConfig,
    -- | An object that describes the access rules of the bucket.
    accessRules :: Prelude.Maybe AccessRules,
    -- | The Amazon Resource Name (ARN) of the bucket.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the bundle currently applied to the bucket.
    --
    -- A bucket bundle specifies the monthly cost, storage space, and data
    -- transfer quota for a bucket.
    --
    -- Use the
    -- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_UpdateBucketBundle.html UpdateBucketBundle>
    -- action to change the bundle of a bucket.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the distribution was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | An object that describes the location of the bucket, such as the Amazon
    -- Web Services Region and Availability Zone.
    location :: Prelude.Maybe ResourceLocation,
    -- | The name of the bucket.
    name :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether object versioning is enabled for the bucket.
    --
    -- The following options can be configured:
    --
    -- -   @Enabled@ - Object versioning is enabled.
    --
    -- -   @Suspended@ - Object versioning was previously enabled but is
    --     currently suspended. Existing object versions are retained.
    --
    -- -   @NeverEnabled@ - Object versioning has never been enabled.
    objectVersioning :: Prelude.Maybe Prelude.Text,
    -- | An array of strings that specify the Amazon Web Services account IDs
    -- that have read-only access to the bucket.
    readonlyAccessAccounts :: Prelude.Maybe [Prelude.Text],
    -- | The Lightsail resource type of the bucket (for example, @Bucket@).
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe Lightsail instances that have access
    -- to the bucket.
    --
    -- Use the
    -- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_SetResourceAccessForBucket.html SetResourceAccessForBucket>
    -- action to update the instances that have access to a bucket.
    resourcesReceivingAccess :: Prelude.Maybe [ResourceReceivingAccess],
    -- | An object that describes the state of the bucket.
    state :: Prelude.Maybe BucketState,
    -- | The support code for a bucket. Include this code in your email to
    -- support when you have questions about a Lightsail bucket. This code
    -- enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The tag keys and optional values for the bucket. For more information,
    -- see
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Tags in Amazon Lightsail>
    -- in the /Amazon Lightsail Developer Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The URL of the bucket.
    url :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Bucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ableToUpdateBundle', 'bucket_ableToUpdateBundle' - Indicates whether the bundle that is currently applied to a bucket can
-- be changed to another bundle.
--
-- You can update a bucket\'s bundle only one time within a monthly Amazon
-- Web Services billing cycle.
--
-- Use the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_UpdateBucketBundle.html UpdateBucketBundle>
-- action to change a bucket\'s bundle.
--
-- 'accessLogConfig', 'bucket_accessLogConfig' - An object that describes the access log configuration for the bucket.
--
-- 'accessRules', 'bucket_accessRules' - An object that describes the access rules of the bucket.
--
-- 'arn', 'bucket_arn' - The Amazon Resource Name (ARN) of the bucket.
--
-- 'bundleId', 'bucket_bundleId' - The ID of the bundle currently applied to the bucket.
--
-- A bucket bundle specifies the monthly cost, storage space, and data
-- transfer quota for a bucket.
--
-- Use the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_UpdateBucketBundle.html UpdateBucketBundle>
-- action to change the bundle of a bucket.
--
-- 'createdAt', 'bucket_createdAt' - The timestamp when the distribution was created.
--
-- 'location', 'bucket_location' - An object that describes the location of the bucket, such as the Amazon
-- Web Services Region and Availability Zone.
--
-- 'name', 'bucket_name' - The name of the bucket.
--
-- 'objectVersioning', 'bucket_objectVersioning' - Indicates whether object versioning is enabled for the bucket.
--
-- The following options can be configured:
--
-- -   @Enabled@ - Object versioning is enabled.
--
-- -   @Suspended@ - Object versioning was previously enabled but is
--     currently suspended. Existing object versions are retained.
--
-- -   @NeverEnabled@ - Object versioning has never been enabled.
--
-- 'readonlyAccessAccounts', 'bucket_readonlyAccessAccounts' - An array of strings that specify the Amazon Web Services account IDs
-- that have read-only access to the bucket.
--
-- 'resourceType', 'bucket_resourceType' - The Lightsail resource type of the bucket (for example, @Bucket@).
--
-- 'resourcesReceivingAccess', 'bucket_resourcesReceivingAccess' - An array of objects that describe Lightsail instances that have access
-- to the bucket.
--
-- Use the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_SetResourceAccessForBucket.html SetResourceAccessForBucket>
-- action to update the instances that have access to a bucket.
--
-- 'state', 'bucket_state' - An object that describes the state of the bucket.
--
-- 'supportCode', 'bucket_supportCode' - The support code for a bucket. Include this code in your email to
-- support when you have questions about a Lightsail bucket. This code
-- enables our support team to look up your Lightsail information more
-- easily.
--
-- 'tags', 'bucket_tags' - The tag keys and optional values for the bucket. For more information,
-- see
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Tags in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
--
-- 'url', 'bucket_url' - The URL of the bucket.
newBucket ::
  Bucket
newBucket =
  Bucket'
    { ableToUpdateBundle = Prelude.Nothing,
      accessLogConfig = Prelude.Nothing,
      accessRules = Prelude.Nothing,
      arn = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      location = Prelude.Nothing,
      name = Prelude.Nothing,
      objectVersioning = Prelude.Nothing,
      readonlyAccessAccounts = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      resourcesReceivingAccess = Prelude.Nothing,
      state = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      tags = Prelude.Nothing,
      url = Prelude.Nothing
    }

-- | Indicates whether the bundle that is currently applied to a bucket can
-- be changed to another bundle.
--
-- You can update a bucket\'s bundle only one time within a monthly Amazon
-- Web Services billing cycle.
--
-- Use the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_UpdateBucketBundle.html UpdateBucketBundle>
-- action to change a bucket\'s bundle.
bucket_ableToUpdateBundle :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Bool)
bucket_ableToUpdateBundle = Lens.lens (\Bucket' {ableToUpdateBundle} -> ableToUpdateBundle) (\s@Bucket' {} a -> s {ableToUpdateBundle = a} :: Bucket)

-- | An object that describes the access log configuration for the bucket.
bucket_accessLogConfig :: Lens.Lens' Bucket (Prelude.Maybe BucketAccessLogConfig)
bucket_accessLogConfig = Lens.lens (\Bucket' {accessLogConfig} -> accessLogConfig) (\s@Bucket' {} a -> s {accessLogConfig = a} :: Bucket)

-- | An object that describes the access rules of the bucket.
bucket_accessRules :: Lens.Lens' Bucket (Prelude.Maybe AccessRules)
bucket_accessRules = Lens.lens (\Bucket' {accessRules} -> accessRules) (\s@Bucket' {} a -> s {accessRules = a} :: Bucket)

-- | The Amazon Resource Name (ARN) of the bucket.
bucket_arn :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_arn = Lens.lens (\Bucket' {arn} -> arn) (\s@Bucket' {} a -> s {arn = a} :: Bucket)

-- | The ID of the bundle currently applied to the bucket.
--
-- A bucket bundle specifies the monthly cost, storage space, and data
-- transfer quota for a bucket.
--
-- Use the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_UpdateBucketBundle.html UpdateBucketBundle>
-- action to change the bundle of a bucket.
bucket_bundleId :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_bundleId = Lens.lens (\Bucket' {bundleId} -> bundleId) (\s@Bucket' {} a -> s {bundleId = a} :: Bucket)

-- | The timestamp when the distribution was created.
bucket_createdAt :: Lens.Lens' Bucket (Prelude.Maybe Prelude.UTCTime)
bucket_createdAt = Lens.lens (\Bucket' {createdAt} -> createdAt) (\s@Bucket' {} a -> s {createdAt = a} :: Bucket) Prelude.. Lens.mapping Data._Time

-- | An object that describes the location of the bucket, such as the Amazon
-- Web Services Region and Availability Zone.
bucket_location :: Lens.Lens' Bucket (Prelude.Maybe ResourceLocation)
bucket_location = Lens.lens (\Bucket' {location} -> location) (\s@Bucket' {} a -> s {location = a} :: Bucket)

-- | The name of the bucket.
bucket_name :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_name = Lens.lens (\Bucket' {name} -> name) (\s@Bucket' {} a -> s {name = a} :: Bucket)

-- | Indicates whether object versioning is enabled for the bucket.
--
-- The following options can be configured:
--
-- -   @Enabled@ - Object versioning is enabled.
--
-- -   @Suspended@ - Object versioning was previously enabled but is
--     currently suspended. Existing object versions are retained.
--
-- -   @NeverEnabled@ - Object versioning has never been enabled.
bucket_objectVersioning :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_objectVersioning = Lens.lens (\Bucket' {objectVersioning} -> objectVersioning) (\s@Bucket' {} a -> s {objectVersioning = a} :: Bucket)

-- | An array of strings that specify the Amazon Web Services account IDs
-- that have read-only access to the bucket.
bucket_readonlyAccessAccounts :: Lens.Lens' Bucket (Prelude.Maybe [Prelude.Text])
bucket_readonlyAccessAccounts = Lens.lens (\Bucket' {readonlyAccessAccounts} -> readonlyAccessAccounts) (\s@Bucket' {} a -> s {readonlyAccessAccounts = a} :: Bucket) Prelude.. Lens.mapping Lens.coerced

-- | The Lightsail resource type of the bucket (for example, @Bucket@).
bucket_resourceType :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_resourceType = Lens.lens (\Bucket' {resourceType} -> resourceType) (\s@Bucket' {} a -> s {resourceType = a} :: Bucket)

-- | An array of objects that describe Lightsail instances that have access
-- to the bucket.
--
-- Use the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_SetResourceAccessForBucket.html SetResourceAccessForBucket>
-- action to update the instances that have access to a bucket.
bucket_resourcesReceivingAccess :: Lens.Lens' Bucket (Prelude.Maybe [ResourceReceivingAccess])
bucket_resourcesReceivingAccess = Lens.lens (\Bucket' {resourcesReceivingAccess} -> resourcesReceivingAccess) (\s@Bucket' {} a -> s {resourcesReceivingAccess = a} :: Bucket) Prelude.. Lens.mapping Lens.coerced

-- | An object that describes the state of the bucket.
bucket_state :: Lens.Lens' Bucket (Prelude.Maybe BucketState)
bucket_state = Lens.lens (\Bucket' {state} -> state) (\s@Bucket' {} a -> s {state = a} :: Bucket)

-- | The support code for a bucket. Include this code in your email to
-- support when you have questions about a Lightsail bucket. This code
-- enables our support team to look up your Lightsail information more
-- easily.
bucket_supportCode :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_supportCode = Lens.lens (\Bucket' {supportCode} -> supportCode) (\s@Bucket' {} a -> s {supportCode = a} :: Bucket)

-- | The tag keys and optional values for the bucket. For more information,
-- see
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Tags in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
bucket_tags :: Lens.Lens' Bucket (Prelude.Maybe [Tag])
bucket_tags = Lens.lens (\Bucket' {tags} -> tags) (\s@Bucket' {} a -> s {tags = a} :: Bucket) Prelude.. Lens.mapping Lens.coerced

-- | The URL of the bucket.
bucket_url :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_url = Lens.lens (\Bucket' {url} -> url) (\s@Bucket' {} a -> s {url = a} :: Bucket)

instance Data.FromJSON Bucket where
  parseJSON =
    Data.withObject
      "Bucket"
      ( \x ->
          Bucket'
            Prelude.<$> (x Data..:? "ableToUpdateBundle")
            Prelude.<*> (x Data..:? "accessLogConfig")
            Prelude.<*> (x Data..:? "accessRules")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "bundleId")
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "location")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "objectVersioning")
            Prelude.<*> ( x
                            Data..:? "readonlyAccessAccounts"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "resourceType")
            Prelude.<*> ( x
                            Data..:? "resourcesReceivingAccess"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "state")
            Prelude.<*> (x Data..:? "supportCode")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "url")
      )

instance Prelude.Hashable Bucket where
  hashWithSalt _salt Bucket' {..} =
    _salt
      `Prelude.hashWithSalt` ableToUpdateBundle
      `Prelude.hashWithSalt` accessLogConfig
      `Prelude.hashWithSalt` accessRules
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` bundleId
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` objectVersioning
      `Prelude.hashWithSalt` readonlyAccessAccounts
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourcesReceivingAccess
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` supportCode
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` url

instance Prelude.NFData Bucket where
  rnf Bucket' {..} =
    Prelude.rnf ableToUpdateBundle
      `Prelude.seq` Prelude.rnf accessLogConfig
      `Prelude.seq` Prelude.rnf accessRules
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf bundleId
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf objectVersioning
      `Prelude.seq` Prelude.rnf readonlyAccessAccounts
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourcesReceivingAccess
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf supportCode
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf url
