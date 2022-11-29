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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.Bucket where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
  { -- | The tag keys and optional values for the bucket. For more information,
    -- see
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Tags in Amazon Lightsail>
    -- in the /Amazon Lightsail Developer Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The Lightsail resource type of the bucket (for example, @Bucket@).
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe Lightsail instances that have access
    -- to the bucket.
    --
    -- Use the
    -- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_SetResourceAccessForBucket.html SetResourceAccessForBucket>
    -- action to update the instances that have access to a bucket.
    resourcesReceivingAccess :: Prelude.Maybe [ResourceReceivingAccess],
    -- | The name of the bucket.
    name :: Prelude.Maybe Prelude.Text,
    -- | An array of strings that specify the Amazon Web Services account IDs
    -- that have read-only access to the bucket.
    readonlyAccessAccounts :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the bucket.
    arn :: Prelude.Maybe Prelude.Text,
    -- | An object that describes the state of the bucket.
    state :: Prelude.Maybe BucketState,
    -- | The URL of the bucket.
    url :: Prelude.Maybe Prelude.Text,
    -- | An object that describes the location of the bucket, such as the Amazon
    -- Web Services Region and Availability Zone.
    location :: Prelude.Maybe ResourceLocation,
    -- | Indicates whether the bundle that is currently applied to a bucket can
    -- be changed to another bundle.
    --
    -- You can update a bucket\'s bundle only one time within a monthly Amazon
    -- Web Services billing cycle.
    --
    -- Use the
    -- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_UpdateBucketBundle.html UpdateBucketBundle>
    -- action to change a bucket\'s bundle.
    ableToUpdateBundle :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the bundle currently applied to the bucket.
    --
    -- A bucket bundle specifies the monthly cost, storage space, and data
    -- transfer quota for a bucket.
    --
    -- Use the
    -- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_UpdateBucketBundle.html UpdateBucketBundle>
    -- action to change the bundle of a bucket.
    bundleId :: Prelude.Maybe Prelude.Text,
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
    -- | An object that describes the access rules of the bucket.
    accessRules :: Prelude.Maybe AccessRules,
    -- | The support code for a bucket. Include this code in your email to
    -- support when you have questions about a Lightsail bucket. This code
    -- enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The timestamp when the distribution was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | An object that describes the access log configuration for the bucket.
    accessLogConfig :: Prelude.Maybe BucketAccessLogConfig
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
-- 'tags', 'bucket_tags' - The tag keys and optional values for the bucket. For more information,
-- see
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Tags in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
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
-- 'name', 'bucket_name' - The name of the bucket.
--
-- 'readonlyAccessAccounts', 'bucket_readonlyAccessAccounts' - An array of strings that specify the Amazon Web Services account IDs
-- that have read-only access to the bucket.
--
-- 'arn', 'bucket_arn' - The Amazon Resource Name (ARN) of the bucket.
--
-- 'state', 'bucket_state' - An object that describes the state of the bucket.
--
-- 'url', 'bucket_url' - The URL of the bucket.
--
-- 'location', 'bucket_location' - An object that describes the location of the bucket, such as the Amazon
-- Web Services Region and Availability Zone.
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
-- 'bundleId', 'bucket_bundleId' - The ID of the bundle currently applied to the bucket.
--
-- A bucket bundle specifies the monthly cost, storage space, and data
-- transfer quota for a bucket.
--
-- Use the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_UpdateBucketBundle.html UpdateBucketBundle>
-- action to change the bundle of a bucket.
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
-- 'accessRules', 'bucket_accessRules' - An object that describes the access rules of the bucket.
--
-- 'supportCode', 'bucket_supportCode' - The support code for a bucket. Include this code in your email to
-- support when you have questions about a Lightsail bucket. This code
-- enables our support team to look up your Lightsail information more
-- easily.
--
-- 'createdAt', 'bucket_createdAt' - The timestamp when the distribution was created.
--
-- 'accessLogConfig', 'bucket_accessLogConfig' - An object that describes the access log configuration for the bucket.
newBucket ::
  Bucket
newBucket =
  Bucket'
    { tags = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      resourcesReceivingAccess = Prelude.Nothing,
      name = Prelude.Nothing,
      readonlyAccessAccounts = Prelude.Nothing,
      arn = Prelude.Nothing,
      state = Prelude.Nothing,
      url = Prelude.Nothing,
      location = Prelude.Nothing,
      ableToUpdateBundle = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      objectVersioning = Prelude.Nothing,
      accessRules = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      accessLogConfig = Prelude.Nothing
    }

-- | The tag keys and optional values for the bucket. For more information,
-- see
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Tags in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
bucket_tags :: Lens.Lens' Bucket (Prelude.Maybe [Tag])
bucket_tags = Lens.lens (\Bucket' {tags} -> tags) (\s@Bucket' {} a -> s {tags = a} :: Bucket) Prelude.. Lens.mapping Lens.coerced

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

-- | The name of the bucket.
bucket_name :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_name = Lens.lens (\Bucket' {name} -> name) (\s@Bucket' {} a -> s {name = a} :: Bucket)

-- | An array of strings that specify the Amazon Web Services account IDs
-- that have read-only access to the bucket.
bucket_readonlyAccessAccounts :: Lens.Lens' Bucket (Prelude.Maybe [Prelude.Text])
bucket_readonlyAccessAccounts = Lens.lens (\Bucket' {readonlyAccessAccounts} -> readonlyAccessAccounts) (\s@Bucket' {} a -> s {readonlyAccessAccounts = a} :: Bucket) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the bucket.
bucket_arn :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_arn = Lens.lens (\Bucket' {arn} -> arn) (\s@Bucket' {} a -> s {arn = a} :: Bucket)

-- | An object that describes the state of the bucket.
bucket_state :: Lens.Lens' Bucket (Prelude.Maybe BucketState)
bucket_state = Lens.lens (\Bucket' {state} -> state) (\s@Bucket' {} a -> s {state = a} :: Bucket)

-- | The URL of the bucket.
bucket_url :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_url = Lens.lens (\Bucket' {url} -> url) (\s@Bucket' {} a -> s {url = a} :: Bucket)

-- | An object that describes the location of the bucket, such as the Amazon
-- Web Services Region and Availability Zone.
bucket_location :: Lens.Lens' Bucket (Prelude.Maybe ResourceLocation)
bucket_location = Lens.lens (\Bucket' {location} -> location) (\s@Bucket' {} a -> s {location = a} :: Bucket)

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

-- | An object that describes the access rules of the bucket.
bucket_accessRules :: Lens.Lens' Bucket (Prelude.Maybe AccessRules)
bucket_accessRules = Lens.lens (\Bucket' {accessRules} -> accessRules) (\s@Bucket' {} a -> s {accessRules = a} :: Bucket)

-- | The support code for a bucket. Include this code in your email to
-- support when you have questions about a Lightsail bucket. This code
-- enables our support team to look up your Lightsail information more
-- easily.
bucket_supportCode :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_supportCode = Lens.lens (\Bucket' {supportCode} -> supportCode) (\s@Bucket' {} a -> s {supportCode = a} :: Bucket)

-- | The timestamp when the distribution was created.
bucket_createdAt :: Lens.Lens' Bucket (Prelude.Maybe Prelude.UTCTime)
bucket_createdAt = Lens.lens (\Bucket' {createdAt} -> createdAt) (\s@Bucket' {} a -> s {createdAt = a} :: Bucket) Prelude.. Lens.mapping Core._Time

-- | An object that describes the access log configuration for the bucket.
bucket_accessLogConfig :: Lens.Lens' Bucket (Prelude.Maybe BucketAccessLogConfig)
bucket_accessLogConfig = Lens.lens (\Bucket' {accessLogConfig} -> accessLogConfig) (\s@Bucket' {} a -> s {accessLogConfig = a} :: Bucket)

instance Core.FromJSON Bucket where
  parseJSON =
    Core.withObject
      "Bucket"
      ( \x ->
          Bucket'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> ( x Core..:? "resourcesReceivingAccess"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> ( x Core..:? "readonlyAccessAccounts"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "url")
            Prelude.<*> (x Core..:? "location")
            Prelude.<*> (x Core..:? "ableToUpdateBundle")
            Prelude.<*> (x Core..:? "bundleId")
            Prelude.<*> (x Core..:? "objectVersioning")
            Prelude.<*> (x Core..:? "accessRules")
            Prelude.<*> (x Core..:? "supportCode")
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "accessLogConfig")
      )

instance Prelude.Hashable Bucket where
  hashWithSalt _salt Bucket' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` resourceType
      `Prelude.hashWithSalt` resourcesReceivingAccess
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` readonlyAccessAccounts
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` url
      `Prelude.hashWithSalt` location
      `Prelude.hashWithSalt` ableToUpdateBundle
      `Prelude.hashWithSalt` bundleId
      `Prelude.hashWithSalt` objectVersioning
      `Prelude.hashWithSalt` accessRules
      `Prelude.hashWithSalt` supportCode
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` accessLogConfig

instance Prelude.NFData Bucket where
  rnf Bucket' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf resourceType
      `Prelude.seq` Prelude.rnf resourcesReceivingAccess
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf readonlyAccessAccounts
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf url
      `Prelude.seq` Prelude.rnf location
      `Prelude.seq` Prelude.rnf ableToUpdateBundle
      `Prelude.seq` Prelude.rnf bundleId
      `Prelude.seq` Prelude.rnf objectVersioning
      `Prelude.seq` Prelude.rnf accessRules
      `Prelude.seq` Prelude.rnf supportCode
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf accessLogConfig
