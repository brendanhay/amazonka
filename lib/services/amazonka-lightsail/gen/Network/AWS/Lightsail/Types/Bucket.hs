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
-- Module      : Network.AWS.Lightsail.Types.Bucket
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.Bucket where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types.AccessRules
import Network.AWS.Lightsail.Types.BucketState
import Network.AWS.Lightsail.Types.ResourceLocation
import Network.AWS.Lightsail.Types.ResourceReceivingAccess
import Network.AWS.Lightsail.Types.Tag
import qualified Network.AWS.Prelude as Prelude

-- | Describes an Amazon Lightsail bucket.
--
-- /See:/ 'newBucket' smart constructor.
data Bucket = Bucket'
  { -- | Indicates whether object versioning is enabled for the bucket.
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
    -- | The ID of the bundle currently applied to the bucket.
    --
    -- A bucket bundle specifies the monthly cost, storage space, and data
    -- transfer quota for a bucket.
    --
    -- Use the UpdateBucketBundle action to change the bundle of a bucket.
    bundleId :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe Lightsail instances that have access
    -- to the bucket.
    --
    -- Use the SetResourceAccessForBucket action to update the instances that
    -- have access to a bucket.
    resourcesReceivingAccess :: Prelude.Maybe [ResourceReceivingAccess],
    -- | An array of strings that specify the AWS account IDs that have read-only
    -- access to the bucket.
    readonlyAccessAccounts :: Prelude.Maybe [Prelude.Text],
    -- | The timestamp when the distribution was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the bucket.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The support code for a bucket. Include this code in your email to
    -- support when you have questions about a Lightsail bucket. This code
    -- enables our support team to look up your Lightsail information more
    -- easily.
    supportCode :: Prelude.Maybe Prelude.Text,
    -- | The Lightsail resource type of the bucket (for example, @Bucket@).
    resourceType :: Prelude.Maybe Prelude.Text,
    -- | An object that describes the state of the bucket.
    state :: Prelude.Maybe BucketState,
    -- | The name of the bucket.
    name :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the bundle that is currently applied to a bucket can
    -- be changed to another bundle.
    --
    -- You can update a bucket\'s bundle only one time within a monthly AWS
    -- billing cycle.
    --
    -- Use the UpdateBucketBundle action to change a bucket\'s bundle.
    ableToUpdateBundle :: Prelude.Maybe Prelude.Bool,
    -- | An object that describes the access rules of the bucket.
    accessRules :: Prelude.Maybe AccessRules,
    -- | The tag keys and optional values for the bucket. For more information,
    -- see
    -- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Tags in Amazon Lightsail>
    -- in the /Amazon Lightsail Developer Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The URL of the bucket.
    url :: Prelude.Maybe Prelude.Text,
    location :: Prelude.Maybe ResourceLocation
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
-- 'bundleId', 'bucket_bundleId' - The ID of the bundle currently applied to the bucket.
--
-- A bucket bundle specifies the monthly cost, storage space, and data
-- transfer quota for a bucket.
--
-- Use the UpdateBucketBundle action to change the bundle of a bucket.
--
-- 'resourcesReceivingAccess', 'bucket_resourcesReceivingAccess' - An array of objects that describe Lightsail instances that have access
-- to the bucket.
--
-- Use the SetResourceAccessForBucket action to update the instances that
-- have access to a bucket.
--
-- 'readonlyAccessAccounts', 'bucket_readonlyAccessAccounts' - An array of strings that specify the AWS account IDs that have read-only
-- access to the bucket.
--
-- 'createdAt', 'bucket_createdAt' - The timestamp when the distribution was created.
--
-- 'arn', 'bucket_arn' - The Amazon Resource Name (ARN) of the bucket.
--
-- 'supportCode', 'bucket_supportCode' - The support code for a bucket. Include this code in your email to
-- support when you have questions about a Lightsail bucket. This code
-- enables our support team to look up your Lightsail information more
-- easily.
--
-- 'resourceType', 'bucket_resourceType' - The Lightsail resource type of the bucket (for example, @Bucket@).
--
-- 'state', 'bucket_state' - An object that describes the state of the bucket.
--
-- 'name', 'bucket_name' - The name of the bucket.
--
-- 'ableToUpdateBundle', 'bucket_ableToUpdateBundle' - Indicates whether the bundle that is currently applied to a bucket can
-- be changed to another bundle.
--
-- You can update a bucket\'s bundle only one time within a monthly AWS
-- billing cycle.
--
-- Use the UpdateBucketBundle action to change a bucket\'s bundle.
--
-- 'accessRules', 'bucket_accessRules' - An object that describes the access rules of the bucket.
--
-- 'tags', 'bucket_tags' - The tag keys and optional values for the bucket. For more information,
-- see
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Tags in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
--
-- 'url', 'bucket_url' - The URL of the bucket.
--
-- 'location', 'bucket_location' - Undocumented member.
newBucket ::
  Bucket
newBucket =
  Bucket'
    { objectVersioning = Prelude.Nothing,
      bundleId = Prelude.Nothing,
      resourcesReceivingAccess = Prelude.Nothing,
      readonlyAccessAccounts = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      arn = Prelude.Nothing,
      supportCode = Prelude.Nothing,
      resourceType = Prelude.Nothing,
      state = Prelude.Nothing,
      name = Prelude.Nothing,
      ableToUpdateBundle = Prelude.Nothing,
      accessRules = Prelude.Nothing,
      tags = Prelude.Nothing,
      url = Prelude.Nothing,
      location = Prelude.Nothing
    }

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

-- | The ID of the bundle currently applied to the bucket.
--
-- A bucket bundle specifies the monthly cost, storage space, and data
-- transfer quota for a bucket.
--
-- Use the UpdateBucketBundle action to change the bundle of a bucket.
bucket_bundleId :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_bundleId = Lens.lens (\Bucket' {bundleId} -> bundleId) (\s@Bucket' {} a -> s {bundleId = a} :: Bucket)

-- | An array of objects that describe Lightsail instances that have access
-- to the bucket.
--
-- Use the SetResourceAccessForBucket action to update the instances that
-- have access to a bucket.
bucket_resourcesReceivingAccess :: Lens.Lens' Bucket (Prelude.Maybe [ResourceReceivingAccess])
bucket_resourcesReceivingAccess = Lens.lens (\Bucket' {resourcesReceivingAccess} -> resourcesReceivingAccess) (\s@Bucket' {} a -> s {resourcesReceivingAccess = a} :: Bucket) Prelude.. Lens.mapping Lens._Coerce

-- | An array of strings that specify the AWS account IDs that have read-only
-- access to the bucket.
bucket_readonlyAccessAccounts :: Lens.Lens' Bucket (Prelude.Maybe [Prelude.Text])
bucket_readonlyAccessAccounts = Lens.lens (\Bucket' {readonlyAccessAccounts} -> readonlyAccessAccounts) (\s@Bucket' {} a -> s {readonlyAccessAccounts = a} :: Bucket) Prelude.. Lens.mapping Lens._Coerce

-- | The timestamp when the distribution was created.
bucket_createdAt :: Lens.Lens' Bucket (Prelude.Maybe Prelude.UTCTime)
bucket_createdAt = Lens.lens (\Bucket' {createdAt} -> createdAt) (\s@Bucket' {} a -> s {createdAt = a} :: Bucket) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the bucket.
bucket_arn :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_arn = Lens.lens (\Bucket' {arn} -> arn) (\s@Bucket' {} a -> s {arn = a} :: Bucket)

-- | The support code for a bucket. Include this code in your email to
-- support when you have questions about a Lightsail bucket. This code
-- enables our support team to look up your Lightsail information more
-- easily.
bucket_supportCode :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_supportCode = Lens.lens (\Bucket' {supportCode} -> supportCode) (\s@Bucket' {} a -> s {supportCode = a} :: Bucket)

-- | The Lightsail resource type of the bucket (for example, @Bucket@).
bucket_resourceType :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_resourceType = Lens.lens (\Bucket' {resourceType} -> resourceType) (\s@Bucket' {} a -> s {resourceType = a} :: Bucket)

-- | An object that describes the state of the bucket.
bucket_state :: Lens.Lens' Bucket (Prelude.Maybe BucketState)
bucket_state = Lens.lens (\Bucket' {state} -> state) (\s@Bucket' {} a -> s {state = a} :: Bucket)

-- | The name of the bucket.
bucket_name :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_name = Lens.lens (\Bucket' {name} -> name) (\s@Bucket' {} a -> s {name = a} :: Bucket)

-- | Indicates whether the bundle that is currently applied to a bucket can
-- be changed to another bundle.
--
-- You can update a bucket\'s bundle only one time within a monthly AWS
-- billing cycle.
--
-- Use the UpdateBucketBundle action to change a bucket\'s bundle.
bucket_ableToUpdateBundle :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Bool)
bucket_ableToUpdateBundle = Lens.lens (\Bucket' {ableToUpdateBundle} -> ableToUpdateBundle) (\s@Bucket' {} a -> s {ableToUpdateBundle = a} :: Bucket)

-- | An object that describes the access rules of the bucket.
bucket_accessRules :: Lens.Lens' Bucket (Prelude.Maybe AccessRules)
bucket_accessRules = Lens.lens (\Bucket' {accessRules} -> accessRules) (\s@Bucket' {} a -> s {accessRules = a} :: Bucket)

-- | The tag keys and optional values for the bucket. For more information,
-- see
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-tags Tags in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
bucket_tags :: Lens.Lens' Bucket (Prelude.Maybe [Tag])
bucket_tags = Lens.lens (\Bucket' {tags} -> tags) (\s@Bucket' {} a -> s {tags = a} :: Bucket) Prelude.. Lens.mapping Lens._Coerce

-- | The URL of the bucket.
bucket_url :: Lens.Lens' Bucket (Prelude.Maybe Prelude.Text)
bucket_url = Lens.lens (\Bucket' {url} -> url) (\s@Bucket' {} a -> s {url = a} :: Bucket)

-- | Undocumented member.
bucket_location :: Lens.Lens' Bucket (Prelude.Maybe ResourceLocation)
bucket_location = Lens.lens (\Bucket' {location} -> location) (\s@Bucket' {} a -> s {location = a} :: Bucket)

instance Core.FromJSON Bucket where
  parseJSON =
    Core.withObject
      "Bucket"
      ( \x ->
          Bucket'
            Prelude.<$> (x Core..:? "objectVersioning")
            Prelude.<*> (x Core..:? "bundleId")
            Prelude.<*> ( x Core..:? "resourcesReceivingAccess"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> ( x Core..:? "readonlyAccessAccounts"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "createdAt")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "supportCode")
            Prelude.<*> (x Core..:? "resourceType")
            Prelude.<*> (x Core..:? "state")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "ableToUpdateBundle")
            Prelude.<*> (x Core..:? "accessRules")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "url")
            Prelude.<*> (x Core..:? "location")
      )

instance Prelude.Hashable Bucket

instance Prelude.NFData Bucket
