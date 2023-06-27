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
-- Module      : Amazonka.SecurityLake.Types.SubscriberResource
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityLake.Types.SubscriberResource where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityLake.Types.AccessType
import Amazonka.SecurityLake.Types.AwsIdentity
import Amazonka.SecurityLake.Types.LogSourceResource
import Amazonka.SecurityLake.Types.SubscriberStatus

-- | Provides details about the Amazon Security Lake account subscription.
-- Subscribers are notified of new objects for a source as the data is
-- written to your Amazon S3 bucket for Security Lake.
--
-- /See:/ 'newSubscriberResource' smart constructor.
data SubscriberResource = SubscriberResource'
  { -- | You can choose to notify subscribers of new objects with an Amazon
    -- Simple Queue Service (Amazon SQS) queue or through messaging to an HTTPS
    -- endpoint provided by the subscriber.
    --
    -- Subscribers can consume data by directly querying Lake Formation tables
    -- in your Amazon S3 bucket through services like Amazon Athena. This
    -- subscription type is defined as @LAKEFORMATION@.
    accessTypes :: Prelude.Maybe [AccessType],
    -- | The date and time when the subscriber was created.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The Amazon Resource Name (ARN) which uniquely defines the AWS RAM
    -- resource share. Before accepting the RAM resource share invitation, you
    -- can view details related to the RAM resource share.
    --
    -- This field is available only for Lake Formation subscribers created
    -- after March 8, 2023.
    resourceShareArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource share.
    resourceShareName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) specifying the role of the subscriber.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the Amazon S3 bucket.
    s3BucketArn :: Prelude.Maybe Prelude.Text,
    -- | The subscriber descriptions for a subscriber account. The description
    -- for a subscriber includes @subscriberName@, @accountID@, @externalID@,
    -- and @subscriberId@.
    subscriberDescription :: Prelude.Maybe Prelude.Text,
    -- | The subscriber endpoint to which exception messages are posted.
    subscriberEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The subscriber status of the Amazon Security Lake subscriber account.
    subscriberStatus :: Prelude.Maybe SubscriberStatus,
    -- | The date and time when the subscriber was last updated.
    updatedAt :: Prelude.Maybe Data.ISO8601,
    -- | Amazon Security Lake supports log and event collection for natively
    -- supported Amazon Web Services. For more information, see the Amazon
    -- Security Lake User Guide.
    sources :: [LogSourceResource],
    -- | The subscriber ARN of the Amazon Security Lake subscriber account.
    subscriberArn :: Prelude.Text,
    -- | The subscriber ID of the Amazon Security Lake subscriber account.
    subscriberId :: Prelude.Text,
    -- | The AWS identity used to access your data.
    subscriberIdentity :: AwsIdentity,
    -- | The name of your Amazon Security Lake subscriber account.
    subscriberName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SubscriberResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessTypes', 'subscriberResource_accessTypes' - You can choose to notify subscribers of new objects with an Amazon
-- Simple Queue Service (Amazon SQS) queue or through messaging to an HTTPS
-- endpoint provided by the subscriber.
--
-- Subscribers can consume data by directly querying Lake Formation tables
-- in your Amazon S3 bucket through services like Amazon Athena. This
-- subscription type is defined as @LAKEFORMATION@.
--
-- 'createdAt', 'subscriberResource_createdAt' - The date and time when the subscriber was created.
--
-- 'resourceShareArn', 'subscriberResource_resourceShareArn' - The Amazon Resource Name (ARN) which uniquely defines the AWS RAM
-- resource share. Before accepting the RAM resource share invitation, you
-- can view details related to the RAM resource share.
--
-- This field is available only for Lake Formation subscribers created
-- after March 8, 2023.
--
-- 'resourceShareName', 'subscriberResource_resourceShareName' - The name of the resource share.
--
-- 'roleArn', 'subscriberResource_roleArn' - The Amazon Resource Name (ARN) specifying the role of the subscriber.
--
-- 's3BucketArn', 'subscriberResource_s3BucketArn' - The ARN for the Amazon S3 bucket.
--
-- 'subscriberDescription', 'subscriberResource_subscriberDescription' - The subscriber descriptions for a subscriber account. The description
-- for a subscriber includes @subscriberName@, @accountID@, @externalID@,
-- and @subscriberId@.
--
-- 'subscriberEndpoint', 'subscriberResource_subscriberEndpoint' - The subscriber endpoint to which exception messages are posted.
--
-- 'subscriberStatus', 'subscriberResource_subscriberStatus' - The subscriber status of the Amazon Security Lake subscriber account.
--
-- 'updatedAt', 'subscriberResource_updatedAt' - The date and time when the subscriber was last updated.
--
-- 'sources', 'subscriberResource_sources' - Amazon Security Lake supports log and event collection for natively
-- supported Amazon Web Services. For more information, see the Amazon
-- Security Lake User Guide.
--
-- 'subscriberArn', 'subscriberResource_subscriberArn' - The subscriber ARN of the Amazon Security Lake subscriber account.
--
-- 'subscriberId', 'subscriberResource_subscriberId' - The subscriber ID of the Amazon Security Lake subscriber account.
--
-- 'subscriberIdentity', 'subscriberResource_subscriberIdentity' - The AWS identity used to access your data.
--
-- 'subscriberName', 'subscriberResource_subscriberName' - The name of your Amazon Security Lake subscriber account.
newSubscriberResource ::
  -- | 'subscriberArn'
  Prelude.Text ->
  -- | 'subscriberId'
  Prelude.Text ->
  -- | 'subscriberIdentity'
  AwsIdentity ->
  -- | 'subscriberName'
  Prelude.Text ->
  SubscriberResource
newSubscriberResource
  pSubscriberArn_
  pSubscriberId_
  pSubscriberIdentity_
  pSubscriberName_ =
    SubscriberResource'
      { accessTypes = Prelude.Nothing,
        createdAt = Prelude.Nothing,
        resourceShareArn = Prelude.Nothing,
        resourceShareName = Prelude.Nothing,
        roleArn = Prelude.Nothing,
        s3BucketArn = Prelude.Nothing,
        subscriberDescription = Prelude.Nothing,
        subscriberEndpoint = Prelude.Nothing,
        subscriberStatus = Prelude.Nothing,
        updatedAt = Prelude.Nothing,
        sources = Prelude.mempty,
        subscriberArn = pSubscriberArn_,
        subscriberId = pSubscriberId_,
        subscriberIdentity = pSubscriberIdentity_,
        subscriberName = pSubscriberName_
      }

-- | You can choose to notify subscribers of new objects with an Amazon
-- Simple Queue Service (Amazon SQS) queue or through messaging to an HTTPS
-- endpoint provided by the subscriber.
--
-- Subscribers can consume data by directly querying Lake Formation tables
-- in your Amazon S3 bucket through services like Amazon Athena. This
-- subscription type is defined as @LAKEFORMATION@.
subscriberResource_accessTypes :: Lens.Lens' SubscriberResource (Prelude.Maybe [AccessType])
subscriberResource_accessTypes = Lens.lens (\SubscriberResource' {accessTypes} -> accessTypes) (\s@SubscriberResource' {} a -> s {accessTypes = a} :: SubscriberResource) Prelude.. Lens.mapping Lens.coerced

-- | The date and time when the subscriber was created.
subscriberResource_createdAt :: Lens.Lens' SubscriberResource (Prelude.Maybe Prelude.UTCTime)
subscriberResource_createdAt = Lens.lens (\SubscriberResource' {createdAt} -> createdAt) (\s@SubscriberResource' {} a -> s {createdAt = a} :: SubscriberResource) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) which uniquely defines the AWS RAM
-- resource share. Before accepting the RAM resource share invitation, you
-- can view details related to the RAM resource share.
--
-- This field is available only for Lake Formation subscribers created
-- after March 8, 2023.
subscriberResource_resourceShareArn :: Lens.Lens' SubscriberResource (Prelude.Maybe Prelude.Text)
subscriberResource_resourceShareArn = Lens.lens (\SubscriberResource' {resourceShareArn} -> resourceShareArn) (\s@SubscriberResource' {} a -> s {resourceShareArn = a} :: SubscriberResource)

-- | The name of the resource share.
subscriberResource_resourceShareName :: Lens.Lens' SubscriberResource (Prelude.Maybe Prelude.Text)
subscriberResource_resourceShareName = Lens.lens (\SubscriberResource' {resourceShareName} -> resourceShareName) (\s@SubscriberResource' {} a -> s {resourceShareName = a} :: SubscriberResource)

-- | The Amazon Resource Name (ARN) specifying the role of the subscriber.
subscriberResource_roleArn :: Lens.Lens' SubscriberResource (Prelude.Maybe Prelude.Text)
subscriberResource_roleArn = Lens.lens (\SubscriberResource' {roleArn} -> roleArn) (\s@SubscriberResource' {} a -> s {roleArn = a} :: SubscriberResource)

-- | The ARN for the Amazon S3 bucket.
subscriberResource_s3BucketArn :: Lens.Lens' SubscriberResource (Prelude.Maybe Prelude.Text)
subscriberResource_s3BucketArn = Lens.lens (\SubscriberResource' {s3BucketArn} -> s3BucketArn) (\s@SubscriberResource' {} a -> s {s3BucketArn = a} :: SubscriberResource)

-- | The subscriber descriptions for a subscriber account. The description
-- for a subscriber includes @subscriberName@, @accountID@, @externalID@,
-- and @subscriberId@.
subscriberResource_subscriberDescription :: Lens.Lens' SubscriberResource (Prelude.Maybe Prelude.Text)
subscriberResource_subscriberDescription = Lens.lens (\SubscriberResource' {subscriberDescription} -> subscriberDescription) (\s@SubscriberResource' {} a -> s {subscriberDescription = a} :: SubscriberResource)

-- | The subscriber endpoint to which exception messages are posted.
subscriberResource_subscriberEndpoint :: Lens.Lens' SubscriberResource (Prelude.Maybe Prelude.Text)
subscriberResource_subscriberEndpoint = Lens.lens (\SubscriberResource' {subscriberEndpoint} -> subscriberEndpoint) (\s@SubscriberResource' {} a -> s {subscriberEndpoint = a} :: SubscriberResource)

-- | The subscriber status of the Amazon Security Lake subscriber account.
subscriberResource_subscriberStatus :: Lens.Lens' SubscriberResource (Prelude.Maybe SubscriberStatus)
subscriberResource_subscriberStatus = Lens.lens (\SubscriberResource' {subscriberStatus} -> subscriberStatus) (\s@SubscriberResource' {} a -> s {subscriberStatus = a} :: SubscriberResource)

-- | The date and time when the subscriber was last updated.
subscriberResource_updatedAt :: Lens.Lens' SubscriberResource (Prelude.Maybe Prelude.UTCTime)
subscriberResource_updatedAt = Lens.lens (\SubscriberResource' {updatedAt} -> updatedAt) (\s@SubscriberResource' {} a -> s {updatedAt = a} :: SubscriberResource) Prelude.. Lens.mapping Data._Time

-- | Amazon Security Lake supports log and event collection for natively
-- supported Amazon Web Services. For more information, see the Amazon
-- Security Lake User Guide.
subscriberResource_sources :: Lens.Lens' SubscriberResource [LogSourceResource]
subscriberResource_sources = Lens.lens (\SubscriberResource' {sources} -> sources) (\s@SubscriberResource' {} a -> s {sources = a} :: SubscriberResource) Prelude.. Lens.coerced

-- | The subscriber ARN of the Amazon Security Lake subscriber account.
subscriberResource_subscriberArn :: Lens.Lens' SubscriberResource Prelude.Text
subscriberResource_subscriberArn = Lens.lens (\SubscriberResource' {subscriberArn} -> subscriberArn) (\s@SubscriberResource' {} a -> s {subscriberArn = a} :: SubscriberResource)

-- | The subscriber ID of the Amazon Security Lake subscriber account.
subscriberResource_subscriberId :: Lens.Lens' SubscriberResource Prelude.Text
subscriberResource_subscriberId = Lens.lens (\SubscriberResource' {subscriberId} -> subscriberId) (\s@SubscriberResource' {} a -> s {subscriberId = a} :: SubscriberResource)

-- | The AWS identity used to access your data.
subscriberResource_subscriberIdentity :: Lens.Lens' SubscriberResource AwsIdentity
subscriberResource_subscriberIdentity = Lens.lens (\SubscriberResource' {subscriberIdentity} -> subscriberIdentity) (\s@SubscriberResource' {} a -> s {subscriberIdentity = a} :: SubscriberResource)

-- | The name of your Amazon Security Lake subscriber account.
subscriberResource_subscriberName :: Lens.Lens' SubscriberResource Prelude.Text
subscriberResource_subscriberName = Lens.lens (\SubscriberResource' {subscriberName} -> subscriberName) (\s@SubscriberResource' {} a -> s {subscriberName = a} :: SubscriberResource)

instance Data.FromJSON SubscriberResource where
  parseJSON =
    Data.withObject
      "SubscriberResource"
      ( \x ->
          SubscriberResource'
            Prelude.<$> (x Data..:? "accessTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "resourceShareArn")
            Prelude.<*> (x Data..:? "resourceShareName")
            Prelude.<*> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "s3BucketArn")
            Prelude.<*> (x Data..:? "subscriberDescription")
            Prelude.<*> (x Data..:? "subscriberEndpoint")
            Prelude.<*> (x Data..:? "subscriberStatus")
            Prelude.<*> (x Data..:? "updatedAt")
            Prelude.<*> (x Data..:? "sources" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "subscriberArn")
            Prelude.<*> (x Data..: "subscriberId")
            Prelude.<*> (x Data..: "subscriberIdentity")
            Prelude.<*> (x Data..: "subscriberName")
      )

instance Prelude.Hashable SubscriberResource where
  hashWithSalt _salt SubscriberResource' {..} =
    _salt
      `Prelude.hashWithSalt` accessTypes
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` resourceShareArn
      `Prelude.hashWithSalt` resourceShareName
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` s3BucketArn
      `Prelude.hashWithSalt` subscriberDescription
      `Prelude.hashWithSalt` subscriberEndpoint
      `Prelude.hashWithSalt` subscriberStatus
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` sources
      `Prelude.hashWithSalt` subscriberArn
      `Prelude.hashWithSalt` subscriberId
      `Prelude.hashWithSalt` subscriberIdentity
      `Prelude.hashWithSalt` subscriberName

instance Prelude.NFData SubscriberResource where
  rnf SubscriberResource' {..} =
    Prelude.rnf accessTypes
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf resourceShareArn
      `Prelude.seq` Prelude.rnf resourceShareName
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf s3BucketArn
      `Prelude.seq` Prelude.rnf subscriberDescription
      `Prelude.seq` Prelude.rnf subscriberEndpoint
      `Prelude.seq` Prelude.rnf subscriberStatus
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf sources
      `Prelude.seq` Prelude.rnf subscriberArn
      `Prelude.seq` Prelude.rnf subscriberId
      `Prelude.seq` Prelude.rnf subscriberIdentity
      `Prelude.seq` Prelude.rnf subscriberName
