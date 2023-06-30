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
import Amazonka.SecurityLake.Types.EndpointProtocol
import Amazonka.SecurityLake.Types.SourceType
import Amazonka.SecurityLake.Types.SubscriptionStatus

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
    -- | The date and time when the subscription was created.
    createdAt :: Prelude.Maybe Data.ISO8601,
    -- | The external ID of the subscriber. The external ID lets the user that is
    -- assuming the role assert the circumstances in which they are operating.
    -- It also provides a way for the account owner to permit the role to be
    -- assumed only under specific circumstances.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) specifying the role of the subscriber.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the Amazon S3 bucket.
    s3BucketArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN for the Amazon Simple Notification Service.
    snsArn :: Prelude.Maybe Prelude.Text,
    -- | The subscriber descriptions for a subscriber account. The description
    -- for a subscriber includes @subscriberName@, @accountID@, @externalID@,
    -- and @subscriptionId@.
    subscriberDescription :: Prelude.Maybe Prelude.Text,
    -- | The name of your Amazon Security Lake subscriber account.
    subscriberName :: Prelude.Maybe Prelude.Text,
    -- | The subscription endpoint to which exception messages are posted.
    subscriptionEndpoint :: Prelude.Maybe Prelude.Text,
    -- | The subscription protocol to which exception messages are posted.
    subscriptionProtocol :: Prelude.Maybe EndpointProtocol,
    -- | The subscription status of the Amazon Security Lake subscriber account.
    subscriptionStatus :: Prelude.Maybe SubscriptionStatus,
    -- | The date and time when the subscription was created.
    updatedAt :: Prelude.Maybe Data.ISO8601,
    -- | The Amazon Web Services account ID you are using to create your Amazon
    -- Security Lake account.
    accountId :: Prelude.Text,
    -- | Amazon Security Lake supports log and event collection for natively
    -- supported Amazon Web Services. For more information, see the Amazon
    -- Security Lake User Guide.
    sourceTypes :: [SourceType],
    -- | The subscription ID of the Amazon Security Lake subscriber account.
    subscriptionId :: Prelude.Text
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
-- 'createdAt', 'subscriberResource_createdAt' - The date and time when the subscription was created.
--
-- 'externalId', 'subscriberResource_externalId' - The external ID of the subscriber. The external ID lets the user that is
-- assuming the role assert the circumstances in which they are operating.
-- It also provides a way for the account owner to permit the role to be
-- assumed only under specific circumstances.
--
-- 'roleArn', 'subscriberResource_roleArn' - The Amazon Resource Name (ARN) specifying the role of the subscriber.
--
-- 's3BucketArn', 'subscriberResource_s3BucketArn' - The ARN for the Amazon S3 bucket.
--
-- 'snsArn', 'subscriberResource_snsArn' - The ARN for the Amazon Simple Notification Service.
--
-- 'subscriberDescription', 'subscriberResource_subscriberDescription' - The subscriber descriptions for a subscriber account. The description
-- for a subscriber includes @subscriberName@, @accountID@, @externalID@,
-- and @subscriptionId@.
--
-- 'subscriberName', 'subscriberResource_subscriberName' - The name of your Amazon Security Lake subscriber account.
--
-- 'subscriptionEndpoint', 'subscriberResource_subscriptionEndpoint' - The subscription endpoint to which exception messages are posted.
--
-- 'subscriptionProtocol', 'subscriberResource_subscriptionProtocol' - The subscription protocol to which exception messages are posted.
--
-- 'subscriptionStatus', 'subscriberResource_subscriptionStatus' - The subscription status of the Amazon Security Lake subscriber account.
--
-- 'updatedAt', 'subscriberResource_updatedAt' - The date and time when the subscription was created.
--
-- 'accountId', 'subscriberResource_accountId' - The Amazon Web Services account ID you are using to create your Amazon
-- Security Lake account.
--
-- 'sourceTypes', 'subscriberResource_sourceTypes' - Amazon Security Lake supports log and event collection for natively
-- supported Amazon Web Services. For more information, see the Amazon
-- Security Lake User Guide.
--
-- 'subscriptionId', 'subscriberResource_subscriptionId' - The subscription ID of the Amazon Security Lake subscriber account.
newSubscriberResource ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'subscriptionId'
  Prelude.Text ->
  SubscriberResource
newSubscriberResource pAccountId_ pSubscriptionId_ =
  SubscriberResource'
    { accessTypes = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      externalId = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      s3BucketArn = Prelude.Nothing,
      snsArn = Prelude.Nothing,
      subscriberDescription = Prelude.Nothing,
      subscriberName = Prelude.Nothing,
      subscriptionEndpoint = Prelude.Nothing,
      subscriptionProtocol = Prelude.Nothing,
      subscriptionStatus = Prelude.Nothing,
      updatedAt = Prelude.Nothing,
      accountId = pAccountId_,
      sourceTypes = Prelude.mempty,
      subscriptionId = pSubscriptionId_
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

-- | The date and time when the subscription was created.
subscriberResource_createdAt :: Lens.Lens' SubscriberResource (Prelude.Maybe Prelude.UTCTime)
subscriberResource_createdAt = Lens.lens (\SubscriberResource' {createdAt} -> createdAt) (\s@SubscriberResource' {} a -> s {createdAt = a} :: SubscriberResource) Prelude.. Lens.mapping Data._Time

-- | The external ID of the subscriber. The external ID lets the user that is
-- assuming the role assert the circumstances in which they are operating.
-- It also provides a way for the account owner to permit the role to be
-- assumed only under specific circumstances.
subscriberResource_externalId :: Lens.Lens' SubscriberResource (Prelude.Maybe Prelude.Text)
subscriberResource_externalId = Lens.lens (\SubscriberResource' {externalId} -> externalId) (\s@SubscriberResource' {} a -> s {externalId = a} :: SubscriberResource)

-- | The Amazon Resource Name (ARN) specifying the role of the subscriber.
subscriberResource_roleArn :: Lens.Lens' SubscriberResource (Prelude.Maybe Prelude.Text)
subscriberResource_roleArn = Lens.lens (\SubscriberResource' {roleArn} -> roleArn) (\s@SubscriberResource' {} a -> s {roleArn = a} :: SubscriberResource)

-- | The ARN for the Amazon S3 bucket.
subscriberResource_s3BucketArn :: Lens.Lens' SubscriberResource (Prelude.Maybe Prelude.Text)
subscriberResource_s3BucketArn = Lens.lens (\SubscriberResource' {s3BucketArn} -> s3BucketArn) (\s@SubscriberResource' {} a -> s {s3BucketArn = a} :: SubscriberResource)

-- | The ARN for the Amazon Simple Notification Service.
subscriberResource_snsArn :: Lens.Lens' SubscriberResource (Prelude.Maybe Prelude.Text)
subscriberResource_snsArn = Lens.lens (\SubscriberResource' {snsArn} -> snsArn) (\s@SubscriberResource' {} a -> s {snsArn = a} :: SubscriberResource)

-- | The subscriber descriptions for a subscriber account. The description
-- for a subscriber includes @subscriberName@, @accountID@, @externalID@,
-- and @subscriptionId@.
subscriberResource_subscriberDescription :: Lens.Lens' SubscriberResource (Prelude.Maybe Prelude.Text)
subscriberResource_subscriberDescription = Lens.lens (\SubscriberResource' {subscriberDescription} -> subscriberDescription) (\s@SubscriberResource' {} a -> s {subscriberDescription = a} :: SubscriberResource)

-- | The name of your Amazon Security Lake subscriber account.
subscriberResource_subscriberName :: Lens.Lens' SubscriberResource (Prelude.Maybe Prelude.Text)
subscriberResource_subscriberName = Lens.lens (\SubscriberResource' {subscriberName} -> subscriberName) (\s@SubscriberResource' {} a -> s {subscriberName = a} :: SubscriberResource)

-- | The subscription endpoint to which exception messages are posted.
subscriberResource_subscriptionEndpoint :: Lens.Lens' SubscriberResource (Prelude.Maybe Prelude.Text)
subscriberResource_subscriptionEndpoint = Lens.lens (\SubscriberResource' {subscriptionEndpoint} -> subscriptionEndpoint) (\s@SubscriberResource' {} a -> s {subscriptionEndpoint = a} :: SubscriberResource)

-- | The subscription protocol to which exception messages are posted.
subscriberResource_subscriptionProtocol :: Lens.Lens' SubscriberResource (Prelude.Maybe EndpointProtocol)
subscriberResource_subscriptionProtocol = Lens.lens (\SubscriberResource' {subscriptionProtocol} -> subscriptionProtocol) (\s@SubscriberResource' {} a -> s {subscriptionProtocol = a} :: SubscriberResource)

-- | The subscription status of the Amazon Security Lake subscriber account.
subscriberResource_subscriptionStatus :: Lens.Lens' SubscriberResource (Prelude.Maybe SubscriptionStatus)
subscriberResource_subscriptionStatus = Lens.lens (\SubscriberResource' {subscriptionStatus} -> subscriptionStatus) (\s@SubscriberResource' {} a -> s {subscriptionStatus = a} :: SubscriberResource)

-- | The date and time when the subscription was created.
subscriberResource_updatedAt :: Lens.Lens' SubscriberResource (Prelude.Maybe Prelude.UTCTime)
subscriberResource_updatedAt = Lens.lens (\SubscriberResource' {updatedAt} -> updatedAt) (\s@SubscriberResource' {} a -> s {updatedAt = a} :: SubscriberResource) Prelude.. Lens.mapping Data._Time

-- | The Amazon Web Services account ID you are using to create your Amazon
-- Security Lake account.
subscriberResource_accountId :: Lens.Lens' SubscriberResource Prelude.Text
subscriberResource_accountId = Lens.lens (\SubscriberResource' {accountId} -> accountId) (\s@SubscriberResource' {} a -> s {accountId = a} :: SubscriberResource)

-- | Amazon Security Lake supports log and event collection for natively
-- supported Amazon Web Services. For more information, see the Amazon
-- Security Lake User Guide.
subscriberResource_sourceTypes :: Lens.Lens' SubscriberResource [SourceType]
subscriberResource_sourceTypes = Lens.lens (\SubscriberResource' {sourceTypes} -> sourceTypes) (\s@SubscriberResource' {} a -> s {sourceTypes = a} :: SubscriberResource) Prelude.. Lens.coerced

-- | The subscription ID of the Amazon Security Lake subscriber account.
subscriberResource_subscriptionId :: Lens.Lens' SubscriberResource Prelude.Text
subscriberResource_subscriptionId = Lens.lens (\SubscriberResource' {subscriptionId} -> subscriptionId) (\s@SubscriberResource' {} a -> s {subscriptionId = a} :: SubscriberResource)

instance Data.FromJSON SubscriberResource where
  parseJSON =
    Data.withObject
      "SubscriberResource"
      ( \x ->
          SubscriberResource'
            Prelude.<$> (x Data..:? "accessTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "createdAt")
            Prelude.<*> (x Data..:? "externalId")
            Prelude.<*> (x Data..:? "roleArn")
            Prelude.<*> (x Data..:? "s3BucketArn")
            Prelude.<*> (x Data..:? "snsArn")
            Prelude.<*> (x Data..:? "subscriberDescription")
            Prelude.<*> (x Data..:? "subscriberName")
            Prelude.<*> (x Data..:? "subscriptionEndpoint")
            Prelude.<*> (x Data..:? "subscriptionProtocol")
            Prelude.<*> (x Data..:? "subscriptionStatus")
            Prelude.<*> (x Data..:? "updatedAt")
            Prelude.<*> (x Data..: "accountId")
            Prelude.<*> (x Data..:? "sourceTypes" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "subscriptionId")
      )

instance Prelude.Hashable SubscriberResource where
  hashWithSalt _salt SubscriberResource' {..} =
    _salt
      `Prelude.hashWithSalt` accessTypes
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` externalId
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` s3BucketArn
      `Prelude.hashWithSalt` snsArn
      `Prelude.hashWithSalt` subscriberDescription
      `Prelude.hashWithSalt` subscriberName
      `Prelude.hashWithSalt` subscriptionEndpoint
      `Prelude.hashWithSalt` subscriptionProtocol
      `Prelude.hashWithSalt` subscriptionStatus
      `Prelude.hashWithSalt` updatedAt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` sourceTypes
      `Prelude.hashWithSalt` subscriptionId

instance Prelude.NFData SubscriberResource where
  rnf SubscriberResource' {..} =
    Prelude.rnf accessTypes
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf externalId
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf s3BucketArn
      `Prelude.seq` Prelude.rnf snsArn
      `Prelude.seq` Prelude.rnf subscriberDescription
      `Prelude.seq` Prelude.rnf subscriberName
      `Prelude.seq` Prelude.rnf subscriptionEndpoint
      `Prelude.seq` Prelude.rnf subscriptionProtocol
      `Prelude.seq` Prelude.rnf subscriptionStatus
      `Prelude.seq` Prelude.rnf updatedAt
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf sourceTypes
      `Prelude.seq` Prelude.rnf subscriptionId
