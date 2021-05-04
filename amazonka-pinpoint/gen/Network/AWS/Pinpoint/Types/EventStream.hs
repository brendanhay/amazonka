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
-- Module      : Network.AWS.Pinpoint.Types.EventStream
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EventStream where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies settings for publishing event data to an Amazon Kinesis data
-- stream or an Amazon Kinesis Data Firehose delivery stream.
--
-- /See:/ 'newEventStream' smart constructor.
data EventStream = EventStream'
  { -- | The date, in ISO 8601 format, when the event stream was last modified.
    lastModifiedDate :: Prelude.Maybe Prelude.Text,
    -- | The IAM user who last modified the event stream.
    lastUpdatedBy :: Prelude.Maybe Prelude.Text,
    -- | (Deprecated) Your AWS account ID, which you assigned to an external ID
    -- key in an IAM trust policy. Amazon Pinpoint previously used this value
    -- to assume an IAM role when publishing event data, but we removed this
    -- requirement. We don\'t recommend use of external IDs for IAM roles that
    -- are assumed by Amazon Pinpoint.
    externalId :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the application to publish event data for.
    applicationId :: Prelude.Text,
    -- | The AWS Identity and Access Management (IAM) role that authorizes Amazon
    -- Pinpoint to publish event data to the stream in your AWS account.
    roleArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon Kinesis data stream or
    -- Amazon Kinesis Data Firehose delivery stream to publish event data to.
    --
    -- For a Kinesis data stream, the ARN format is:
    -- arn:aws:kinesis:region:account-id:stream\/stream_name
    --
    -- For a Kinesis Data Firehose delivery stream, the ARN format is:
    -- arn:aws:firehose:region:account-id:deliverystream\/stream_name
    destinationStreamArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EventStream' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'eventStream_lastModifiedDate' - The date, in ISO 8601 format, when the event stream was last modified.
--
-- 'lastUpdatedBy', 'eventStream_lastUpdatedBy' - The IAM user who last modified the event stream.
--
-- 'externalId', 'eventStream_externalId' - (Deprecated) Your AWS account ID, which you assigned to an external ID
-- key in an IAM trust policy. Amazon Pinpoint previously used this value
-- to assume an IAM role when publishing event data, but we removed this
-- requirement. We don\'t recommend use of external IDs for IAM roles that
-- are assumed by Amazon Pinpoint.
--
-- 'applicationId', 'eventStream_applicationId' - The unique identifier for the application to publish event data for.
--
-- 'roleArn', 'eventStream_roleArn' - The AWS Identity and Access Management (IAM) role that authorizes Amazon
-- Pinpoint to publish event data to the stream in your AWS account.
--
-- 'destinationStreamArn', 'eventStream_destinationStreamArn' - The Amazon Resource Name (ARN) of the Amazon Kinesis data stream or
-- Amazon Kinesis Data Firehose delivery stream to publish event data to.
--
-- For a Kinesis data stream, the ARN format is:
-- arn:aws:kinesis:region:account-id:stream\/stream_name
--
-- For a Kinesis Data Firehose delivery stream, the ARN format is:
-- arn:aws:firehose:region:account-id:deliverystream\/stream_name
newEventStream ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'destinationStreamArn'
  Prelude.Text ->
  EventStream
newEventStream
  pApplicationId_
  pRoleArn_
  pDestinationStreamArn_ =
    EventStream'
      { lastModifiedDate = Prelude.Nothing,
        lastUpdatedBy = Prelude.Nothing,
        externalId = Prelude.Nothing,
        applicationId = pApplicationId_,
        roleArn = pRoleArn_,
        destinationStreamArn = pDestinationStreamArn_
      }

-- | The date, in ISO 8601 format, when the event stream was last modified.
eventStream_lastModifiedDate :: Lens.Lens' EventStream (Prelude.Maybe Prelude.Text)
eventStream_lastModifiedDate = Lens.lens (\EventStream' {lastModifiedDate} -> lastModifiedDate) (\s@EventStream' {} a -> s {lastModifiedDate = a} :: EventStream)

-- | The IAM user who last modified the event stream.
eventStream_lastUpdatedBy :: Lens.Lens' EventStream (Prelude.Maybe Prelude.Text)
eventStream_lastUpdatedBy = Lens.lens (\EventStream' {lastUpdatedBy} -> lastUpdatedBy) (\s@EventStream' {} a -> s {lastUpdatedBy = a} :: EventStream)

-- | (Deprecated) Your AWS account ID, which you assigned to an external ID
-- key in an IAM trust policy. Amazon Pinpoint previously used this value
-- to assume an IAM role when publishing event data, but we removed this
-- requirement. We don\'t recommend use of external IDs for IAM roles that
-- are assumed by Amazon Pinpoint.
eventStream_externalId :: Lens.Lens' EventStream (Prelude.Maybe Prelude.Text)
eventStream_externalId = Lens.lens (\EventStream' {externalId} -> externalId) (\s@EventStream' {} a -> s {externalId = a} :: EventStream)

-- | The unique identifier for the application to publish event data for.
eventStream_applicationId :: Lens.Lens' EventStream Prelude.Text
eventStream_applicationId = Lens.lens (\EventStream' {applicationId} -> applicationId) (\s@EventStream' {} a -> s {applicationId = a} :: EventStream)

-- | The AWS Identity and Access Management (IAM) role that authorizes Amazon
-- Pinpoint to publish event data to the stream in your AWS account.
eventStream_roleArn :: Lens.Lens' EventStream Prelude.Text
eventStream_roleArn = Lens.lens (\EventStream' {roleArn} -> roleArn) (\s@EventStream' {} a -> s {roleArn = a} :: EventStream)

-- | The Amazon Resource Name (ARN) of the Amazon Kinesis data stream or
-- Amazon Kinesis Data Firehose delivery stream to publish event data to.
--
-- For a Kinesis data stream, the ARN format is:
-- arn:aws:kinesis:region:account-id:stream\/stream_name
--
-- For a Kinesis Data Firehose delivery stream, the ARN format is:
-- arn:aws:firehose:region:account-id:deliverystream\/stream_name
eventStream_destinationStreamArn :: Lens.Lens' EventStream Prelude.Text
eventStream_destinationStreamArn = Lens.lens (\EventStream' {destinationStreamArn} -> destinationStreamArn) (\s@EventStream' {} a -> s {destinationStreamArn = a} :: EventStream)

instance Prelude.FromJSON EventStream where
  parseJSON =
    Prelude.withObject
      "EventStream"
      ( \x ->
          EventStream'
            Prelude.<$> (x Prelude..:? "LastModifiedDate")
            Prelude.<*> (x Prelude..:? "LastUpdatedBy")
            Prelude.<*> (x Prelude..:? "ExternalId")
            Prelude.<*> (x Prelude..: "ApplicationId")
            Prelude.<*> (x Prelude..: "RoleArn")
            Prelude.<*> (x Prelude..: "DestinationStreamArn")
      )

instance Prelude.Hashable EventStream

instance Prelude.NFData EventStream
