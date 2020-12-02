{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.DeliveryStreamDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.DeliveryStreamDescription where

import Network.AWS.Firehose.Types.DeliveryStreamEncryptionConfiguration
import Network.AWS.Firehose.Types.DeliveryStreamStatus
import Network.AWS.Firehose.Types.DeliveryStreamType
import Network.AWS.Firehose.Types.DestinationDescription
import Network.AWS.Firehose.Types.FailureDescription
import Network.AWS.Firehose.Types.SourceDescription
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a delivery stream.
--
--
--
-- /See:/ 'deliveryStreamDescription' smart constructor.
data DeliveryStreamDescription = DeliveryStreamDescription'
  { _dsdFailureDescription ::
      !(Maybe FailureDescription),
    _dsdDeliveryStreamEncryptionConfiguration ::
      !( Maybe
           DeliveryStreamEncryptionConfiguration
       ),
    _dsdCreateTimestamp :: !(Maybe POSIX),
    _dsdSource ::
      !(Maybe SourceDescription),
    _dsdLastUpdateTimestamp ::
      !(Maybe POSIX),
    _dsdDeliveryStreamName :: !Text,
    _dsdDeliveryStreamARN :: !Text,
    _dsdDeliveryStreamStatus ::
      !DeliveryStreamStatus,
    _dsdDeliveryStreamType ::
      !DeliveryStreamType,
    _dsdVersionId :: !Text,
    _dsdDestinations ::
      ![DestinationDescription],
    _dsdHasMoreDestinations :: !Bool
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeliveryStreamDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsdFailureDescription' - Provides details in case one of the following operations fails due to an error related to KMS: 'CreateDeliveryStream' , 'DeleteDeliveryStream' , 'StartDeliveryStreamEncryption' , 'StopDeliveryStreamEncryption' .
--
-- * 'dsdDeliveryStreamEncryptionConfiguration' - Indicates the server-side encryption (SSE) status for the delivery stream.
--
-- * 'dsdCreateTimestamp' - The date and time that the delivery stream was created.
--
-- * 'dsdSource' - If the @DeliveryStreamType@ parameter is @KinesisStreamAsSource@ , a 'SourceDescription' object describing the source Kinesis data stream.
--
-- * 'dsdLastUpdateTimestamp' - The date and time that the delivery stream was last updated.
--
-- * 'dsdDeliveryStreamName' - The name of the delivery stream.
--
-- * 'dsdDeliveryStreamARN' - The Amazon Resource Name (ARN) of the delivery stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- * 'dsdDeliveryStreamStatus' - The status of the delivery stream. If the status of a delivery stream is @CREATING_FAILED@ , this status doesn't change, and you can't invoke @CreateDeliveryStream@ again on it. However, you can invoke the 'DeleteDeliveryStream' operation to delete it.
--
-- * 'dsdDeliveryStreamType' - The delivery stream type. This can be one of the following values:     * @DirectPut@ : Provider applications access the delivery stream directly.     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
--
-- * 'dsdVersionId' - Each time the destination is updated for a delivery stream, the version ID is changed, and the current version ID is required when updating the destination. This is so that the service knows it is applying the changes to the correct version of the delivery stream.
--
-- * 'dsdDestinations' - The destinations.
--
-- * 'dsdHasMoreDestinations' - Indicates whether there are more destinations available to list.
deliveryStreamDescription ::
  -- | 'dsdDeliveryStreamName'
  Text ->
  -- | 'dsdDeliveryStreamARN'
  Text ->
  -- | 'dsdDeliveryStreamStatus'
  DeliveryStreamStatus ->
  -- | 'dsdDeliveryStreamType'
  DeliveryStreamType ->
  -- | 'dsdVersionId'
  Text ->
  -- | 'dsdHasMoreDestinations'
  Bool ->
  DeliveryStreamDescription
deliveryStreamDescription
  pDeliveryStreamName_
  pDeliveryStreamARN_
  pDeliveryStreamStatus_
  pDeliveryStreamType_
  pVersionId_
  pHasMoreDestinations_ =
    DeliveryStreamDescription'
      { _dsdFailureDescription = Nothing,
        _dsdDeliveryStreamEncryptionConfiguration = Nothing,
        _dsdCreateTimestamp = Nothing,
        _dsdSource = Nothing,
        _dsdLastUpdateTimestamp = Nothing,
        _dsdDeliveryStreamName = pDeliveryStreamName_,
        _dsdDeliveryStreamARN = pDeliveryStreamARN_,
        _dsdDeliveryStreamStatus = pDeliveryStreamStatus_,
        _dsdDeliveryStreamType = pDeliveryStreamType_,
        _dsdVersionId = pVersionId_,
        _dsdDestinations = mempty,
        _dsdHasMoreDestinations = pHasMoreDestinations_
      }

-- | Provides details in case one of the following operations fails due to an error related to KMS: 'CreateDeliveryStream' , 'DeleteDeliveryStream' , 'StartDeliveryStreamEncryption' , 'StopDeliveryStreamEncryption' .
dsdFailureDescription :: Lens' DeliveryStreamDescription (Maybe FailureDescription)
dsdFailureDescription = lens _dsdFailureDescription (\s a -> s {_dsdFailureDescription = a})

-- | Indicates the server-side encryption (SSE) status for the delivery stream.
dsdDeliveryStreamEncryptionConfiguration :: Lens' DeliveryStreamDescription (Maybe DeliveryStreamEncryptionConfiguration)
dsdDeliveryStreamEncryptionConfiguration = lens _dsdDeliveryStreamEncryptionConfiguration (\s a -> s {_dsdDeliveryStreamEncryptionConfiguration = a})

-- | The date and time that the delivery stream was created.
dsdCreateTimestamp :: Lens' DeliveryStreamDescription (Maybe UTCTime)
dsdCreateTimestamp = lens _dsdCreateTimestamp (\s a -> s {_dsdCreateTimestamp = a}) . mapping _Time

-- | If the @DeliveryStreamType@ parameter is @KinesisStreamAsSource@ , a 'SourceDescription' object describing the source Kinesis data stream.
dsdSource :: Lens' DeliveryStreamDescription (Maybe SourceDescription)
dsdSource = lens _dsdSource (\s a -> s {_dsdSource = a})

-- | The date and time that the delivery stream was last updated.
dsdLastUpdateTimestamp :: Lens' DeliveryStreamDescription (Maybe UTCTime)
dsdLastUpdateTimestamp = lens _dsdLastUpdateTimestamp (\s a -> s {_dsdLastUpdateTimestamp = a}) . mapping _Time

-- | The name of the delivery stream.
dsdDeliveryStreamName :: Lens' DeliveryStreamDescription Text
dsdDeliveryStreamName = lens _dsdDeliveryStreamName (\s a -> s {_dsdDeliveryStreamName = a})

-- | The Amazon Resource Name (ARN) of the delivery stream. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
dsdDeliveryStreamARN :: Lens' DeliveryStreamDescription Text
dsdDeliveryStreamARN = lens _dsdDeliveryStreamARN (\s a -> s {_dsdDeliveryStreamARN = a})

-- | The status of the delivery stream. If the status of a delivery stream is @CREATING_FAILED@ , this status doesn't change, and you can't invoke @CreateDeliveryStream@ again on it. However, you can invoke the 'DeleteDeliveryStream' operation to delete it.
dsdDeliveryStreamStatus :: Lens' DeliveryStreamDescription DeliveryStreamStatus
dsdDeliveryStreamStatus = lens _dsdDeliveryStreamStatus (\s a -> s {_dsdDeliveryStreamStatus = a})

-- | The delivery stream type. This can be one of the following values:     * @DirectPut@ : Provider applications access the delivery stream directly.     * @KinesisStreamAsSource@ : The delivery stream uses a Kinesis data stream as a source.
dsdDeliveryStreamType :: Lens' DeliveryStreamDescription DeliveryStreamType
dsdDeliveryStreamType = lens _dsdDeliveryStreamType (\s a -> s {_dsdDeliveryStreamType = a})

-- | Each time the destination is updated for a delivery stream, the version ID is changed, and the current version ID is required when updating the destination. This is so that the service knows it is applying the changes to the correct version of the delivery stream.
dsdVersionId :: Lens' DeliveryStreamDescription Text
dsdVersionId = lens _dsdVersionId (\s a -> s {_dsdVersionId = a})

-- | The destinations.
dsdDestinations :: Lens' DeliveryStreamDescription [DestinationDescription]
dsdDestinations = lens _dsdDestinations (\s a -> s {_dsdDestinations = a}) . _Coerce

-- | Indicates whether there are more destinations available to list.
dsdHasMoreDestinations :: Lens' DeliveryStreamDescription Bool
dsdHasMoreDestinations = lens _dsdHasMoreDestinations (\s a -> s {_dsdHasMoreDestinations = a})

instance FromJSON DeliveryStreamDescription where
  parseJSON =
    withObject
      "DeliveryStreamDescription"
      ( \x ->
          DeliveryStreamDescription'
            <$> (x .:? "FailureDescription")
            <*> (x .:? "DeliveryStreamEncryptionConfiguration")
            <*> (x .:? "CreateTimestamp")
            <*> (x .:? "Source")
            <*> (x .:? "LastUpdateTimestamp")
            <*> (x .: "DeliveryStreamName")
            <*> (x .: "DeliveryStreamARN")
            <*> (x .: "DeliveryStreamStatus")
            <*> (x .: "DeliveryStreamType")
            <*> (x .: "VersionId")
            <*> (x .:? "Destinations" .!= mempty)
            <*> (x .: "HasMoreDestinations")
      )

instance Hashable DeliveryStreamDescription

instance NFData DeliveryStreamDescription
