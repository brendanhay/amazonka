{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.UpdateDestination
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified destination of the specified delivery stream.
--
--
-- Use this operation to change the destination type (for example, to replace the Amazon S3 destination with Amazon Redshift) or change the parameters associated with a destination (for example, to change the bucket name of the Amazon S3 destination). The update might not occur immediately. The target delivery stream remains active while the configurations are updated, so data writes to the delivery stream can continue during this process. The updated configurations are usually effective within a few minutes.
--
-- Switching between Amazon ES and other services is not supported. For an Amazon ES destination, you can only update to another Amazon ES destination.
--
-- If the destination type is the same, Kinesis Data Firehose merges the configuration parameters specified with the destination configuration that already exists on the delivery stream. If any of the parameters are not specified in the call, the existing values are retained. For example, in the Amazon S3 destination, if 'EncryptionConfiguration' is not specified, then the existing @EncryptionConfiguration@ is maintained on the destination.
--
-- If the destination type is not the same, for example, changing the destination from Amazon S3 to Amazon Redshift, Kinesis Data Firehose does not merge any parameters. In this case, all parameters must be specified.
--
-- Kinesis Data Firehose uses __CurrentDeliveryStreamVersionId__ to avoid race conditions and conflicting merges. This is a required field, and the service updates the configuration only if the existing configuration has a version ID that matches. After the update is applied successfully, the version ID is updated, and can be retrieved using 'DescribeDeliveryStream' . Use the new version ID to set __CurrentDeliveryStreamVersionId__ in the next call.
--
module Network.AWS.Firehose.UpdateDestination
    (
    -- * Creating a Request
      updateDestination
    , UpdateDestination
    -- * Request Lenses
    , udSplunkDestinationUpdate
    , udS3DestinationUpdate
    , udRedshiftDestinationUpdate
    , udElasticsearchDestinationUpdate
    , udExtendedS3DestinationUpdate
    , udDeliveryStreamName
    , udCurrentDeliveryStreamVersionId
    , udDestinationId

    -- * Destructuring the Response
    , updateDestinationResponse
    , UpdateDestinationResponse
    -- * Response Lenses
    , udrsResponseStatus
    ) where

import Network.AWS.Firehose.Types
import Network.AWS.Firehose.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDestination' smart constructor.
data UpdateDestination = UpdateDestination'
  { _udSplunkDestinationUpdate        :: !(Maybe SplunkDestinationUpdate)
  , _udS3DestinationUpdate            :: !(Maybe S3DestinationUpdate)
  , _udRedshiftDestinationUpdate      :: !(Maybe RedshiftDestinationUpdate)
  , _udElasticsearchDestinationUpdate :: !(Maybe ElasticsearchDestinationUpdate)
  , _udExtendedS3DestinationUpdate    :: !(Maybe ExtendedS3DestinationUpdate)
  , _udDeliveryStreamName             :: !Text
  , _udCurrentDeliveryStreamVersionId :: !Text
  , _udDestinationId                  :: !Text
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udSplunkDestinationUpdate' - Describes an update for a destination in Splunk.
--
-- * 'udS3DestinationUpdate' - [Deprecated] Describes an update for a destination in Amazon S3.
--
-- * 'udRedshiftDestinationUpdate' - Describes an update for a destination in Amazon Redshift.
--
-- * 'udElasticsearchDestinationUpdate' - Describes an update for a destination in Amazon ES.
--
-- * 'udExtendedS3DestinationUpdate' - Describes an update for a destination in Amazon S3.
--
-- * 'udDeliveryStreamName' - The name of the delivery stream.
--
-- * 'udCurrentDeliveryStreamVersionId' - Obtain this value from the __VersionId__ result of 'DeliveryStreamDescription' . This value is required, and helps the service perform conditional operations. For example, if there is an interleaving update and this value is null, then the update destination fails. After the update is successful, the @VersionId@ value is updated. The service then performs a merge of the old configuration with the new configuration.
--
-- * 'udDestinationId' - The ID of the destination.
updateDestination
    :: Text -- ^ 'udDeliveryStreamName'
    -> Text -- ^ 'udCurrentDeliveryStreamVersionId'
    -> Text -- ^ 'udDestinationId'
    -> UpdateDestination
updateDestination pDeliveryStreamName_ pCurrentDeliveryStreamVersionId_ pDestinationId_ =
  UpdateDestination'
    { _udSplunkDestinationUpdate = Nothing
    , _udS3DestinationUpdate = Nothing
    , _udRedshiftDestinationUpdate = Nothing
    , _udElasticsearchDestinationUpdate = Nothing
    , _udExtendedS3DestinationUpdate = Nothing
    , _udDeliveryStreamName = pDeliveryStreamName_
    , _udCurrentDeliveryStreamVersionId = pCurrentDeliveryStreamVersionId_
    , _udDestinationId = pDestinationId_
    }


-- | Describes an update for a destination in Splunk.
udSplunkDestinationUpdate :: Lens' UpdateDestination (Maybe SplunkDestinationUpdate)
udSplunkDestinationUpdate = lens _udSplunkDestinationUpdate (\ s a -> s{_udSplunkDestinationUpdate = a})

-- | [Deprecated] Describes an update for a destination in Amazon S3.
udS3DestinationUpdate :: Lens' UpdateDestination (Maybe S3DestinationUpdate)
udS3DestinationUpdate = lens _udS3DestinationUpdate (\ s a -> s{_udS3DestinationUpdate = a})

-- | Describes an update for a destination in Amazon Redshift.
udRedshiftDestinationUpdate :: Lens' UpdateDestination (Maybe RedshiftDestinationUpdate)
udRedshiftDestinationUpdate = lens _udRedshiftDestinationUpdate (\ s a -> s{_udRedshiftDestinationUpdate = a})

-- | Describes an update for a destination in Amazon ES.
udElasticsearchDestinationUpdate :: Lens' UpdateDestination (Maybe ElasticsearchDestinationUpdate)
udElasticsearchDestinationUpdate = lens _udElasticsearchDestinationUpdate (\ s a -> s{_udElasticsearchDestinationUpdate = a})

-- | Describes an update for a destination in Amazon S3.
udExtendedS3DestinationUpdate :: Lens' UpdateDestination (Maybe ExtendedS3DestinationUpdate)
udExtendedS3DestinationUpdate = lens _udExtendedS3DestinationUpdate (\ s a -> s{_udExtendedS3DestinationUpdate = a})

-- | The name of the delivery stream.
udDeliveryStreamName :: Lens' UpdateDestination Text
udDeliveryStreamName = lens _udDeliveryStreamName (\ s a -> s{_udDeliveryStreamName = a})

-- | Obtain this value from the __VersionId__ result of 'DeliveryStreamDescription' . This value is required, and helps the service perform conditional operations. For example, if there is an interleaving update and this value is null, then the update destination fails. After the update is successful, the @VersionId@ value is updated. The service then performs a merge of the old configuration with the new configuration.
udCurrentDeliveryStreamVersionId :: Lens' UpdateDestination Text
udCurrentDeliveryStreamVersionId = lens _udCurrentDeliveryStreamVersionId (\ s a -> s{_udCurrentDeliveryStreamVersionId = a})

-- | The ID of the destination.
udDestinationId :: Lens' UpdateDestination Text
udDestinationId = lens _udDestinationId (\ s a -> s{_udDestinationId = a})

instance AWSRequest UpdateDestination where
        type Rs UpdateDestination = UpdateDestinationResponse
        request = postJSON firehose
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateDestinationResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateDestination where

instance NFData UpdateDestination where

instance ToHeaders UpdateDestination where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Firehose_20150804.UpdateDestination" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateDestination where
        toJSON UpdateDestination'{..}
          = object
              (catMaybes
                 [("SplunkDestinationUpdate" .=) <$>
                    _udSplunkDestinationUpdate,
                  ("S3DestinationUpdate" .=) <$>
                    _udS3DestinationUpdate,
                  ("RedshiftDestinationUpdate" .=) <$>
                    _udRedshiftDestinationUpdate,
                  ("ElasticsearchDestinationUpdate" .=) <$>
                    _udElasticsearchDestinationUpdate,
                  ("ExtendedS3DestinationUpdate" .=) <$>
                    _udExtendedS3DestinationUpdate,
                  Just ("DeliveryStreamName" .= _udDeliveryStreamName),
                  Just
                    ("CurrentDeliveryStreamVersionId" .=
                       _udCurrentDeliveryStreamVersionId),
                  Just ("DestinationId" .= _udDestinationId)])

instance ToPath UpdateDestination where
        toPath = const "/"

instance ToQuery UpdateDestination where
        toQuery = const mempty

-- | /See:/ 'updateDestinationResponse' smart constructor.
newtype UpdateDestinationResponse = UpdateDestinationResponse'
  { _udrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateDestinationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udrsResponseStatus' - -- | The response status code.
updateDestinationResponse
    :: Int -- ^ 'udrsResponseStatus'
    -> UpdateDestinationResponse
updateDestinationResponse pResponseStatus_ =
  UpdateDestinationResponse' {_udrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
udrsResponseStatus :: Lens' UpdateDestinationResponse Int
udrsResponseStatus = lens _udrsResponseStatus (\ s a -> s{_udrsResponseStatus = a})

instance NFData UpdateDestinationResponse where
