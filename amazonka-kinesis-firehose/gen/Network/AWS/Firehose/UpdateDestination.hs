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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified destination of the specified delivery stream. Note: Switching between Elasticsearch and other services is not supported. For Elasticsearch destination, you can only update an existing Elasticsearch destination with this operation.
--
-- This operation can be used to change the destination type (for example, to replace the Amazon S3 destination with Amazon Redshift) or change the parameters associated with a given destination (for example, to change the bucket name of the Amazon S3 destination). The update may not occur immediately. The target delivery stream remains active while the configurations are updated, so data writes to the delivery stream can continue during this process. The updated configurations are normally effective within a few minutes.
--
-- If the destination type is the same, Firehose merges the configuration parameters specified in the < UpdateDestination> request with the destination configuration that already exists on the delivery stream. If any of the parameters are not specified in the update request, then the existing configuration parameters are retained. For example, in the Amazon S3 destination, if < EncryptionConfiguration> is not specified then the existing < EncryptionConfiguration> is maintained on the destination.
--
-- If the destination type is not the same, for example, changing the destination from Amazon S3 to Amazon Redshift, Firehose does not merge any parameters. In this case, all parameters must be specified.
--
-- Firehose uses the __CurrentDeliveryStreamVersionId__ to avoid race conditions and conflicting merges. This is a required field in every request and the service only updates the configuration if the existing configuration matches the __VersionId__. After the update is applied successfully, the __VersionId__ is updated, which can be retrieved with the < DescribeDeliveryStream> operation. The new __VersionId__ should be uses to set __CurrentDeliveryStreamVersionId__ in the next < UpdateDestination> operation.
module Network.AWS.Firehose.UpdateDestination
    (
    -- * Creating a Request
      updateDestination
    , UpdateDestination
    -- * Request Lenses
    , udS3DestinationUpdate
    , udRedshiftDestinationUpdate
    , udElasticsearchDestinationUpdate
    , udDeliveryStreamName
    , udCurrentDeliveryStreamVersionId
    , udDestinationId

    -- * Destructuring the Response
    , updateDestinationResponse
    , UpdateDestinationResponse
    -- * Response Lenses
    , udrsResponseStatus
    ) where

import           Network.AWS.Firehose.Types
import           Network.AWS.Firehose.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Contains the parameters for < UpdateDestination>.
--
-- /See:/ 'updateDestination' smart constructor.
data UpdateDestination = UpdateDestination'
    { _udS3DestinationUpdate            :: !(Maybe S3DestinationUpdate)
    , _udRedshiftDestinationUpdate      :: !(Maybe RedshiftDestinationUpdate)
    , _udElasticsearchDestinationUpdate :: !(Maybe ElasticsearchDestinationUpdate)
    , _udDeliveryStreamName             :: !Text
    , _udCurrentDeliveryStreamVersionId :: !Text
    , _udDestinationId                  :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateDestination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udS3DestinationUpdate'
--
-- * 'udRedshiftDestinationUpdate'
--
-- * 'udElasticsearchDestinationUpdate'
--
-- * 'udDeliveryStreamName'
--
-- * 'udCurrentDeliveryStreamVersionId'
--
-- * 'udDestinationId'
updateDestination
    :: Text -- ^ 'udDeliveryStreamName'
    -> Text -- ^ 'udCurrentDeliveryStreamVersionId'
    -> Text -- ^ 'udDestinationId'
    -> UpdateDestination
updateDestination pDeliveryStreamName_ pCurrentDeliveryStreamVersionId_ pDestinationId_ =
    UpdateDestination'
    { _udS3DestinationUpdate = Nothing
    , _udRedshiftDestinationUpdate = Nothing
    , _udElasticsearchDestinationUpdate = Nothing
    , _udDeliveryStreamName = pDeliveryStreamName_
    , _udCurrentDeliveryStreamVersionId = pCurrentDeliveryStreamVersionId_
    , _udDestinationId = pDestinationId_
    }

-- | Undocumented member.
udS3DestinationUpdate :: Lens' UpdateDestination (Maybe S3DestinationUpdate)
udS3DestinationUpdate = lens _udS3DestinationUpdate (\ s a -> s{_udS3DestinationUpdate = a});

-- | Undocumented member.
udRedshiftDestinationUpdate :: Lens' UpdateDestination (Maybe RedshiftDestinationUpdate)
udRedshiftDestinationUpdate = lens _udRedshiftDestinationUpdate (\ s a -> s{_udRedshiftDestinationUpdate = a});

-- | Describes an update for a destination in Amazon ES.
udElasticsearchDestinationUpdate :: Lens' UpdateDestination (Maybe ElasticsearchDestinationUpdate)
udElasticsearchDestinationUpdate = lens _udElasticsearchDestinationUpdate (\ s a -> s{_udElasticsearchDestinationUpdate = a});

-- | The name of the delivery stream.
udDeliveryStreamName :: Lens' UpdateDestination Text
udDeliveryStreamName = lens _udDeliveryStreamName (\ s a -> s{_udDeliveryStreamName = a});

-- | Obtain this value from the __VersionId__ result of the < DeliveryStreamDescription> operation. This value is required, and helps the service to perform conditional operations. For example, if there is a interleaving update and this value is null, then the update destination fails. After the update is successful, the __VersionId__ value is updated. The service then performs a merge of the old configuration with the new configuration.
udCurrentDeliveryStreamVersionId :: Lens' UpdateDestination Text
udCurrentDeliveryStreamVersionId = lens _udCurrentDeliveryStreamVersionId (\ s a -> s{_udCurrentDeliveryStreamVersionId = a});

-- | The ID of the destination.
udDestinationId :: Lens' UpdateDestination Text
udDestinationId = lens _udDestinationId (\ s a -> s{_udDestinationId = a});

instance AWSRequest UpdateDestination where
        type Rs UpdateDestination = UpdateDestinationResponse
        request = postJSON firehose
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateDestinationResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateDestination

instance NFData UpdateDestination

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
                 [("S3DestinationUpdate" .=) <$>
                    _udS3DestinationUpdate,
                  ("RedshiftDestinationUpdate" .=) <$>
                    _udRedshiftDestinationUpdate,
                  ("ElasticsearchDestinationUpdate" .=) <$>
                    _udElasticsearchDestinationUpdate,
                  Just ("DeliveryStreamName" .= _udDeliveryStreamName),
                  Just
                    ("CurrentDeliveryStreamVersionId" .=
                       _udCurrentDeliveryStreamVersionId),
                  Just ("DestinationId" .= _udDestinationId)])

instance ToPath UpdateDestination where
        toPath = const "/"

instance ToQuery UpdateDestination where
        toQuery = const mempty

-- | Contains the output of < UpdateDestination>.
--
-- /See:/ 'updateDestinationResponse' smart constructor.
newtype UpdateDestinationResponse = UpdateDestinationResponse'
    { _udrsResponseStatus :: Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'UpdateDestinationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udrsResponseStatus'
updateDestinationResponse
    :: Int -- ^ 'udrsResponseStatus'
    -> UpdateDestinationResponse
updateDestinationResponse pResponseStatus_ =
    UpdateDestinationResponse'
    { _udrsResponseStatus = pResponseStatus_
    }

-- | The response status code.
udrsResponseStatus :: Lens' UpdateDestinationResponse Int
udrsResponseStatus = lens _udrsResponseStatus (\ s a -> s{_udrsResponseStatus = a});

instance NFData UpdateDestinationResponse
