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
-- Module      : Network.AWS.SSM.CreateResourceDataSync
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a resource data sync configuration to a single bucket in Amazon S3. This is an asynchronous operation that returns immediately. After a successful initial sync is completed, the system continuously syncs data to the Amazon S3 bucket. To check the status of the sync, use the 'ListResourceDataSync' .
--
--
-- By default, data is not encrypted in Amazon S3. We strongly recommend that you enable encryption in Amazon S3 to ensure secure data storage. We also recommend that you secure access to the Amazon S3 bucket by creating a restrictive bucket policy. To view an example of a restrictive Amazon S3 bucket policy for Resource Data Sync, see <http://docs.aws.amazon.com/systems-manager/latest/userguide/sysman-inventory-configuring.html#sysman-inventory-datasync Configuring Resource Data Sync for Inventory> .
--
module Network.AWS.SSM.CreateResourceDataSync
    (
    -- * Creating a Request
      createResourceDataSync
    , CreateResourceDataSync
    -- * Request Lenses
    , crdsSyncName
    , crdsS3Destination

    -- * Destructuring the Response
    , createResourceDataSyncResponse
    , CreateResourceDataSyncResponse
    -- * Response Lenses
    , crdsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'createResourceDataSync' smart constructor.
data CreateResourceDataSync = CreateResourceDataSync'
  { _crdsSyncName      :: !Text
  , _crdsS3Destination :: !ResourceDataSyncS3Destination
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateResourceDataSync' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crdsSyncName' - A name for the configuration.
--
-- * 'crdsS3Destination' - Amazon S3 configuration details for the sync.
createResourceDataSync
    :: Text -- ^ 'crdsSyncName'
    -> ResourceDataSyncS3Destination -- ^ 'crdsS3Destination'
    -> CreateResourceDataSync
createResourceDataSync pSyncName_ pS3Destination_ =
  CreateResourceDataSync'
    {_crdsSyncName = pSyncName_, _crdsS3Destination = pS3Destination_}


-- | A name for the configuration.
crdsSyncName :: Lens' CreateResourceDataSync Text
crdsSyncName = lens _crdsSyncName (\ s a -> s{_crdsSyncName = a})

-- | Amazon S3 configuration details for the sync.
crdsS3Destination :: Lens' CreateResourceDataSync ResourceDataSyncS3Destination
crdsS3Destination = lens _crdsS3Destination (\ s a -> s{_crdsS3Destination = a})

instance AWSRequest CreateResourceDataSync where
        type Rs CreateResourceDataSync =
             CreateResourceDataSyncResponse
        request = postJSON ssm
        response
          = receiveEmpty
              (\ s h x ->
                 CreateResourceDataSyncResponse' <$>
                   (pure (fromEnum s)))

instance Hashable CreateResourceDataSync where

instance NFData CreateResourceDataSync where

instance ToHeaders CreateResourceDataSync where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.CreateResourceDataSync" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateResourceDataSync where
        toJSON CreateResourceDataSync'{..}
          = object
              (catMaybes
                 [Just ("SyncName" .= _crdsSyncName),
                  Just ("S3Destination" .= _crdsS3Destination)])

instance ToPath CreateResourceDataSync where
        toPath = const "/"

instance ToQuery CreateResourceDataSync where
        toQuery = const mempty

-- | /See:/ 'createResourceDataSyncResponse' smart constructor.
newtype CreateResourceDataSyncResponse = CreateResourceDataSyncResponse'
  { _crdsrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateResourceDataSyncResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crdsrsResponseStatus' - -- | The response status code.
createResourceDataSyncResponse
    :: Int -- ^ 'crdsrsResponseStatus'
    -> CreateResourceDataSyncResponse
createResourceDataSyncResponse pResponseStatus_ =
  CreateResourceDataSyncResponse' {_crdsrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
crdsrsResponseStatus :: Lens' CreateResourceDataSyncResponse Int
crdsrsResponseStatus = lens _crdsrsResponseStatus (\ s a -> s{_crdsrsResponseStatus = a})

instance NFData CreateResourceDataSyncResponse where
