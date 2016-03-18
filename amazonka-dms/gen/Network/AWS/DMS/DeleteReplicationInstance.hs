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
-- Module      : Network.AWS.DMS.DeleteReplicationInstance
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified replication instance.
--
-- You must delete any migration tasks that are associated with the
-- replication instance before you can delete it.
--
--
-- /See:/ <http://docs.aws.amazon.com/apigateway/api-reference/resource/DeleteReplicationInstance.html AWS API Reference> for DeleteReplicationInstance.
module Network.AWS.DMS.DeleteReplicationInstance
    (
    -- * Creating a Request
      deleteReplicationInstance
    , DeleteReplicationInstance
    -- * Request Lenses
    , driReplicationInstanceARN

    -- * Destructuring the Response
    , deleteReplicationInstanceResponse
    , DeleteReplicationInstanceResponse
    -- * Response Lenses
    , drirsReplicationInstance
    , drirsResponseStatus
    ) where

import           Network.AWS.DMS.Types
import           Network.AWS.DMS.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteReplicationInstance' smart constructor.
newtype DeleteReplicationInstance = DeleteReplicationInstance'
    { _driReplicationInstanceARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteReplicationInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'driReplicationInstanceARN'
deleteReplicationInstance
    :: Text -- ^ 'driReplicationInstanceARN'
    -> DeleteReplicationInstance
deleteReplicationInstance pReplicationInstanceARN_ =
    DeleteReplicationInstance'
    { _driReplicationInstanceARN = pReplicationInstanceARN_
    }

-- | The Amazon Resource Name (ARN) of the replication instance to be
-- deleted.
driReplicationInstanceARN :: Lens' DeleteReplicationInstance Text
driReplicationInstanceARN = lens _driReplicationInstanceARN (\ s a -> s{_driReplicationInstanceARN = a});

instance AWSRequest DeleteReplicationInstance where
        type Rs DeleteReplicationInstance =
             DeleteReplicationInstanceResponse
        request = postJSON dMS
        response
          = receiveJSON
              (\ s h x ->
                 DeleteReplicationInstanceResponse' <$>
                   (x .?> "ReplicationInstance") <*>
                     (pure (fromEnum s)))

instance ToHeaders DeleteReplicationInstance where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonDMSv20160101.DeleteReplicationInstance" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteReplicationInstance where
        toJSON DeleteReplicationInstance'{..}
          = object
              (catMaybes
                 [Just
                    ("ReplicationInstanceArn" .=
                       _driReplicationInstanceARN)])

instance ToPath DeleteReplicationInstance where
        toPath = const "/"

instance ToQuery DeleteReplicationInstance where
        toQuery = const mempty

-- | /See:/ 'deleteReplicationInstanceResponse' smart constructor.
data DeleteReplicationInstanceResponse = DeleteReplicationInstanceResponse'
    { _drirsReplicationInstance :: !(Maybe ReplicationInstance)
    , _drirsResponseStatus      :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteReplicationInstanceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drirsReplicationInstance'
--
-- * 'drirsResponseStatus'
deleteReplicationInstanceResponse
    :: Int -- ^ 'drirsResponseStatus'
    -> DeleteReplicationInstanceResponse
deleteReplicationInstanceResponse pResponseStatus_ =
    DeleteReplicationInstanceResponse'
    { _drirsReplicationInstance = Nothing
    , _drirsResponseStatus = pResponseStatus_
    }

-- | The replication instance that was deleted.
drirsReplicationInstance :: Lens' DeleteReplicationInstanceResponse (Maybe ReplicationInstance)
drirsReplicationInstance = lens _drirsReplicationInstance (\ s a -> s{_drirsReplicationInstance = a});

-- | The response status code.
drirsResponseStatus :: Lens' DeleteReplicationInstanceResponse Int
drirsResponseStatus = lens _drirsResponseStatus (\ s a -> s{_drirsResponseStatus = a});
