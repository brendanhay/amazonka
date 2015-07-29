{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DeleteDestination
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the destination with the specified name and eventually disables
-- all the subscription filters that publish to it. This will not delete
-- the physical resource encapsulated by the destination.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DeleteDestination.html>
module Network.AWS.CloudWatchLogs.DeleteDestination
    (
    -- * Request
      DeleteDestination
    -- ** Request constructor
    , deleteDestination
    -- ** Request lenses
    , ddDestinationName

    -- * Response
    , DeleteDestinationResponse
    -- ** Response constructor
    , deleteDestinationResponse
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteDestination' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddDestinationName'
newtype DeleteDestination = DeleteDestination'
    { _ddDestinationName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDestination' smart constructor.
deleteDestination :: Text -> DeleteDestination
deleteDestination pDestinationName_ =
    DeleteDestination'
    { _ddDestinationName = pDestinationName_
    }

-- | The name of destination to delete.
ddDestinationName :: Lens' DeleteDestination Text
ddDestinationName = lens _ddDestinationName (\ s a -> s{_ddDestinationName = a});

instance AWSRequest DeleteDestination where
        type Sv DeleteDestination = CloudWatchLogs
        type Rs DeleteDestination = DeleteDestinationResponse
        request = postJSON
        response = receiveNull DeleteDestinationResponse'

instance ToHeaders DeleteDestination where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DeleteDestination" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteDestination where
        toJSON DeleteDestination'{..}
          = object ["destinationName" .= _ddDestinationName]

instance ToPath DeleteDestination where
        toPath = const "/"

instance ToQuery DeleteDestination where
        toQuery = const mempty

-- | /See:/ 'deleteDestinationResponse' smart constructor.
data DeleteDestinationResponse =
    DeleteDestinationResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteDestinationResponse' smart constructor.
deleteDestinationResponse :: DeleteDestinationResponse
deleteDestinationResponse = DeleteDestinationResponse'
