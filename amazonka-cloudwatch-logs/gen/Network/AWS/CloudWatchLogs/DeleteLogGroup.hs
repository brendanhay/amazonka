{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.CloudWatchLogs.DeleteLogGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the log group with the specified name and permanently deletes
-- all the archived log events associated with it.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DeleteLogGroup.html>
module Network.AWS.CloudWatchLogs.DeleteLogGroup
    (
    -- * Request
      DeleteLogGroup
    -- ** Request constructor
    , deleteLogGroup
    -- ** Request lenses
    , dlgLogGroupName

    -- * Response
    , DeleteLogGroupResponse
    -- ** Response constructor
    , deleteLogGroupResponse
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteLogGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlgLogGroupName'
newtype DeleteLogGroup = DeleteLogGroup'
    { _dlgLogGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLogGroup' smart constructor.
deleteLogGroup :: Text -> DeleteLogGroup
deleteLogGroup pLogGroupName =
    DeleteLogGroup'
    { _dlgLogGroupName = pLogGroupName
    }

-- | The name of the log group to delete.
dlgLogGroupName :: Lens' DeleteLogGroup Text
dlgLogGroupName = lens _dlgLogGroupName (\ s a -> s{_dlgLogGroupName = a});

instance AWSRequest DeleteLogGroup where
        type Sv DeleteLogGroup = CloudWatchLogs
        type Rs DeleteLogGroup = DeleteLogGroupResponse
        request = postJSON
        response = receiveNull DeleteLogGroupResponse'

instance ToHeaders DeleteLogGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.DeleteLogGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteLogGroup where
        toJSON DeleteLogGroup'{..}
          = object ["logGroupName" .= _dlgLogGroupName]

instance ToPath DeleteLogGroup where
        toPath = const "/"

instance ToQuery DeleteLogGroup where
        toQuery = const mempty

-- | /See:/ 'deleteLogGroupResponse' smart constructor.
data DeleteLogGroupResponse =
    DeleteLogGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLogGroupResponse' smart constructor.
deleteLogGroupResponse :: DeleteLogGroupResponse
deleteLogGroupResponse = DeleteLogGroupResponse'
