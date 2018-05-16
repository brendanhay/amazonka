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
-- Module      : Network.AWS.CloudWatchLogs.DeleteLogGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified log group and permanently deletes all the archived log events associated with the log group.
--
--
module Network.AWS.CloudWatchLogs.DeleteLogGroup
    (
    -- * Creating a Request
      deleteLogGroup
    , DeleteLogGroup
    -- * Request Lenses
    , dlgLogGroupName

    -- * Destructuring the Response
    , deleteLogGroupResponse
    , DeleteLogGroupResponse
    ) where

import Network.AWS.CloudWatchLogs.Types
import Network.AWS.CloudWatchLogs.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLogGroup' smart constructor.
newtype DeleteLogGroup = DeleteLogGroup'
  { _dlgLogGroupName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLogGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlgLogGroupName' - The name of the log group.
deleteLogGroup
    :: Text -- ^ 'dlgLogGroupName'
    -> DeleteLogGroup
deleteLogGroup pLogGroupName_ =
  DeleteLogGroup' {_dlgLogGroupName = pLogGroupName_}


-- | The name of the log group.
dlgLogGroupName :: Lens' DeleteLogGroup Text
dlgLogGroupName = lens _dlgLogGroupName (\ s a -> s{_dlgLogGroupName = a})

instance AWSRequest DeleteLogGroup where
        type Rs DeleteLogGroup = DeleteLogGroupResponse
        request = postJSON cloudWatchLogs
        response = receiveNull DeleteLogGroupResponse'

instance Hashable DeleteLogGroup where

instance NFData DeleteLogGroup where

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
          = object
              (catMaybes
                 [Just ("logGroupName" .= _dlgLogGroupName)])

instance ToPath DeleteLogGroup where
        toPath = const "/"

instance ToQuery DeleteLogGroup where
        toQuery = const mempty

-- | /See:/ 'deleteLogGroupResponse' smart constructor.
data DeleteLogGroupResponse =
  DeleteLogGroupResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLogGroupResponse' with the minimum fields required to make a request.
--
deleteLogGroupResponse
    :: DeleteLogGroupResponse
deleteLogGroupResponse = DeleteLogGroupResponse'


instance NFData DeleteLogGroupResponse where
