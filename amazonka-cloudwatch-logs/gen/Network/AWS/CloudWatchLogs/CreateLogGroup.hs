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
-- Module      : Network.AWS.CloudWatchLogs.CreateLogGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new log group with the specified name. The name of the log
-- group must be unique within a region for an AWS account. You can create
-- up to 500 log groups per account.
--
-- You must use the following guidelines when naming a log group:
--
-- -   Log group names can be between 1 and 512 characters long.
-- -   Allowed characters are a-z, A-Z, 0-9, \'_\' (underscore), \'-\'
--     (hyphen), \'\/\' (forward slash), and \'.\' (period).
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_CreateLogGroup.html AWS API Reference> for CreateLogGroup.
module Network.AWS.CloudWatchLogs.CreateLogGroup
    (
    -- * Creating a Request
      CreateLogGroup
    , createLogGroup
    -- * Request Lenses
    , clgLogGroupName

    -- * Destructuring the Response
    , CreateLogGroupResponse
    , createLogGroupResponse
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.CloudWatchLogs.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createLogGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'clgLogGroupName'
newtype CreateLogGroup = CreateLogGroup'
    { _clgLogGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLogGroup' smart constructor.
createLogGroup :: Text -> CreateLogGroup
createLogGroup pLogGroupName_ =
    CreateLogGroup'
    { _clgLogGroupName = pLogGroupName_
    }

-- | The name of the log group to create.
clgLogGroupName :: Lens' CreateLogGroup Text
clgLogGroupName = lens _clgLogGroupName (\ s a -> s{_clgLogGroupName = a});

instance AWSRequest CreateLogGroup where
        type Sv CreateLogGroup = CloudWatchLogs
        type Rs CreateLogGroup = CreateLogGroupResponse
        request = postJSON
        response = receiveNull CreateLogGroupResponse'

instance ToHeaders CreateLogGroup where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("Logs_20140328.CreateLogGroup" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreateLogGroup where
        toJSON CreateLogGroup'{..}
          = object ["logGroupName" .= _clgLogGroupName]

instance ToPath CreateLogGroup where
        toPath = const "/"

instance ToQuery CreateLogGroup where
        toQuery = const mempty

-- | /See:/ 'createLogGroupResponse' smart constructor.
data CreateLogGroupResponse =
    CreateLogGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateLogGroupResponse' smart constructor.
createLogGroupResponse :: CreateLogGroupResponse
createLogGroupResponse = CreateLogGroupResponse'
