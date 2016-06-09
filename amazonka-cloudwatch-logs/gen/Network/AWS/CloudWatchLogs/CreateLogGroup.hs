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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new log group with the specified name. The name of the log group must be unique within a region for an AWS account. You can create up to 500 log groups per account.
--
-- You must use the following guidelines when naming a log group:
--
-- -   Log group names can be between 1 and 512 characters long.
-- -   Allowed characters are a-z, A-Z, 0-9, \'_\' (underscore), \'-\' (hyphen), \'\/\' (forward slash), and \'.\' (period).
module Network.AWS.CloudWatchLogs.CreateLogGroup
    (
    -- * Creating a Request
      createLogGroup
    , CreateLogGroup
    -- * Request Lenses
    , clgLogGroupName

    -- * Destructuring the Response
    , createLogGroupResponse
    , CreateLogGroupResponse
    ) where

import           Network.AWS.CloudWatchLogs.Types
import           Network.AWS.CloudWatchLogs.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createLogGroup' smart constructor.
newtype CreateLogGroup = CreateLogGroup'
    { _clgLogGroupName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateLogGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'clgLogGroupName'
createLogGroup
    :: Text -- ^ 'clgLogGroupName'
    -> CreateLogGroup
createLogGroup pLogGroupName_ =
    CreateLogGroup'
    { _clgLogGroupName = pLogGroupName_
    }

-- | The name of the log group to create.
clgLogGroupName :: Lens' CreateLogGroup Text
clgLogGroupName = lens _clgLogGroupName (\ s a -> s{_clgLogGroupName = a});

instance AWSRequest CreateLogGroup where
        type Rs CreateLogGroup = CreateLogGroupResponse
        request = postJSON cloudWatchLogs
        response = receiveNull CreateLogGroupResponse'

instance Hashable CreateLogGroup

instance NFData CreateLogGroup

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
          = object
              (catMaybes
                 [Just ("logGroupName" .= _clgLogGroupName)])

instance ToPath CreateLogGroup where
        toPath = const "/"

instance ToQuery CreateLogGroup where
        toQuery = const mempty

-- | /See:/ 'createLogGroupResponse' smart constructor.
data CreateLogGroupResponse =
    CreateLogGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateLogGroupResponse' with the minimum fields required to make a request.
--
createLogGroupResponse
    :: CreateLogGroupResponse
createLogGroupResponse = CreateLogGroupResponse'

instance NFData CreateLogGroupResponse
