{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.StopStack
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Stops a specified stack.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_StopStack.html>
module Network.AWS.OpsWorks.StopStack
    (
    -- * Request
      StopStack
    -- ** Request constructor
    , stopStack
    -- ** Request lenses
    , stoStackId

    -- * Response
    , StopStackResponse
    -- ** Response constructor
    , stopStackResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'stopStack' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'stoStackId'
newtype StopStack = StopStack'
    { _stoStackId :: Text
    } deriving (Eq,Read,Show)

-- | 'StopStack' smart constructor.
stopStack :: Text -> StopStack
stopStack pStackId =
    StopStack'
    { _stoStackId = pStackId
    }

-- | The stack ID.
stoStackId :: Lens' StopStack Text
stoStackId = lens _stoStackId (\ s a -> s{_stoStackId = a});

instance AWSRequest StopStack where
        type Sv StopStack = OpsWorks
        type Rs StopStack = StopStackResponse
        request = postJSON
        response = receiveNull StopStackResponse'

instance ToHeaders StopStack where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.StopStack" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON StopStack where
        toJSON StopStack'{..}
          = object ["StackId" .= _stoStackId]

instance ToPath StopStack where
        toPath = const "/"

instance ToQuery StopStack where
        toQuery = const mempty

-- | /See:/ 'stopStackResponse' smart constructor.
data StopStackResponse =
    StopStackResponse'
    deriving (Eq,Read,Show)

-- | 'StopStackResponse' smart constructor.
stopStackResponse :: StopStackResponse
stopStackResponse = StopStackResponse'
