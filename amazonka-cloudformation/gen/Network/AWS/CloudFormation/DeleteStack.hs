{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFormation.DeleteStack
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

-- | Deletes a specified stack. Once the call completes successfully, stack
-- deletion starts. Deleted stacks do not show up in the DescribeStacks API
-- if the deletion has been completed successfully.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_DeleteStack.html>
module Network.AWS.CloudFormation.DeleteStack
    (
    -- * Request
      DeleteStack
    -- ** Request constructor
    , deleteStack
    -- ** Request lenses
    , dsStackName

    -- * Response
    , DeleteStackResponse
    -- ** Response constructor
    , deleteStackResponse
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteStack' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsStackName'
newtype DeleteStack = DeleteStack'{_dsStackName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteStack' smart constructor.
deleteStack :: Text -> DeleteStack
deleteStack pStackName = DeleteStack'{_dsStackName = pStackName};

-- | The name or the unique stack ID that is associated with the stack.
dsStackName :: Lens' DeleteStack Text
dsStackName = lens _dsStackName (\ s a -> s{_dsStackName = a});

instance AWSRequest DeleteStack where
        type Sv DeleteStack = CloudFormation
        type Rs DeleteStack = DeleteStackResponse
        request = post
        response = receiveNull DeleteStackResponse'

instance ToHeaders DeleteStack where
        toHeaders = const mempty

instance ToPath DeleteStack where
        toPath = const "/"

instance ToQuery DeleteStack where
        toQuery DeleteStack'{..}
          = mconcat
              ["Action" =: ("DeleteStack" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackName" =: _dsStackName]

-- | /See:/ 'deleteStackResponse' smart constructor.
data DeleteStackResponse = DeleteStackResponse' deriving (Eq, Read, Show)

-- | 'DeleteStackResponse' smart constructor.
deleteStackResponse :: DeleteStackResponse
deleteStackResponse = DeleteStackResponse';
