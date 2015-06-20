{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudFormation.CancelUpdateStack
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

-- | Cancels an update on the specified stack. If the call completes
-- successfully, the stack will roll back the update and revert to the
-- previous stack configuration.
--
-- Only stacks that are in the UPDATE_IN_PROGRESS state can be canceled.
--
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/APIReference/API_CancelUpdateStack.html>
module Network.AWS.CloudFormation.CancelUpdateStack
    (
    -- * Request
      CancelUpdateStack
    -- ** Request constructor
    , cancelUpdateStack
    -- ** Request lenses
    , cusStackName

    -- * Response
    , CancelUpdateStackResponse
    -- ** Response constructor
    , cancelUpdateStackResponse
    ) where

import Network.AWS.CloudFormation.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'cancelUpdateStack' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cusStackName'
newtype CancelUpdateStack = CancelUpdateStack'{_cusStackName :: Text} deriving (Eq, Read, Show)

-- | 'CancelUpdateStack' smart constructor.
cancelUpdateStack :: Text -> CancelUpdateStack
cancelUpdateStack pStackName = CancelUpdateStack'{_cusStackName = pStackName};

-- | The name or the unique stack ID that is associated with the stack.
cusStackName :: Lens' CancelUpdateStack Text
cusStackName = lens _cusStackName (\ s a -> s{_cusStackName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest CancelUpdateStack where
        type Sv CancelUpdateStack = CloudFormation
        type Rs CancelUpdateStack = CancelUpdateStackResponse
        request = post
        response = receiveNull CancelUpdateStackResponse'

instance ToHeaders CancelUpdateStack where
        toHeaders = const mempty

instance ToPath CancelUpdateStack where
        toPath = const "/"

instance ToQuery CancelUpdateStack where
        toQuery CancelUpdateStack'{..}
          = mconcat
              ["Action" =: ("CancelUpdateStack" :: ByteString),
               "Version" =: ("2010-05-15" :: ByteString),
               "StackName" =: _cusStackName]

-- | /See:/ 'cancelUpdateStackResponse' smart constructor.
data CancelUpdateStackResponse = CancelUpdateStackResponse' deriving (Eq, Read, Show)

-- | 'CancelUpdateStackResponse' smart constructor.
cancelUpdateStackResponse :: CancelUpdateStackResponse
cancelUpdateStackResponse = CancelUpdateStackResponse';
