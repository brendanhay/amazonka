{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.CancelUpdateStack
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Cancels an update on the specified stack. If the call completes
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

import           Network.AWS.CloudFormation.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | The input for CancelUpdateStack action.
--
-- /See:/ 'cancelUpdateStack' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cusStackName'
newtype CancelUpdateStack = CancelUpdateStack'
    { _cusStackName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelUpdateStack' smart constructor.
cancelUpdateStack :: Text -> CancelUpdateStack
cancelUpdateStack pStackName_ =
    CancelUpdateStack'
    { _cusStackName = pStackName_
    }

-- | The name or the unique stack ID that is associated with the stack.
cusStackName :: Lens' CancelUpdateStack Text
cusStackName = lens _cusStackName (\ s a -> s{_cusStackName = a});

instance AWSRequest CancelUpdateStack where
        type Sv CancelUpdateStack = CloudFormation
        type Rs CancelUpdateStack = CancelUpdateStackResponse
        request = postQuery
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
data CancelUpdateStackResponse =
    CancelUpdateStackResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelUpdateStackResponse' smart constructor.
cancelUpdateStackResponse :: CancelUpdateStackResponse
cancelUpdateStackResponse = CancelUpdateStackResponse'
