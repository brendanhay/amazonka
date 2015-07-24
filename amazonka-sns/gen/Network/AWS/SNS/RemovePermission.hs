{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SNS.RemovePermission
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes a statement from a topic\'s access control policy.
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_RemovePermission.html>
module Network.AWS.SNS.RemovePermission
    (
    -- * Request
      RemovePermission
    -- ** Request constructor
    , removePermission
    -- ** Request lenses
    , rpTopicARN
    , rpLabel

    -- * Response
    , RemovePermissionResponse
    -- ** Response constructor
    , removePermissionResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SNS.Types

-- | Input for RemovePermission action.
--
-- /See:/ 'removePermission' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rpTopicARN'
--
-- * 'rpLabel'
data RemovePermission = RemovePermission'
    { _rpTopicARN :: !Text
    , _rpLabel    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemovePermission' smart constructor.
removePermission :: Text -> Text -> RemovePermission
removePermission pTopicARN_ pLabel_ =
    RemovePermission'
    { _rpTopicARN = pTopicARN_
    , _rpLabel = pLabel_
    }

-- | The ARN of the topic whose access control policy you wish to modify.
rpTopicARN :: Lens' RemovePermission Text
rpTopicARN = lens _rpTopicARN (\ s a -> s{_rpTopicARN = a});

-- | The unique label of the statement you want to remove.
rpLabel :: Lens' RemovePermission Text
rpLabel = lens _rpLabel (\ s a -> s{_rpLabel = a});

instance AWSRequest RemovePermission where
        type Sv RemovePermission = SNS
        type Rs RemovePermission = RemovePermissionResponse
        request = post
        response = receiveNull RemovePermissionResponse'

instance ToHeaders RemovePermission where
        toHeaders = const mempty

instance ToPath RemovePermission where
        toPath = const "/"

instance ToQuery RemovePermission where
        toQuery RemovePermission'{..}
          = mconcat
              ["Action" =: ("RemovePermission" :: ByteString),
               "Version" =: ("2010-03-31" :: ByteString),
               "TopicArn" =: _rpTopicARN, "Label" =: _rpLabel]

-- | /See:/ 'removePermissionResponse' smart constructor.
data RemovePermissionResponse =
    RemovePermissionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemovePermissionResponse' smart constructor.
removePermissionResponse :: RemovePermissionResponse
removePermissionResponse = RemovePermissionResponse'
