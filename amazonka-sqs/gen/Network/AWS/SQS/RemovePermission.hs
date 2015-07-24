{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SQS.RemovePermission
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Revokes any permissions in the queue policy that matches the specified
-- @Label@ parameter. Only the owner of the queue can remove permissions.
--
-- <http://docs.aws.amazon.com/AWSSimpleQueueService/latest/APIReference/API_RemovePermission.html>
module Network.AWS.SQS.RemovePermission
    (
    -- * Request
      RemovePermission
    -- ** Request constructor
    , removePermission
    -- ** Request lenses
    , rpQueueURL
    , rpLabel

    -- * Response
    , RemovePermissionResponse
    -- ** Response constructor
    , removePermissionResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SQS.Types

-- | /See:/ 'removePermission' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rpQueueURL'
--
-- * 'rpLabel'
data RemovePermission = RemovePermission'
    { _rpQueueURL :: !Text
    , _rpLabel    :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemovePermission' smart constructor.
removePermission :: Text -> Text -> RemovePermission
removePermission pQueueURL_ pLabel_ =
    RemovePermission'
    { _rpQueueURL = pQueueURL_
    , _rpLabel = pLabel_
    }

-- | The URL of the Amazon SQS queue to take action on.
rpQueueURL :: Lens' RemovePermission Text
rpQueueURL = lens _rpQueueURL (\ s a -> s{_rpQueueURL = a});

-- | The identification of the permission to remove. This is the label added
-- with the AddPermission action.
rpLabel :: Lens' RemovePermission Text
rpLabel = lens _rpLabel (\ s a -> s{_rpLabel = a});

instance AWSRequest RemovePermission where
        type Sv RemovePermission = SQS
        type Rs RemovePermission = RemovePermissionResponse
        request = post "RemovePermission"
        response = receiveNull RemovePermissionResponse'

instance ToHeaders RemovePermission where
        toHeaders = const mempty

instance ToPath RemovePermission where
        toPath = const "/"

instance ToQuery RemovePermission where
        toQuery RemovePermission'{..}
          = mconcat
              ["Action" =: ("RemovePermission" :: ByteString),
               "Version" =: ("2012-11-05" :: ByteString),
               "QueueUrl" =: _rpQueueURL, "Label" =: _rpLabel]

-- | /See:/ 'removePermissionResponse' smart constructor.
data RemovePermissionResponse =
    RemovePermissionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RemovePermissionResponse' smart constructor.
removePermissionResponse :: RemovePermissionResponse
removePermissionResponse = RemovePermissionResponse'
