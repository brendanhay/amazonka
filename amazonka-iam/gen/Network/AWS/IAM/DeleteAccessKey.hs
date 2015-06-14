{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.DeleteAccessKey
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

-- | Deletes the access key associated with the specified user.
--
-- If you do not specify a user name, IAM determines the user name
-- implicitly based on the AWS access key ID signing the request. Because
-- this action works for access keys under the AWS account, you can use
-- this action to manage root credentials even if the AWS account has no
-- associated users.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteAccessKey.html>
module Network.AWS.IAM.DeleteAccessKey
    (
    -- * Request
      DeleteAccessKey
    -- ** Request constructor
    , deleteAccessKey
    -- ** Request lenses
    , dakUserName
    , dakAccessKeyId

    -- * Response
    , DeleteAccessKeyResponse
    -- ** Response constructor
    , deleteAccessKeyResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'deleteAccessKey' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dakUserName'
--
-- * 'dakAccessKeyId'
data DeleteAccessKey = DeleteAccessKey'{_dakUserName :: Maybe Text, _dakAccessKeyId :: Text} deriving (Eq, Read, Show)

-- | 'DeleteAccessKey' smart constructor.
deleteAccessKey :: Text -> DeleteAccessKey
deleteAccessKey pAccessKeyId = DeleteAccessKey'{_dakUserName = Nothing, _dakAccessKeyId = pAccessKeyId};

-- | The name of the user whose key you want to delete.
dakUserName :: Lens' DeleteAccessKey (Maybe Text)
dakUserName = lens _dakUserName (\ s a -> s{_dakUserName = a});

-- | The access key ID for the access key ID and secret access key you want
-- to delete.
dakAccessKeyId :: Lens' DeleteAccessKey Text
dakAccessKeyId = lens _dakAccessKeyId (\ s a -> s{_dakAccessKeyId = a});

instance AWSRequest DeleteAccessKey where
        type Sv DeleteAccessKey = IAM
        type Rs DeleteAccessKey = DeleteAccessKeyResponse
        request = post
        response = receiveNull DeleteAccessKeyResponse'

instance ToHeaders DeleteAccessKey where
        toHeaders = const mempty

instance ToPath DeleteAccessKey where
        toPath = const "/"

instance ToQuery DeleteAccessKey where
        toQuery DeleteAccessKey'{..}
          = mconcat
              ["Action" =: ("DeleteAccessKey" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _dakUserName,
               "AccessKeyId" =: _dakAccessKeyId]

-- | /See:/ 'deleteAccessKeyResponse' smart constructor.
data DeleteAccessKeyResponse = DeleteAccessKeyResponse' deriving (Eq, Read, Show)

-- | 'DeleteAccessKeyResponse' smart constructor.
deleteAccessKeyResponse :: DeleteAccessKeyResponse
deleteAccessKeyResponse = DeleteAccessKeyResponse';
