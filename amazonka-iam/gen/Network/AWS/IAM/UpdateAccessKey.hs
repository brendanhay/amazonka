{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.IAM.UpdateAccessKey
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

-- | Changes the status of the specified access key from Active to Inactive,
-- or vice versa. This action can be used to disable a user\'s key as part
-- of a key rotation work flow.
--
-- If the @UserName@ field is not specified, the UserName is determined
-- implicitly based on the AWS access key ID used to sign the request.
-- Because this action works for access keys under the AWS account, you can
-- use this action to manage root credentials even if the AWS account has
-- no associated users.
--
-- For information about rotating keys, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/ManagingCredentials.html Managing Keys and Certificates>
-- in the /Using IAM/ guide.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_UpdateAccessKey.html>
module Network.AWS.IAM.UpdateAccessKey
    (
    -- * Request
      UpdateAccessKey
    -- ** Request constructor
    , updateAccessKey
    -- ** Request lenses
    , uakAccessKeyId
    , uakStatus
    , uakUserName

    -- * Response
    , UpdateAccessKeyResponse
    -- ** Response constructor
    , updateAccessKeyResponse
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.IAM.Types

-- | /See:/ 'updateAccessKey' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'uakAccessKeyId'
--
-- * 'uakStatus'
--
-- * 'uakUserName'
data UpdateAccessKey = UpdateAccessKey'{_uakAccessKeyId :: Text, _uakStatus :: StatusType, _uakUserName :: Text} deriving (Eq, Read, Show)

-- | 'UpdateAccessKey' smart constructor.
updateAccessKey :: Text -> StatusType -> Text -> UpdateAccessKey
updateAccessKey pAccessKeyId pStatus pUserName = UpdateAccessKey'{_uakAccessKeyId = pAccessKeyId, _uakStatus = pStatus, _uakUserName = pUserName};

-- | The access key ID of the secret access key you want to update.
uakAccessKeyId :: Lens' UpdateAccessKey Text
uakAccessKeyId = lens _uakAccessKeyId (\ s a -> s{_uakAccessKeyId = a});

-- | The status you want to assign to the secret access key. @Active@ means
-- the key can be used for API calls to AWS, while @Inactive@ means the key
-- cannot be used.
uakStatus :: Lens' UpdateAccessKey StatusType
uakStatus = lens _uakStatus (\ s a -> s{_uakStatus = a});

-- | The name of the user whose key you want to update.
uakUserName :: Lens' UpdateAccessKey Text
uakUserName = lens _uakUserName (\ s a -> s{_uakUserName = a});

instance AWSRequest UpdateAccessKey where
        type Sv UpdateAccessKey = IAM
        type Rs UpdateAccessKey = UpdateAccessKeyResponse
        request = post
        response = receiveNull UpdateAccessKeyResponse'

instance ToHeaders UpdateAccessKey where
        toHeaders = const mempty

instance ToPath UpdateAccessKey where
        toPath = const "/"

instance ToQuery UpdateAccessKey where
        toQuery UpdateAccessKey'{..}
          = mconcat
              ["Action" =: ("UpdateAccessKey" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "AccessKeyId" =: _uakAccessKeyId,
               "Status" =: _uakStatus, "UserName" =: _uakUserName]

-- | /See:/ 'updateAccessKeyResponse' smart constructor.
data UpdateAccessKeyResponse = UpdateAccessKeyResponse' deriving (Eq, Read, Show)

-- | 'UpdateAccessKeyResponse' smart constructor.
updateAccessKeyResponse :: UpdateAccessKeyResponse
updateAccessKeyResponse = UpdateAccessKeyResponse';
