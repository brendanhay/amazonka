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
-- Module      : Network.AWS.IAM.UpdateAccessKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the status of the specified access key from Active to Inactive, or vice versa. This operation can be used to disable a user's key as part of a key rotation workflow.
--
--
-- If the @UserName@ field is not specified, the user name is determined implicitly based on the AWS access key ID used to sign the request. Because this operation works for access keys under the AWS account, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated users.
--
-- For information about rotating keys, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/ManagingCredentials.html Managing Keys and Certificates> in the /IAM User Guide/ .
--
module Network.AWS.IAM.UpdateAccessKey
    (
    -- * Creating a Request
      updateAccessKey
    , UpdateAccessKey
    -- * Request Lenses
    , uakUserName
    , uakAccessKeyId
    , uakStatus

    -- * Destructuring the Response
    , updateAccessKeyResponse
    , UpdateAccessKeyResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateAccessKey' smart constructor.
data UpdateAccessKey = UpdateAccessKey'
  { _uakUserName    :: !(Maybe Text)
  , _uakAccessKeyId :: !AccessKey
  , _uakStatus      :: !StatusType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAccessKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'uakUserName' - The name of the user whose key you want to update. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'uakAccessKeyId' - The access key ID of the secret access key you want to update. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
--
-- * 'uakStatus' - The status you want to assign to the secret access key. @Active@ means that the key can be used for API calls to AWS, while @Inactive@ means that the key cannot be used.
updateAccessKey
    :: AccessKey -- ^ 'uakAccessKeyId'
    -> StatusType -- ^ 'uakStatus'
    -> UpdateAccessKey
updateAccessKey pAccessKeyId_ pStatus_ =
  UpdateAccessKey'
    { _uakUserName = Nothing
    , _uakAccessKeyId = pAccessKeyId_
    , _uakStatus = pStatus_
    }


-- | The name of the user whose key you want to update. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
uakUserName :: Lens' UpdateAccessKey (Maybe Text)
uakUserName = lens _uakUserName (\ s a -> s{_uakUserName = a})

-- | The access key ID of the secret access key you want to update. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
uakAccessKeyId :: Lens' UpdateAccessKey AccessKey
uakAccessKeyId = lens _uakAccessKeyId (\ s a -> s{_uakAccessKeyId = a})

-- | The status you want to assign to the secret access key. @Active@ means that the key can be used for API calls to AWS, while @Inactive@ means that the key cannot be used.
uakStatus :: Lens' UpdateAccessKey StatusType
uakStatus = lens _uakStatus (\ s a -> s{_uakStatus = a})

instance AWSRequest UpdateAccessKey where
        type Rs UpdateAccessKey = UpdateAccessKeyResponse
        request = postQuery iam
        response = receiveNull UpdateAccessKeyResponse'

instance Hashable UpdateAccessKey where

instance NFData UpdateAccessKey where

instance ToHeaders UpdateAccessKey where
        toHeaders = const mempty

instance ToPath UpdateAccessKey where
        toPath = const "/"

instance ToQuery UpdateAccessKey where
        toQuery UpdateAccessKey'{..}
          = mconcat
              ["Action" =: ("UpdateAccessKey" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _uakUserName,
               "AccessKeyId" =: _uakAccessKeyId,
               "Status" =: _uakStatus]

-- | /See:/ 'updateAccessKeyResponse' smart constructor.
data UpdateAccessKeyResponse =
  UpdateAccessKeyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateAccessKeyResponse' with the minimum fields required to make a request.
--
updateAccessKeyResponse
    :: UpdateAccessKeyResponse
updateAccessKeyResponse = UpdateAccessKeyResponse'


instance NFData UpdateAccessKeyResponse where
