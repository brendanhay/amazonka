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
-- Module      : Network.AWS.IAM.CreateAccessKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new AWS secret access key and corresponding AWS access key ID for the specified user. The default status for new keys is @Active@ .
--
--
-- If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID signing the request. Because this operation works for access keys under the AWS account, you can use this operation to manage AWS account root user credentials. This is true even if the AWS account has no associated users.
--
-- For information about limits on the number of keys you can create, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities> in the /IAM User Guide/ .
--
-- /Important:/ To ensure the security of your AWS account, the secret access key is accessible only during key and user creation. You must save the key (for example, in a text file) if you want to be able to access it again. If a secret key is lost, you can delete the access keys for the associated user and then create new keys.
--
module Network.AWS.IAM.CreateAccessKey
    (
    -- * Creating a Request
      createAccessKey
    , CreateAccessKey
    -- * Request Lenses
    , cakUserName

    -- * Destructuring the Response
    , createAccessKeyResponse
    , CreateAccessKeyResponse
    -- * Response Lenses
    , cakrsResponseStatus
    , cakrsAccessKey
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createAccessKey' smart constructor.
newtype CreateAccessKey = CreateAccessKey'
  { _cakUserName :: Maybe Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAccessKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cakUserName' - The name of the IAM user that the new key will belong to. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
createAccessKey
    :: CreateAccessKey
createAccessKey = CreateAccessKey' {_cakUserName = Nothing}


-- | The name of the IAM user that the new key will belong to. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
cakUserName :: Lens' CreateAccessKey (Maybe Text)
cakUserName = lens _cakUserName (\ s a -> s{_cakUserName = a})

instance AWSRequest CreateAccessKey where
        type Rs CreateAccessKey = CreateAccessKeyResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "CreateAccessKeyResult"
              (\ s h x ->
                 CreateAccessKeyResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "AccessKey"))

instance Hashable CreateAccessKey where

instance NFData CreateAccessKey where

instance ToHeaders CreateAccessKey where
        toHeaders = const mempty

instance ToPath CreateAccessKey where
        toPath = const "/"

instance ToQuery CreateAccessKey where
        toQuery CreateAccessKey'{..}
          = mconcat
              ["Action" =: ("CreateAccessKey" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _cakUserName]

-- | Contains the response to a successful 'CreateAccessKey' request.
--
--
--
-- /See:/ 'createAccessKeyResponse' smart constructor.
data CreateAccessKeyResponse = CreateAccessKeyResponse'
  { _cakrsResponseStatus :: !Int
  , _cakrsAccessKey      :: !AccessKeyInfo
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateAccessKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cakrsResponseStatus' - -- | The response status code.
--
-- * 'cakrsAccessKey' - A structure with details about the access key.
createAccessKeyResponse
    :: Int -- ^ 'cakrsResponseStatus'
    -> AccessKeyInfo -- ^ 'cakrsAccessKey'
    -> CreateAccessKeyResponse
createAccessKeyResponse pResponseStatus_ pAccessKey_ =
  CreateAccessKeyResponse'
    {_cakrsResponseStatus = pResponseStatus_, _cakrsAccessKey = pAccessKey_}


-- | -- | The response status code.
cakrsResponseStatus :: Lens' CreateAccessKeyResponse Int
cakrsResponseStatus = lens _cakrsResponseStatus (\ s a -> s{_cakrsResponseStatus = a})

-- | A structure with details about the access key.
cakrsAccessKey :: Lens' CreateAccessKeyResponse AccessKeyInfo
cakrsAccessKey = lens _cakrsAccessKey (\ s a -> s{_cakrsAccessKey = a})

instance NFData CreateAccessKeyResponse where
