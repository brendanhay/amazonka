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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new AWS secret access key and corresponding AWS access key ID
-- for the specified user. The default status for new keys is 'Active'.
--
-- If you do not specify a user name, IAM determines the user name
-- implicitly based on the AWS access key ID signing the request. Because
-- this action works for access keys under the AWS account, you can use
-- this action to manage root credentials even if the AWS account has no
-- associated users.
--
-- For information about limits on the number of keys you can create, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities>
-- in the /IAM User Guide/.
--
-- To ensure the security of your AWS account, the secret access key is
-- accessible only during key and user creation. You must save the key (for
-- example, in a text file) if you want to be able to access it again. If a
-- secret key is lost, you can delete the access keys for the associated
-- user and then create new keys.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateAccessKey.html AWS API Reference> for CreateAccessKey.
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

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createAccessKey' smart constructor.
newtype CreateAccessKey = CreateAccessKey'
    { _cakUserName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateAccessKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cakUserName'
createAccessKey
    :: CreateAccessKey
createAccessKey =
    CreateAccessKey'
    { _cakUserName = Nothing
    }

-- | The user name that the new key will belong to.
cakUserName :: Lens' CreateAccessKey (Maybe Text)
cakUserName = lens _cakUserName (\ s a -> s{_cakUserName = a});

instance AWSRequest CreateAccessKey where
        type Rs CreateAccessKey = CreateAccessKeyResponse
        request = postQuery iAM
        response
          = receiveXMLWrapper "CreateAccessKeyResult"
              (\ s h x ->
                 CreateAccessKeyResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "AccessKey"))

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

-- | Contains the response to a successful CreateAccessKey request.
--
-- /See:/ 'createAccessKeyResponse' smart constructor.
data CreateAccessKeyResponse = CreateAccessKeyResponse'
    { _cakrsResponseStatus :: !Int
    , _cakrsAccessKey      :: !AccessKey
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreateAccessKeyResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cakrsResponseStatus'
--
-- * 'cakrsAccessKey'
createAccessKeyResponse
    :: Int -- ^ 'cakrsResponseStatus'
    -> AccessKey -- ^ 'cakrsAccessKey'
    -> CreateAccessKeyResponse
createAccessKeyResponse pResponseStatus_ pAccessKey_ =
    CreateAccessKeyResponse'
    { _cakrsResponseStatus = pResponseStatus_
    , _cakrsAccessKey = pAccessKey_
    }

-- | The response status code.
cakrsResponseStatus :: Lens' CreateAccessKeyResponse Int
cakrsResponseStatus = lens _cakrsResponseStatus (\ s a -> s{_cakrsResponseStatus = a});

-- | Information about the access key.
cakrsAccessKey :: Lens' CreateAccessKeyResponse AccessKey
cakrsAccessKey = lens _cakrsAccessKey (\ s a -> s{_cakrsAccessKey = a});
