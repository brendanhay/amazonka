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
-- for the specified user. The default status for new keys is @Active@.
--
-- If you do not specify a user name, IAM determines the user name
-- implicitly based on the AWS access key ID signing the request. Because
-- this action works for access keys under the AWS account, you can use
-- this action to manage root credentials even if the AWS account has no
-- associated users.
--
-- For information about limits on the number of keys you can create, see
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/LimitationsOnEntities.html Limitations on IAM Entities>
-- in the /Using IAM/ guide.
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
      CreateAccessKey
    , createAccessKey
    -- * Request Lenses
    , cakUserName

    -- * Destructuring the Response
    , CreateAccessKeyResponse
    , createAccessKeyResponse
    -- * Response Lenses
    , cakrsStatus
    , cakrsAccessKey
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.IAM.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createAccessKey' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cakUserName'
newtype CreateAccessKey = CreateAccessKey'
    { _cakUserName :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAccessKey' smart constructor.
createAccessKey :: CreateAccessKey
createAccessKey =
    CreateAccessKey'
    { _cakUserName = Nothing
    }

-- | The user name that the new key will belong to.
cakUserName :: Lens' CreateAccessKey (Maybe Text)
cakUserName = lens _cakUserName (\ s a -> s{_cakUserName = a});

instance AWSRequest CreateAccessKey where
        type Sv CreateAccessKey = IAM
        type Rs CreateAccessKey = CreateAccessKeyResponse
        request = postQuery
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
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cakrsStatus'
--
-- * 'cakrsAccessKey'
data CreateAccessKeyResponse = CreateAccessKeyResponse'
    { _cakrsStatus    :: !Int
    , _cakrsAccessKey :: !AccessKey
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreateAccessKeyResponse' smart constructor.
createAccessKeyResponse :: Int -> AccessKey -> CreateAccessKeyResponse
createAccessKeyResponse pStatus_ pAccessKey_ =
    CreateAccessKeyResponse'
    { _cakrsStatus = pStatus_
    , _cakrsAccessKey = pAccessKey_
    }

-- | Undocumented member.
cakrsStatus :: Lens' CreateAccessKeyResponse Int
cakrsStatus = lens _cakrsStatus (\ s a -> s{_cakrsStatus = a});

-- | Information about the access key.
cakrsAccessKey :: Lens' CreateAccessKeyResponse AccessKey
cakrsAccessKey = lens _cakrsAccessKey (\ s a -> s{_cakrsAccessKey = a});
