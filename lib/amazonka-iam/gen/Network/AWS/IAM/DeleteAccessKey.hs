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
-- Module      : Network.AWS.IAM.DeleteAccessKey
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the access key pair associated with the specified IAM user.
--
--
-- If you do not specify a user name, IAM determines the user name implicitly based on the AWS access key ID signing the request. Because this operation works for access keys under the AWS account, you can use this operation to manage AWS account root user credentials even if the AWS account has no associated users.
--
module Network.AWS.IAM.DeleteAccessKey
    (
    -- * Creating a Request
      deleteAccessKey
    , DeleteAccessKey
    -- * Request Lenses
    , dakUserName
    , dakAccessKeyId

    -- * Destructuring the Response
    , deleteAccessKeyResponse
    , DeleteAccessKeyResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAccessKey' smart constructor.
data DeleteAccessKey = DeleteAccessKey'
  { _dakUserName    :: !(Maybe Text)
  , _dakAccessKeyId :: !AccessKey
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAccessKey' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dakUserName' - The name of the user whose access key pair you want to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- * 'dakAccessKeyId' - The access key ID for the access key ID and secret access key you want to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
deleteAccessKey
    :: AccessKey -- ^ 'dakAccessKeyId'
    -> DeleteAccessKey
deleteAccessKey pAccessKeyId_ =
  DeleteAccessKey' {_dakUserName = Nothing, _dakAccessKeyId = pAccessKeyId_}


-- | The name of the user whose access key pair you want to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
dakUserName :: Lens' DeleteAccessKey (Maybe Text)
dakUserName = lens _dakUserName (\ s a -> s{_dakUserName = a})

-- | The access key ID for the access key ID and secret access key you want to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters that can consist of any upper or lowercased letter or digit.
dakAccessKeyId :: Lens' DeleteAccessKey AccessKey
dakAccessKeyId = lens _dakAccessKeyId (\ s a -> s{_dakAccessKeyId = a})

instance AWSRequest DeleteAccessKey where
        type Rs DeleteAccessKey = DeleteAccessKeyResponse
        request = postQuery iam
        response = receiveNull DeleteAccessKeyResponse'

instance Hashable DeleteAccessKey where

instance NFData DeleteAccessKey where

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
data DeleteAccessKeyResponse =
  DeleteAccessKeyResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAccessKeyResponse' with the minimum fields required to make a request.
--
deleteAccessKeyResponse
    :: DeleteAccessKeyResponse
deleteAccessKeyResponse = DeleteAccessKeyResponse'


instance NFData DeleteAccessKeyResponse where
