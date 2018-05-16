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
-- Module      : Network.AWS.IAM.DeleteLoginProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the password for the specified IAM user, which terminates the user's ability to access AWS services through the AWS Management Console.
--
--
-- /Important:/ Deleting a user's password does not prevent a user from accessing AWS through the command line interface or the API. To prevent all user access you must also either make any access keys inactive or delete them. For more information about making keys inactive or deleting them, see 'UpdateAccessKey' and 'DeleteAccessKey' .
--
module Network.AWS.IAM.DeleteLoginProfile
    (
    -- * Creating a Request
      deleteLoginProfile
    , DeleteLoginProfile
    -- * Request Lenses
    , dlpUserName

    -- * Destructuring the Response
    , deleteLoginProfileResponse
    , DeleteLoginProfileResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteLoginProfile' smart constructor.
newtype DeleteLoginProfile = DeleteLoginProfile'
  { _dlpUserName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLoginProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dlpUserName' - The name of the user whose password you want to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
deleteLoginProfile
    :: Text -- ^ 'dlpUserName'
    -> DeleteLoginProfile
deleteLoginProfile pUserName_ = DeleteLoginProfile' {_dlpUserName = pUserName_}


-- | The name of the user whose password you want to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
dlpUserName :: Lens' DeleteLoginProfile Text
dlpUserName = lens _dlpUserName (\ s a -> s{_dlpUserName = a})

instance AWSRequest DeleteLoginProfile where
        type Rs DeleteLoginProfile =
             DeleteLoginProfileResponse
        request = postQuery iam
        response = receiveNull DeleteLoginProfileResponse'

instance Hashable DeleteLoginProfile where

instance NFData DeleteLoginProfile where

instance ToHeaders DeleteLoginProfile where
        toHeaders = const mempty

instance ToPath DeleteLoginProfile where
        toPath = const "/"

instance ToQuery DeleteLoginProfile where
        toQuery DeleteLoginProfile'{..}
          = mconcat
              ["Action" =: ("DeleteLoginProfile" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _dlpUserName]

-- | /See:/ 'deleteLoginProfileResponse' smart constructor.
data DeleteLoginProfileResponse =
  DeleteLoginProfileResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteLoginProfileResponse' with the minimum fields required to make a request.
--
deleteLoginProfileResponse
    :: DeleteLoginProfileResponse
deleteLoginProfileResponse = DeleteLoginProfileResponse'


instance NFData DeleteLoginProfileResponse where
