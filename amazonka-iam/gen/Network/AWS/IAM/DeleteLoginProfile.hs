{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.IAM.DeleteLoginProfile
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the password for the specified user, which terminates the
-- user\'s ability to access AWS services through the AWS Management
-- Console.
--
-- Deleting a user\'s password does not prevent a user from accessing IAM
-- through the command line interface or the API. To prevent all user
-- access you must also either make the access key inactive or delete it.
-- For more information about making keys inactive or deleting them, see
-- UpdateAccessKey and DeleteAccessKey.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteLoginProfile.html>
module Network.AWS.IAM.DeleteLoginProfile
    (
    -- * Request
      DeleteLoginProfile
    -- ** Request constructor
    , deleteLoginProfile
    -- ** Request lenses
    , dlpUserName

    -- * Response
    , DeleteLoginProfileResponse
    -- ** Response constructor
    , deleteLoginProfileResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteLoginProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dlpUserName'
newtype DeleteLoginProfile = DeleteLoginProfile'
    { _dlpUserName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLoginProfile' smart constructor.
deleteLoginProfile :: Text -> DeleteLoginProfile
deleteLoginProfile pUserName =
    DeleteLoginProfile'
    { _dlpUserName = pUserName
    }

-- | The name of the user whose password you want to delete.
dlpUserName :: Lens' DeleteLoginProfile Text
dlpUserName = lens _dlpUserName (\ s a -> s{_dlpUserName = a});

instance AWSRequest DeleteLoginProfile where
        type Sv DeleteLoginProfile = IAM
        type Rs DeleteLoginProfile =
             DeleteLoginProfileResponse
        request = post
        response = receiveNull DeleteLoginProfileResponse'

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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteLoginProfileResponse' smart constructor.
deleteLoginProfileResponse :: DeleteLoginProfileResponse
deleteLoginProfileResponse = DeleteLoginProfileResponse'
