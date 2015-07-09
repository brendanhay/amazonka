{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.DeleteInstanceProfile
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Deletes the specified instance profile. The instance profile must not
-- have an associated role.
--
-- Make sure you do not have any Amazon EC2 instances running with the
-- instance profile you are about to delete. Deleting a role or instance
-- profile that is associated with a running instance will break any
-- applications running on the instance.
--
-- For more information about instance profiles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_DeleteInstanceProfile.html>
module Network.AWS.IAM.DeleteInstanceProfile
    (
    -- * Request
      DeleteInstanceProfile
    -- ** Request constructor
    , deleteInstanceProfile
    -- ** Request lenses
    , dipInstanceProfileName

    -- * Response
    , DeleteInstanceProfileResponse
    -- ** Response constructor
    , deleteInstanceProfileResponse
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteInstanceProfile' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dipInstanceProfileName'
newtype DeleteInstanceProfile = DeleteInstanceProfile'
    { _dipInstanceProfileName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteInstanceProfile' smart constructor.
deleteInstanceProfile :: Text -> DeleteInstanceProfile
deleteInstanceProfile pInstanceProfileName =
    DeleteInstanceProfile'
    { _dipInstanceProfileName = pInstanceProfileName
    }

-- | The name of the instance profile to delete.
dipInstanceProfileName :: Lens' DeleteInstanceProfile Text
dipInstanceProfileName = lens _dipInstanceProfileName (\ s a -> s{_dipInstanceProfileName = a});

instance AWSRequest DeleteInstanceProfile where
        type Sv DeleteInstanceProfile = IAM
        type Rs DeleteInstanceProfile =
             DeleteInstanceProfileResponse
        request = post
        response = receiveNull DeleteInstanceProfileResponse'

instance ToHeaders DeleteInstanceProfile where
        toHeaders = const mempty

instance ToPath DeleteInstanceProfile where
        toPath = const "/"

instance ToQuery DeleteInstanceProfile where
        toQuery DeleteInstanceProfile'{..}
          = mconcat
              ["Action" =: ("DeleteInstanceProfile" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "InstanceProfileName" =: _dipInstanceProfileName]

-- | /See:/ 'deleteInstanceProfileResponse' smart constructor.
data DeleteInstanceProfileResponse =
    DeleteInstanceProfileResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteInstanceProfileResponse' smart constructor.
deleteInstanceProfileResponse :: DeleteInstanceProfileResponse
deleteInstanceProfileResponse = DeleteInstanceProfileResponse'
