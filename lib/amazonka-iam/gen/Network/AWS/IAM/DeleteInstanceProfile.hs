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
-- Module      : Network.AWS.IAM.DeleteInstanceProfile
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified instance profile. The instance profile must not have an associated role.
--
--
-- /Important:/ Make sure that you do not have any Amazon EC2 instances running with the instance profile you are about to delete. Deleting a role or instance profile that is associated with a running instance will break any applications running on the instance.
--
-- For more information about instance profiles, go to <http://docs.aws.amazon.com/IAM/latest/UserGuide/AboutInstanceProfiles.html About Instance Profiles> .
--
module Network.AWS.IAM.DeleteInstanceProfile
    (
    -- * Creating a Request
      deleteInstanceProfile
    , DeleteInstanceProfile
    -- * Request Lenses
    , dipInstanceProfileName

    -- * Destructuring the Response
    , deleteInstanceProfileResponse
    , DeleteInstanceProfileResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteInstanceProfile' smart constructor.
newtype DeleteInstanceProfile = DeleteInstanceProfile'
  { _dipInstanceProfileName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteInstanceProfile' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dipInstanceProfileName' - The name of the instance profile to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
deleteInstanceProfile
    :: Text -- ^ 'dipInstanceProfileName'
    -> DeleteInstanceProfile
deleteInstanceProfile pInstanceProfileName_ =
  DeleteInstanceProfile' {_dipInstanceProfileName = pInstanceProfileName_}


-- | The name of the instance profile to delete. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
dipInstanceProfileName :: Lens' DeleteInstanceProfile Text
dipInstanceProfileName = lens _dipInstanceProfileName (\ s a -> s{_dipInstanceProfileName = a})

instance AWSRequest DeleteInstanceProfile where
        type Rs DeleteInstanceProfile =
             DeleteInstanceProfileResponse
        request = postQuery iam
        response = receiveNull DeleteInstanceProfileResponse'

instance Hashable DeleteInstanceProfile where

instance NFData DeleteInstanceProfile where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteInstanceProfileResponse' with the minimum fields required to make a request.
--
deleteInstanceProfileResponse
    :: DeleteInstanceProfileResponse
deleteInstanceProfileResponse = DeleteInstanceProfileResponse'


instance NFData DeleteInstanceProfileResponse where
