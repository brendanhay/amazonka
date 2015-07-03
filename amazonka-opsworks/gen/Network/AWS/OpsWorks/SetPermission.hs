{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.OpsWorks.SetPermission
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

-- | Specifies a user\'s permissions. For more information, see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/workingsecurity.html Security and Permissions>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- <http://docs.aws.amazon.com/opsworks/latest/APIReference/API_SetPermission.html>
module Network.AWS.OpsWorks.SetPermission
    (
    -- * Request
      SetPermission
    -- ** Request constructor
    , setPermission
    -- ** Request lenses
    , spAllowSudo
    , spLevel
    , spAllowSSH
    , spStackId
    , spIAMUserARN

    -- * Response
    , SetPermissionResponse
    -- ** Response constructor
    , setPermissionResponse
    ) where

import           Network.AWS.OpsWorks.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'setPermission' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'spAllowSudo'
--
-- * 'spLevel'
--
-- * 'spAllowSSH'
--
-- * 'spStackId'
--
-- * 'spIAMUserARN'
data SetPermission = SetPermission'
    { _spAllowSudo  :: !(Maybe Bool)
    , _spLevel      :: !(Maybe Text)
    , _spAllowSSH   :: !(Maybe Bool)
    , _spStackId    :: !Text
    , _spIAMUserARN :: !Text
    } deriving (Eq,Read,Show)

-- | 'SetPermission' smart constructor.
setPermission :: Text -> Text -> SetPermission
setPermission pStackId pIAMUserARN =
    SetPermission'
    { _spAllowSudo = Nothing
    , _spLevel = Nothing
    , _spAllowSSH = Nothing
    , _spStackId = pStackId
    , _spIAMUserARN = pIAMUserARN
    }

-- | The user is allowed to use __sudo__ to elevate privileges.
spAllowSudo :: Lens' SetPermission (Maybe Bool)
spAllowSudo = lens _spAllowSudo (\ s a -> s{_spAllowSudo = a});

-- | The user\'s permission level, which must be set to one of the following
-- strings. You cannot set your own permissions level.
--
-- -   @deny@
-- -   @show@
-- -   @deploy@
-- -   @manage@
-- -   @iam_only@
--
-- For more information on the permissions associated with these levels,
-- see
-- <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
spLevel :: Lens' SetPermission (Maybe Text)
spLevel = lens _spLevel (\ s a -> s{_spLevel = a});

-- | The user is allowed to use SSH to communicate with the instance.
spAllowSSH :: Lens' SetPermission (Maybe Bool)
spAllowSSH = lens _spAllowSSH (\ s a -> s{_spAllowSSH = a});

-- | The stack ID.
spStackId :: Lens' SetPermission Text
spStackId = lens _spStackId (\ s a -> s{_spStackId = a});

-- | The user\'s IAM ARN.
spIAMUserARN :: Lens' SetPermission Text
spIAMUserARN = lens _spIAMUserARN (\ s a -> s{_spIAMUserARN = a});

instance AWSRequest SetPermission where
        type Sv SetPermission = OpsWorks
        type Rs SetPermission = SetPermissionResponse
        request = postJSON
        response = receiveNull SetPermissionResponse'

instance ToHeaders SetPermission where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OpsWorks_20130218.SetPermission" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON SetPermission where
        toJSON SetPermission'{..}
          = object
              ["AllowSudo" .= _spAllowSudo, "Level" .= _spLevel,
               "AllowSsh" .= _spAllowSSH, "StackId" .= _spStackId,
               "IamUserArn" .= _spIAMUserARN]

instance ToPath SetPermission where
        toPath = const "/"

instance ToQuery SetPermission where
        toQuery = const mempty

-- | /See:/ 'setPermissionResponse' smart constructor.
data SetPermissionResponse =
    SetPermissionResponse'
    deriving (Eq,Read,Show)

-- | 'SetPermissionResponse' smart constructor.
setPermissionResponse :: SetPermissionResponse
setPermissionResponse = SetPermissionResponse'
