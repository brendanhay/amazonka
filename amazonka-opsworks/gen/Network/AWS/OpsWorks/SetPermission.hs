{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.SetPermission
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Specifies a user\'s permissions. For more information, see
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
    , sprqAllowSudo
    , sprqLevel
    , sprqAllowSSH
    , sprqStackId
    , sprqIAMUserARN

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
-- * 'sprqAllowSudo'
--
-- * 'sprqLevel'
--
-- * 'sprqAllowSSH'
--
-- * 'sprqStackId'
--
-- * 'sprqIAMUserARN'
data SetPermission = SetPermission'
    { _sprqAllowSudo  :: !(Maybe Bool)
    , _sprqLevel      :: !(Maybe Text)
    , _sprqAllowSSH   :: !(Maybe Bool)
    , _sprqStackId    :: !Text
    , _sprqIAMUserARN :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetPermission' smart constructor.
setPermission :: Text -> Text -> SetPermission
setPermission pStackId_ pIAMUserARN_ =
    SetPermission'
    { _sprqAllowSudo = Nothing
    , _sprqLevel = Nothing
    , _sprqAllowSSH = Nothing
    , _sprqStackId = pStackId_
    , _sprqIAMUserARN = pIAMUserARN_
    }

-- | The user is allowed to use __sudo__ to elevate privileges.
sprqAllowSudo :: Lens' SetPermission (Maybe Bool)
sprqAllowSudo = lens _sprqAllowSudo (\ s a -> s{_sprqAllowSudo = a});

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
sprqLevel :: Lens' SetPermission (Maybe Text)
sprqLevel = lens _sprqLevel (\ s a -> s{_sprqLevel = a});

-- | The user is allowed to use SSH to communicate with the instance.
sprqAllowSSH :: Lens' SetPermission (Maybe Bool)
sprqAllowSSH = lens _sprqAllowSSH (\ s a -> s{_sprqAllowSSH = a});

-- | The stack ID.
sprqStackId :: Lens' SetPermission Text
sprqStackId = lens _sprqStackId (\ s a -> s{_sprqStackId = a});

-- | The user\'s IAM ARN.
sprqIAMUserARN :: Lens' SetPermission Text
sprqIAMUserARN = lens _sprqIAMUserARN (\ s a -> s{_sprqIAMUserARN = a});

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
              ["AllowSudo" .= _sprqAllowSudo,
               "Level" .= _sprqLevel, "AllowSsh" .= _sprqAllowSSH,
               "StackId" .= _sprqStackId,
               "IamUserArn" .= _sprqIAMUserARN]

instance ToPath SetPermission where
        toPath = const "/"

instance ToQuery SetPermission where
        toQuery = const mempty

-- | /See:/ 'setPermissionResponse' smart constructor.
data SetPermissionResponse =
    SetPermissionResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'SetPermissionResponse' smart constructor.
setPermissionResponse :: SetPermissionResponse
setPermissionResponse = SetPermissionResponse'
