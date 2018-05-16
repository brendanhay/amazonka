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
-- Module      : Network.AWS.OpsWorks.SetPermission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Specifies a user's permissions. For more information, see <http://docs.aws.amazon.com/opsworks/latest/userguide/workingsecurity.html Security and Permissions> .
--
--
-- __Required Permissions__ : To use this action, an IAM user must have a Manage permissions level for the stack, or an attached policy that explicitly grants permissions. For more information on user permissions, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
module Network.AWS.OpsWorks.SetPermission
    (
    -- * Creating a Request
      setPermission
    , SetPermission
    -- * Request Lenses
    , spAllowSudo
    , spLevel
    , spAllowSSH
    , spStackId
    , spIAMUserARN

    -- * Destructuring the Response
    , setPermissionResponse
    , SetPermissionResponse
    ) where

import Network.AWS.Lens
import Network.AWS.OpsWorks.Types
import Network.AWS.OpsWorks.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'setPermission' smart constructor.
data SetPermission = SetPermission'
  { _spAllowSudo  :: !(Maybe Bool)
  , _spLevel      :: !(Maybe Text)
  , _spAllowSSH   :: !(Maybe Bool)
  , _spStackId    :: !Text
  , _spIAMUserARN :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetPermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'spAllowSudo' - The user is allowed to use __sudo__ to elevate privileges.
--
-- * 'spLevel' - The user's permission level, which must be set to one of the following strings. You cannot set your own permissions level.     * @deny@      * @show@      * @deploy@      * @manage@      * @iam_only@  For more information on the permissions associated with these levels, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
--
-- * 'spAllowSSH' - The user is allowed to use SSH to communicate with the instance.
--
-- * 'spStackId' - The stack ID.
--
-- * 'spIAMUserARN' - The user's IAM ARN. This can also be a federated user's ARN.
setPermission
    :: Text -- ^ 'spStackId'
    -> Text -- ^ 'spIAMUserARN'
    -> SetPermission
setPermission pStackId_ pIAMUserARN_ =
  SetPermission'
    { _spAllowSudo = Nothing
    , _spLevel = Nothing
    , _spAllowSSH = Nothing
    , _spStackId = pStackId_
    , _spIAMUserARN = pIAMUserARN_
    }


-- | The user is allowed to use __sudo__ to elevate privileges.
spAllowSudo :: Lens' SetPermission (Maybe Bool)
spAllowSudo = lens _spAllowSudo (\ s a -> s{_spAllowSudo = a})

-- | The user's permission level, which must be set to one of the following strings. You cannot set your own permissions level.     * @deny@      * @show@      * @deploy@      * @manage@      * @iam_only@  For more information on the permissions associated with these levels, see <http://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions> .
spLevel :: Lens' SetPermission (Maybe Text)
spLevel = lens _spLevel (\ s a -> s{_spLevel = a})

-- | The user is allowed to use SSH to communicate with the instance.
spAllowSSH :: Lens' SetPermission (Maybe Bool)
spAllowSSH = lens _spAllowSSH (\ s a -> s{_spAllowSSH = a})

-- | The stack ID.
spStackId :: Lens' SetPermission Text
spStackId = lens _spStackId (\ s a -> s{_spStackId = a})

-- | The user's IAM ARN. This can also be a federated user's ARN.
spIAMUserARN :: Lens' SetPermission Text
spIAMUserARN = lens _spIAMUserARN (\ s a -> s{_spIAMUserARN = a})

instance AWSRequest SetPermission where
        type Rs SetPermission = SetPermissionResponse
        request = postJSON opsWorks
        response = receiveNull SetPermissionResponse'

instance Hashable SetPermission where

instance NFData SetPermission where

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
              (catMaybes
                 [("AllowSudo" .=) <$> _spAllowSudo,
                  ("Level" .=) <$> _spLevel,
                  ("AllowSsh" .=) <$> _spAllowSSH,
                  Just ("StackId" .= _spStackId),
                  Just ("IamUserArn" .= _spIAMUserARN)])

instance ToPath SetPermission where
        toPath = const "/"

instance ToQuery SetPermission where
        toQuery = const mempty

-- | /See:/ 'setPermissionResponse' smart constructor.
data SetPermissionResponse =
  SetPermissionResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'SetPermissionResponse' with the minimum fields required to make a request.
--
setPermissionResponse
    :: SetPermissionResponse
setPermissionResponse = SetPermissionResponse'


instance NFData SetPermissionResponse where
