{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.OpsWorks.SetPermission
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Specifies a user's permissions. For more information, see Security and
-- Permissions. Required Permissions: To use this action, an IAM user must
-- have a Manage permissions level for the stack, or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see Managing User Permissions.
module Network.AWS.OpsWorks.SetPermission
    (
    -- * Request
      SetPermission
    -- ** Request constructor
    , setPermission
    -- ** Request lenses
    , spStackId
    , spIamUserArn
    , spAllowSsh
    , spAllowSudo
    , spLevel

    -- * Response
    , SetPermissionResponse
    -- ** Response constructor
    , setPermissionResponse
    ) where

import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

data SetPermission = SetPermission
    { _spStackId :: Text
    , _spIamUserArn :: Text
    , _spAllowSsh :: Maybe Bool
    , _spAllowSudo :: Maybe Bool
    , _spLevel :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetPermission' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @StackId ::@ @Text@
--
-- * @IamUserArn ::@ @Text@
--
-- * @AllowSsh ::@ @Maybe Bool@
--
-- * @AllowSudo ::@ @Maybe Bool@
--
-- * @Level ::@ @Maybe Text@
--
setPermission :: Text -- ^ 'spStackId'
              -> Text -- ^ 'spIamUserArn'
              -> SetPermission
setPermission p1 p2 = SetPermission
    { _spStackId = p1
    , _spIamUserArn = p2
    , _spAllowSsh = Nothing
    , _spAllowSudo = Nothing
    , _spLevel = Nothing
    }

-- | The stack ID.
spStackId :: Lens' SetPermission Text
spStackId = lens _spStackId (\s a -> s { _spStackId = a })

-- | The user's IAM ARN.
spIamUserArn :: Lens' SetPermission Text
spIamUserArn = lens _spIamUserArn (\s a -> s { _spIamUserArn = a })

-- | The user is allowed to use SSH to communicate with the instance.
spAllowSsh :: Lens' SetPermission (Maybe Bool)
spAllowSsh = lens _spAllowSsh (\s a -> s { _spAllowSsh = a })

-- | The user is allowed to use sudo to elevate privileges.
spAllowSudo :: Lens' SetPermission (Maybe Bool)
spAllowSudo = lens _spAllowSudo (\s a -> s { _spAllowSudo = a })

-- | The user's permission level, which must be set to one of the following
-- strings. You cannot set your own permissions level. deny show deploy manage
-- iam_only For more information on the permissions associated with these
-- levels, see Managing User Permissions.
spLevel :: Lens' SetPermission (Maybe Text)
spLevel = lens _spLevel (\s a -> s { _spLevel = a })

instance ToPath SetPermission

instance ToQuery SetPermission

instance ToHeaders SetPermission

instance ToJSON SetPermission

data SetPermissionResponse = SetPermissionResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'SetPermissionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
setPermissionResponse :: SetPermissionResponse
setPermissionResponse = SetPermissionResponse

instance AWSRequest SetPermission where
    type Sv SetPermission = OpsWorks
    type Rs SetPermission = SetPermissionResponse

    request = get
    response _ = nullaryResponse SetPermissionResponse
