{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.IAM.GetRole
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Retrieves information about the specified role, including the role\'s
-- path, GUID, ARN, and the policy granting permission to assume the role.
-- For more information about ARNs, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html#Identifiers_ARNs ARNs>.
-- For more information about roles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles>.
--
-- <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetRole.html>
module Network.AWS.IAM.GetRole
    (
    -- * Request
      GetRole
    -- ** Request constructor
    , getRole
    -- ** Request lenses
    , grRoleName

    -- * Response
    , GetRoleResponse
    -- ** Response constructor
    , getRoleResponse
    -- ** Response lenses
    , grrStatus
    , grrRole
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getRole' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grRoleName'
newtype GetRole = GetRole'
    { _grRoleName :: Text
    } deriving (Eq,Read,Show)

-- | 'GetRole' smart constructor.
getRole :: Text -> GetRole
getRole pRoleName =
    GetRole'
    { _grRoleName = pRoleName
    }

-- | The name of the role to get information about.
grRoleName :: Lens' GetRole Text
grRoleName = lens _grRoleName (\ s a -> s{_grRoleName = a});

instance AWSRequest GetRole where
        type Sv GetRole = IAM
        type Rs GetRole = GetRoleResponse
        request = post
        response
          = receiveXMLWrapper "GetRoleResult"
              (\ s h x ->
                 GetRoleResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Role"))

instance ToHeaders GetRole where
        toHeaders = const mempty

instance ToPath GetRole where
        toPath = const "/"

instance ToQuery GetRole where
        toQuery GetRole'{..}
          = mconcat
              ["Action" =: ("GetRole" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RoleName" =: _grRoleName]

-- | Contains the response to a successful GetRole request.
--
-- /See:/ 'getRoleResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grrStatus'
--
-- * 'grrRole'
data GetRoleResponse = GetRoleResponse'
    { _grrStatus :: !Int
    , _grrRole   :: !Role
    } deriving (Eq,Read,Show)

-- | 'GetRoleResponse' smart constructor.
getRoleResponse :: Int -> Role -> GetRoleResponse
getRoleResponse pStatus pRole =
    GetRoleResponse'
    { _grrStatus = pStatus
    , _grrRole = pRole
    }

-- | FIXME: Undocumented member.
grrStatus :: Lens' GetRoleResponse Int
grrStatus = lens _grrStatus (\ s a -> s{_grrStatus = a});

-- | Information about the role.
grrRole :: Lens' GetRoleResponse Role
grrRole = lens _grrRole (\ s a -> s{_grrRole = a});
