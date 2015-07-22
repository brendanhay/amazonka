{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.GetRole
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified role, including the role\'s
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
    , grrqRoleName

    -- * Response
    , GetRoleResponse
    -- ** Response constructor
    , getRoleResponse
    -- ** Response lenses
    , grrsStatus
    , grrsRole
    ) where

import           Network.AWS.IAM.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'getRole' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grrqRoleName'
newtype GetRole = GetRole'
    { _grrqRoleName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetRole' smart constructor.
getRole :: Text -> GetRole
getRole pRoleName =
    GetRole'
    { _grrqRoleName = pRoleName
    }

-- | The name of the role to get information about.
grrqRoleName :: Lens' GetRole Text
grrqRoleName = lens _grrqRoleName (\ s a -> s{_grrqRoleName = a});

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
               "RoleName" =: _grrqRoleName]

-- | Contains the response to a successful GetRole request.
--
-- /See:/ 'getRoleResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grrsStatus'
--
-- * 'grrsRole'
data GetRoleResponse = GetRoleResponse'
    { _grrsStatus :: !Int
    , _grrsRole   :: !Role
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetRoleResponse' smart constructor.
getRoleResponse :: Int -> Role -> GetRoleResponse
getRoleResponse pStatus pRole =
    GetRoleResponse'
    { _grrsStatus = pStatus
    , _grrsRole = pRole
    }

-- | FIXME: Undocumented member.
grrsStatus :: Lens' GetRoleResponse Int
grrsStatus = lens _grrsStatus (\ s a -> s{_grrsStatus = a});

-- | Information about the role.
grrsRole :: Lens' GetRoleResponse Role
grrsRole = lens _grrsRole (\ s a -> s{_grrsRole = a});
