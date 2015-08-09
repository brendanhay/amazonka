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
-- Module      : Network.AWS.IAM.GetRole
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified role, including the role\'s
-- path, GUID, ARN, and the policy granting permission to assume the role.
-- For more information about ARNs, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html#Identifiers_ARNs ARNs>.
-- For more information about roles, go to
-- <http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles>.
--
-- /See:/ <http://docs.aws.amazon.com/IAM/latest/APIReference/API_GetRole.html AWS API Reference> for GetRole.
module Network.AWS.IAM.GetRole
    (
    -- * Creating a Request
      GetRole
    , getRole
    -- * Request Lenses
    , grRoleName

    -- * Destructuring the Response
    , GetRoleResponse
    , getRoleResponse
    -- * Response Lenses
    , grrsStatus
    , grrsRole
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRole' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grRoleName'
newtype GetRole = GetRole'
    { _grRoleName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetRole' smart constructor.
getRole :: Text -> GetRole
getRole pRoleName_ = 
    GetRole'
    { _grRoleName = pRoleName_
    }

-- | The name of the role to get information about.
grRoleName :: Lens' GetRole Text
grRoleName = lens _grRoleName (\ s a -> s{_grRoleName = a});

instance AWSRequest GetRole where
        type Sv GetRole = IAM
        type Rs GetRole = GetRoleResponse
        request = postQuery
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
-- * 'grrsStatus'
--
-- * 'grrsRole'
data GetRoleResponse = GetRoleResponse'
    { _grrsStatus :: !Int
    , _grrsRole :: !Role
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetRoleResponse' smart constructor.
getRoleResponse :: Int -> Role -> GetRoleResponse
getRoleResponse pStatus_ pRole_ = 
    GetRoleResponse'
    { _grrsStatus = pStatus_
    , _grrsRole = pRole_
    }

-- | Undocumented member.
grrsStatus :: Lens' GetRoleResponse Int
grrsStatus = lens _grrsStatus (\ s a -> s{_grrsStatus = a});

-- | Information about the role.
grrsRole :: Lens' GetRoleResponse Role
grrsRole = lens _grrsRole (\ s a -> s{_grrsRole = a});
