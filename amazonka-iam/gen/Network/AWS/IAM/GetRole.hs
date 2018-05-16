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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about the specified role, including the role's path, GUID, ARN, and the role's trust policy that grants permission to assume the role. For more information about roles, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/WorkingWithRoles.html Working with Roles> .
--
--
module Network.AWS.IAM.GetRole
    (
    -- * Creating a Request
      getRole
    , GetRole
    -- * Request Lenses
    , grRoleName

    -- * Destructuring the Response
    , getRoleResponse
    , GetRoleResponse
    -- * Response Lenses
    , grrsResponseStatus
    , grrsRole
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'getRole' smart constructor.
newtype GetRole = GetRole'
  { _grRoleName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grRoleName' - The name of the IAM role to get information about. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
getRole
    :: Text -- ^ 'grRoleName'
    -> GetRole
getRole pRoleName_ = GetRole' {_grRoleName = pRoleName_}


-- | The name of the IAM role to get information about. This parameter allows (per its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
grRoleName :: Lens' GetRole Text
grRoleName = lens _grRoleName (\ s a -> s{_grRoleName = a})

instance AWSRequest GetRole where
        type Rs GetRole = GetRoleResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "GetRoleResult"
              (\ s h x ->
                 GetRoleResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "Role"))

instance Hashable GetRole where

instance NFData GetRole where

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

-- | Contains the response to a successful 'GetRole' request.
--
--
--
-- /See:/ 'getRoleResponse' smart constructor.
data GetRoleResponse = GetRoleResponse'
  { _grrsResponseStatus :: !Int
  , _grrsRole           :: !Role
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'GetRoleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'grrsResponseStatus' - -- | The response status code.
--
-- * 'grrsRole' - A structure containing details about the IAM role.
getRoleResponse
    :: Int -- ^ 'grrsResponseStatus'
    -> Role -- ^ 'grrsRole'
    -> GetRoleResponse
getRoleResponse pResponseStatus_ pRole_ =
  GetRoleResponse' {_grrsResponseStatus = pResponseStatus_, _grrsRole = pRole_}


-- | -- | The response status code.
grrsResponseStatus :: Lens' GetRoleResponse Int
grrsResponseStatus = lens _grrsResponseStatus (\ s a -> s{_grrsResponseStatus = a})

-- | A structure containing details about the IAM role.
grrsRole :: Lens' GetRoleResponse Role
grrsRole = lens _grrsRole (\ s a -> s{_grrsRole = a})

instance NFData GetRoleResponse where
