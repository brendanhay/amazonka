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
-- Module      : Network.AWS.IAM.UpdateRole
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description or maximum session duration setting of a role.
--
--
module Network.AWS.IAM.UpdateRole
    (
    -- * Creating a Request
      updateRole
    , UpdateRole
    -- * Request Lenses
    , urMaxSessionDuration
    , urDescription
    , urRoleName

    -- * Destructuring the Response
    , updateRoleResponse
    , UpdateRoleResponse
    -- * Response Lenses
    , urrsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateRole' smart constructor.
data UpdateRole = UpdateRole'
  { _urMaxSessionDuration :: !(Maybe Nat)
  , _urDescription        :: !(Maybe Text)
  , _urRoleName           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urMaxSessionDuration' - The maximum session duration (in seconds) that you want to set for the specified role. If you do not specify a value for this setting, the default maximum of one hour is applied. This setting can have a value from 1 hour to 12 hours. Anyone who assumes the role from the AWS CLI or API can use the @DurationSeconds@ API parameter or the @duration-seconds@ CLI parameter to request a longer session. The @MaxSessionDuration@ setting determines the maximum duration that can be requested using the @DurationSeconds@ parameter. If users don't specify a value for the @DurationSeconds@ parameter, their security credentials are valid for one hour by default. This applies when you use the @AssumeRole*@ API operations or the @assume-role*@ CLI operations but does not apply when you use those operations to create a console URL. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles> in the /IAM User Guide/ .
--
-- * 'urDescription' - The new description that you want to apply to the specified role.
--
-- * 'urRoleName' - The name of the role that you want to modify.
updateRole
    :: Text -- ^ 'urRoleName'
    -> UpdateRole
updateRole pRoleName_ =
  UpdateRole'
    { _urMaxSessionDuration = Nothing
    , _urDescription = Nothing
    , _urRoleName = pRoleName_
    }


-- | The maximum session duration (in seconds) that you want to set for the specified role. If you do not specify a value for this setting, the default maximum of one hour is applied. This setting can have a value from 1 hour to 12 hours. Anyone who assumes the role from the AWS CLI or API can use the @DurationSeconds@ API parameter or the @duration-seconds@ CLI parameter to request a longer session. The @MaxSessionDuration@ setting determines the maximum duration that can be requested using the @DurationSeconds@ parameter. If users don't specify a value for the @DurationSeconds@ parameter, their security credentials are valid for one hour by default. This applies when you use the @AssumeRole*@ API operations or the @assume-role*@ CLI operations but does not apply when you use those operations to create a console URL. For more information, see <http://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use.html Using IAM Roles> in the /IAM User Guide/ .
urMaxSessionDuration :: Lens' UpdateRole (Maybe Natural)
urMaxSessionDuration = lens _urMaxSessionDuration (\ s a -> s{_urMaxSessionDuration = a}) . mapping _Nat

-- | The new description that you want to apply to the specified role.
urDescription :: Lens' UpdateRole (Maybe Text)
urDescription = lens _urDescription (\ s a -> s{_urDescription = a})

-- | The name of the role that you want to modify.
urRoleName :: Lens' UpdateRole Text
urRoleName = lens _urRoleName (\ s a -> s{_urRoleName = a})

instance AWSRequest UpdateRole where
        type Rs UpdateRole = UpdateRoleResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "UpdateRoleResult"
              (\ s h x ->
                 UpdateRoleResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateRole where

instance NFData UpdateRole where

instance ToHeaders UpdateRole where
        toHeaders = const mempty

instance ToPath UpdateRole where
        toPath = const "/"

instance ToQuery UpdateRole where
        toQuery UpdateRole'{..}
          = mconcat
              ["Action" =: ("UpdateRole" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "MaxSessionDuration" =: _urMaxSessionDuration,
               "Description" =: _urDescription,
               "RoleName" =: _urRoleName]

-- | /See:/ 'updateRoleResponse' smart constructor.
newtype UpdateRoleResponse = UpdateRoleResponse'
  { _urrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRoleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urrsResponseStatus' - -- | The response status code.
updateRoleResponse
    :: Int -- ^ 'urrsResponseStatus'
    -> UpdateRoleResponse
updateRoleResponse pResponseStatus_ =
  UpdateRoleResponse' {_urrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
urrsResponseStatus :: Lens' UpdateRoleResponse Int
urrsResponseStatus = lens _urrsResponseStatus (\ s a -> s{_urrsResponseStatus = a})

instance NFData UpdateRoleResponse where
