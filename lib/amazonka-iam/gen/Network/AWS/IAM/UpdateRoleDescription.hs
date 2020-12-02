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
-- Module      : Network.AWS.IAM.UpdateRoleDescription
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use instead.
--
--
-- Modifies only the description of a role. This operation performs the same function as the @Description@ parameter in the @UpdateRole@ operation.
--
module Network.AWS.IAM.UpdateRoleDescription
    (
    -- * Creating a Request
      updateRoleDescription
    , UpdateRoleDescription
    -- * Request Lenses
    , urdRoleName
    , urdDescription

    -- * Destructuring the Response
    , updateRoleDescriptionResponse
    , UpdateRoleDescriptionResponse
    -- * Response Lenses
    , urdrsRole
    , urdrsResponseStatus
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateRoleDescription' smart constructor.
data UpdateRoleDescription = UpdateRoleDescription'
  { _urdRoleName    :: !Text
  , _urdDescription :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRoleDescription' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urdRoleName' - The name of the role that you want to modify.
--
-- * 'urdDescription' - The new description that you want to apply to the specified role.
updateRoleDescription
    :: Text -- ^ 'urdRoleName'
    -> Text -- ^ 'urdDescription'
    -> UpdateRoleDescription
updateRoleDescription pRoleName_ pDescription_ =
  UpdateRoleDescription'
    {_urdRoleName = pRoleName_, _urdDescription = pDescription_}


-- | The name of the role that you want to modify.
urdRoleName :: Lens' UpdateRoleDescription Text
urdRoleName = lens _urdRoleName (\ s a -> s{_urdRoleName = a})

-- | The new description that you want to apply to the specified role.
urdDescription :: Lens' UpdateRoleDescription Text
urdDescription = lens _urdDescription (\ s a -> s{_urdDescription = a})

instance AWSRequest UpdateRoleDescription where
        type Rs UpdateRoleDescription =
             UpdateRoleDescriptionResponse
        request = postQuery iam
        response
          = receiveXMLWrapper "UpdateRoleDescriptionResult"
              (\ s h x ->
                 UpdateRoleDescriptionResponse' <$>
                   (x .@? "Role") <*> (pure (fromEnum s)))

instance Hashable UpdateRoleDescription where

instance NFData UpdateRoleDescription where

instance ToHeaders UpdateRoleDescription where
        toHeaders = const mempty

instance ToPath UpdateRoleDescription where
        toPath = const "/"

instance ToQuery UpdateRoleDescription where
        toQuery UpdateRoleDescription'{..}
          = mconcat
              ["Action" =: ("UpdateRoleDescription" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "RoleName" =: _urdRoleName,
               "Description" =: _urdDescription]

-- | /See:/ 'updateRoleDescriptionResponse' smart constructor.
data UpdateRoleDescriptionResponse = UpdateRoleDescriptionResponse'
  { _urdrsRole           :: !(Maybe Role)
  , _urdrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateRoleDescriptionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'urdrsRole' - A structure that contains details about the modified role.
--
-- * 'urdrsResponseStatus' - -- | The response status code.
updateRoleDescriptionResponse
    :: Int -- ^ 'urdrsResponseStatus'
    -> UpdateRoleDescriptionResponse
updateRoleDescriptionResponse pResponseStatus_ =
  UpdateRoleDescriptionResponse'
    {_urdrsRole = Nothing, _urdrsResponseStatus = pResponseStatus_}


-- | A structure that contains details about the modified role.
urdrsRole :: Lens' UpdateRoleDescriptionResponse (Maybe Role)
urdrsRole = lens _urdrsRole (\ s a -> s{_urdrsRole = a})

-- | -- | The response status code.
urdrsResponseStatus :: Lens' UpdateRoleDescriptionResponse Int
urdrsResponseStatus = lens _urdrsResponseStatus (\ s a -> s{_urdrsResponseStatus = a})

instance NFData UpdateRoleDescriptionResponse where
