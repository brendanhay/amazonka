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
-- Module      : Network.AWS.IAM.DeleteUserPermissionsBoundary
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the permissions boundary for the specified IAM user.
--
--
-- /Important:/ Deleting the permissions boundary for a user might increase its permissions by allowing the user to perform all the actions granted in its permissions policies.
--
module Network.AWS.IAM.DeleteUserPermissionsBoundary
    (
    -- * Creating a Request
      deleteUserPermissionsBoundary
    , DeleteUserPermissionsBoundary
    -- * Request Lenses
    , dupbUserName

    -- * Destructuring the Response
    , deleteUserPermissionsBoundaryResponse
    , DeleteUserPermissionsBoundaryResponse
    ) where

import Network.AWS.IAM.Types
import Network.AWS.IAM.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUserPermissionsBoundary' smart constructor.
newtype DeleteUserPermissionsBoundary = DeleteUserPermissionsBoundary'
  { _dupbUserName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserPermissionsBoundary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dupbUserName' - The name (friendly name, not ARN) of the IAM user from which you want to remove the permissions boundary.
deleteUserPermissionsBoundary
    :: Text -- ^ 'dupbUserName'
    -> DeleteUserPermissionsBoundary
deleteUserPermissionsBoundary pUserName_ =
  DeleteUserPermissionsBoundary' {_dupbUserName = pUserName_}


-- | The name (friendly name, not ARN) of the IAM user from which you want to remove the permissions boundary.
dupbUserName :: Lens' DeleteUserPermissionsBoundary Text
dupbUserName = lens _dupbUserName (\ s a -> s{_dupbUserName = a})

instance AWSRequest DeleteUserPermissionsBoundary
         where
        type Rs DeleteUserPermissionsBoundary =
             DeleteUserPermissionsBoundaryResponse
        request = postQuery iam
        response
          = receiveNull DeleteUserPermissionsBoundaryResponse'

instance Hashable DeleteUserPermissionsBoundary where

instance NFData DeleteUserPermissionsBoundary where

instance ToHeaders DeleteUserPermissionsBoundary
         where
        toHeaders = const mempty

instance ToPath DeleteUserPermissionsBoundary where
        toPath = const "/"

instance ToQuery DeleteUserPermissionsBoundary where
        toQuery DeleteUserPermissionsBoundary'{..}
          = mconcat
              ["Action" =:
                 ("DeleteUserPermissionsBoundary" :: ByteString),
               "Version" =: ("2010-05-08" :: ByteString),
               "UserName" =: _dupbUserName]

-- | /See:/ 'deleteUserPermissionsBoundaryResponse' smart constructor.
data DeleteUserPermissionsBoundaryResponse =
  DeleteUserPermissionsBoundaryResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteUserPermissionsBoundaryResponse' with the minimum fields required to make a request.
--
deleteUserPermissionsBoundaryResponse
    :: DeleteUserPermissionsBoundaryResponse
deleteUserPermissionsBoundaryResponse = DeleteUserPermissionsBoundaryResponse'


instance NFData DeleteUserPermissionsBoundaryResponse
         where
