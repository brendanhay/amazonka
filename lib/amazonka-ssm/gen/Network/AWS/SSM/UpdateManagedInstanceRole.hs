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
-- Module      : Network.AWS.SSM.UpdateManagedInstanceRole
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Assigns or changes an Amazon Identity and Access Management (IAM) role to the managed instance.
--
--
module Network.AWS.SSM.UpdateManagedInstanceRole
    (
    -- * Creating a Request
      updateManagedInstanceRole
    , UpdateManagedInstanceRole
    -- * Request Lenses
    , umirInstanceId
    , umirIAMRole

    -- * Destructuring the Response
    , updateManagedInstanceRoleResponse
    , UpdateManagedInstanceRoleResponse
    -- * Response Lenses
    , umirrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SSM.Types
import Network.AWS.SSM.Types.Product

-- | /See:/ 'updateManagedInstanceRole' smart constructor.
data UpdateManagedInstanceRole = UpdateManagedInstanceRole'
  { _umirInstanceId :: !Text
  , _umirIAMRole    :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateManagedInstanceRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umirInstanceId' - The ID of the managed instance where you want to update the role.
--
-- * 'umirIAMRole' - The IAM role you want to assign or change.
updateManagedInstanceRole
    :: Text -- ^ 'umirInstanceId'
    -> Text -- ^ 'umirIAMRole'
    -> UpdateManagedInstanceRole
updateManagedInstanceRole pInstanceId_ pIAMRole_ =
  UpdateManagedInstanceRole'
    {_umirInstanceId = pInstanceId_, _umirIAMRole = pIAMRole_}


-- | The ID of the managed instance where you want to update the role.
umirInstanceId :: Lens' UpdateManagedInstanceRole Text
umirInstanceId = lens _umirInstanceId (\ s a -> s{_umirInstanceId = a})

-- | The IAM role you want to assign or change.
umirIAMRole :: Lens' UpdateManagedInstanceRole Text
umirIAMRole = lens _umirIAMRole (\ s a -> s{_umirIAMRole = a})

instance AWSRequest UpdateManagedInstanceRole where
        type Rs UpdateManagedInstanceRole =
             UpdateManagedInstanceRoleResponse
        request = postJSON ssm
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateManagedInstanceRoleResponse' <$>
                   (pure (fromEnum s)))

instance Hashable UpdateManagedInstanceRole where

instance NFData UpdateManagedInstanceRole where

instance ToHeaders UpdateManagedInstanceRole where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AmazonSSM.UpdateManagedInstanceRole" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateManagedInstanceRole where
        toJSON UpdateManagedInstanceRole'{..}
          = object
              (catMaybes
                 [Just ("InstanceId" .= _umirInstanceId),
                  Just ("IamRole" .= _umirIAMRole)])

instance ToPath UpdateManagedInstanceRole where
        toPath = const "/"

instance ToQuery UpdateManagedInstanceRole where
        toQuery = const mempty

-- | /See:/ 'updateManagedInstanceRoleResponse' smart constructor.
newtype UpdateManagedInstanceRoleResponse = UpdateManagedInstanceRoleResponse'
  { _umirrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateManagedInstanceRoleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'umirrsResponseStatus' - -- | The response status code.
updateManagedInstanceRoleResponse
    :: Int -- ^ 'umirrsResponseStatus'
    -> UpdateManagedInstanceRoleResponse
updateManagedInstanceRoleResponse pResponseStatus_ =
  UpdateManagedInstanceRoleResponse' {_umirrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
umirrsResponseStatus :: Lens' UpdateManagedInstanceRoleResponse Int
umirrsResponseStatus = lens _umirrsResponseStatus (\ s a -> s{_umirrsResponseStatus = a})

instance NFData UpdateManagedInstanceRoleResponse
         where
