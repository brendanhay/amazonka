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
-- Module      : Network.AWS.EC2.DeleteNetworkInterfacePermission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a permission for a network interface. By default, you cannot delete the permission if the account for which you're removing the permission has attached the network interface to an instance. However, you can force delete the permission, regardless of any attachment.
--
--
module Network.AWS.EC2.DeleteNetworkInterfacePermission
    (
    -- * Creating a Request
      deleteNetworkInterfacePermission
    , DeleteNetworkInterfacePermission
    -- * Request Lenses
    , dnipForce
    , dnipDryRun
    , dnipNetworkInterfacePermissionId

    -- * Destructuring the Response
    , deleteNetworkInterfacePermissionResponse
    , DeleteNetworkInterfacePermissionResponse
    -- * Response Lenses
    , dniprsReturn
    , dniprsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DeleteNetworkInterfacePermission.
--
--
--
-- /See:/ 'deleteNetworkInterfacePermission' smart constructor.
data DeleteNetworkInterfacePermission = DeleteNetworkInterfacePermission'
  { _dnipForce                        :: !(Maybe Bool)
  , _dnipDryRun                       :: !(Maybe Bool)
  , _dnipNetworkInterfacePermissionId :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNetworkInterfacePermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dnipForce' - Specify @true@ to remove the permission even if the network interface is attached to an instance.
--
-- * 'dnipDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dnipNetworkInterfacePermissionId' - The ID of the network interface permission.
deleteNetworkInterfacePermission
    :: Text -- ^ 'dnipNetworkInterfacePermissionId'
    -> DeleteNetworkInterfacePermission
deleteNetworkInterfacePermission pNetworkInterfacePermissionId_ =
  DeleteNetworkInterfacePermission'
    { _dnipForce = Nothing
    , _dnipDryRun = Nothing
    , _dnipNetworkInterfacePermissionId = pNetworkInterfacePermissionId_
    }


-- | Specify @true@ to remove the permission even if the network interface is attached to an instance.
dnipForce :: Lens' DeleteNetworkInterfacePermission (Maybe Bool)
dnipForce = lens _dnipForce (\ s a -> s{_dnipForce = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dnipDryRun :: Lens' DeleteNetworkInterfacePermission (Maybe Bool)
dnipDryRun = lens _dnipDryRun (\ s a -> s{_dnipDryRun = a})

-- | The ID of the network interface permission.
dnipNetworkInterfacePermissionId :: Lens' DeleteNetworkInterfacePermission Text
dnipNetworkInterfacePermissionId = lens _dnipNetworkInterfacePermissionId (\ s a -> s{_dnipNetworkInterfacePermissionId = a})

instance AWSRequest DeleteNetworkInterfacePermission
         where
        type Rs DeleteNetworkInterfacePermission =
             DeleteNetworkInterfacePermissionResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DeleteNetworkInterfacePermissionResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable DeleteNetworkInterfacePermission
         where

instance NFData DeleteNetworkInterfacePermission
         where

instance ToHeaders DeleteNetworkInterfacePermission
         where
        toHeaders = const mempty

instance ToPath DeleteNetworkInterfacePermission
         where
        toPath = const "/"

instance ToQuery DeleteNetworkInterfacePermission
         where
        toQuery DeleteNetworkInterfacePermission'{..}
          = mconcat
              ["Action" =:
                 ("DeleteNetworkInterfacePermission" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "Force" =: _dnipForce, "DryRun" =: _dnipDryRun,
               "NetworkInterfacePermissionId" =:
                 _dnipNetworkInterfacePermissionId]

-- | Contains the output for DeleteNetworkInterfacePermission.
--
--
--
-- /See:/ 'deleteNetworkInterfacePermissionResponse' smart constructor.
data DeleteNetworkInterfacePermissionResponse = DeleteNetworkInterfacePermissionResponse'
  { _dniprsReturn         :: !(Maybe Bool)
  , _dniprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteNetworkInterfacePermissionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dniprsReturn' - Returns @true@ if the request succeeds, otherwise returns an error.
--
-- * 'dniprsResponseStatus' - -- | The response status code.
deleteNetworkInterfacePermissionResponse
    :: Int -- ^ 'dniprsResponseStatus'
    -> DeleteNetworkInterfacePermissionResponse
deleteNetworkInterfacePermissionResponse pResponseStatus_ =
  DeleteNetworkInterfacePermissionResponse'
    {_dniprsReturn = Nothing, _dniprsResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds, otherwise returns an error.
dniprsReturn :: Lens' DeleteNetworkInterfacePermissionResponse (Maybe Bool)
dniprsReturn = lens _dniprsReturn (\ s a -> s{_dniprsReturn = a})

-- | -- | The response status code.
dniprsResponseStatus :: Lens' DeleteNetworkInterfacePermissionResponse Int
dniprsResponseStatus = lens _dniprsResponseStatus (\ s a -> s{_dniprsResponseStatus = a})

instance NFData
           DeleteNetworkInterfacePermissionResponse
         where
