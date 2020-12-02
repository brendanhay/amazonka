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
-- Module      : Network.AWS.EC2.ModifyVPCEndpointServicePermissions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the permissions for your <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/endpoint-service.html VPC endpoint service> . You can add or remove permissions for service consumers (IAM users, IAM roles, and AWS accounts) to connect to your endpoint service.
--
--
module Network.AWS.EC2.ModifyVPCEndpointServicePermissions
    (
    -- * Creating a Request
      modifyVPCEndpointServicePermissions
    , ModifyVPCEndpointServicePermissions
    -- * Request Lenses
    , mvespRemoveAllowedPrincipals
    , mvespAddAllowedPrincipals
    , mvespDryRun
    , mvespServiceId

    -- * Destructuring the Response
    , modifyVPCEndpointServicePermissionsResponse
    , ModifyVPCEndpointServicePermissionsResponse
    -- * Response Lenses
    , mvesprsReturnValue
    , mvesprsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyVPCEndpointServicePermissions' smart constructor.
data ModifyVPCEndpointServicePermissions = ModifyVPCEndpointServicePermissions'
  { _mvespRemoveAllowedPrincipals :: !(Maybe [Text])
  , _mvespAddAllowedPrincipals    :: !(Maybe [Text])
  , _mvespDryRun                  :: !(Maybe Bool)
  , _mvespServiceId               :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyVPCEndpointServicePermissions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvespRemoveAllowedPrincipals' - One or more Amazon Resource Names (ARNs) of principals for which to remove permission.
--
-- * 'mvespAddAllowedPrincipals' - One or more Amazon Resource Names (ARNs) of principals for which to allow permission. Specify @*@ to allow all principals.
--
-- * 'mvespDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'mvespServiceId' - The ID of the service.
modifyVPCEndpointServicePermissions
    :: Text -- ^ 'mvespServiceId'
    -> ModifyVPCEndpointServicePermissions
modifyVPCEndpointServicePermissions pServiceId_ =
  ModifyVPCEndpointServicePermissions'
    { _mvespRemoveAllowedPrincipals = Nothing
    , _mvespAddAllowedPrincipals = Nothing
    , _mvespDryRun = Nothing
    , _mvespServiceId = pServiceId_
    }


-- | One or more Amazon Resource Names (ARNs) of principals for which to remove permission.
mvespRemoveAllowedPrincipals :: Lens' ModifyVPCEndpointServicePermissions [Text]
mvespRemoveAllowedPrincipals = lens _mvespRemoveAllowedPrincipals (\ s a -> s{_mvespRemoveAllowedPrincipals = a}) . _Default . _Coerce

-- | One or more Amazon Resource Names (ARNs) of principals for which to allow permission. Specify @*@ to allow all principals.
mvespAddAllowedPrincipals :: Lens' ModifyVPCEndpointServicePermissions [Text]
mvespAddAllowedPrincipals = lens _mvespAddAllowedPrincipals (\ s a -> s{_mvespAddAllowedPrincipals = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mvespDryRun :: Lens' ModifyVPCEndpointServicePermissions (Maybe Bool)
mvespDryRun = lens _mvespDryRun (\ s a -> s{_mvespDryRun = a})

-- | The ID of the service.
mvespServiceId :: Lens' ModifyVPCEndpointServicePermissions Text
mvespServiceId = lens _mvespServiceId (\ s a -> s{_mvespServiceId = a})

instance AWSRequest
           ModifyVPCEndpointServicePermissions
         where
        type Rs ModifyVPCEndpointServicePermissions =
             ModifyVPCEndpointServicePermissionsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 ModifyVPCEndpointServicePermissionsResponse' <$>
                   (x .@? "return") <*> (pure (fromEnum s)))

instance Hashable ModifyVPCEndpointServicePermissions
         where

instance NFData ModifyVPCEndpointServicePermissions
         where

instance ToHeaders
           ModifyVPCEndpointServicePermissions
         where
        toHeaders = const mempty

instance ToPath ModifyVPCEndpointServicePermissions
         where
        toPath = const "/"

instance ToQuery ModifyVPCEndpointServicePermissions
         where
        toQuery ModifyVPCEndpointServicePermissions'{..}
          = mconcat
              ["Action" =:
                 ("ModifyVpcEndpointServicePermissions" ::
                    ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               toQuery
                 (toQueryList "RemoveAllowedPrincipals" <$>
                    _mvespRemoveAllowedPrincipals),
               toQuery
                 (toQueryList "AddAllowedPrincipals" <$>
                    _mvespAddAllowedPrincipals),
               "DryRun" =: _mvespDryRun,
               "ServiceId" =: _mvespServiceId]

-- | /See:/ 'modifyVPCEndpointServicePermissionsResponse' smart constructor.
data ModifyVPCEndpointServicePermissionsResponse = ModifyVPCEndpointServicePermissionsResponse'
  { _mvesprsReturnValue    :: !(Maybe Bool)
  , _mvesprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyVPCEndpointServicePermissionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mvesprsReturnValue' - Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- * 'mvesprsResponseStatus' - -- | The response status code.
modifyVPCEndpointServicePermissionsResponse
    :: Int -- ^ 'mvesprsResponseStatus'
    -> ModifyVPCEndpointServicePermissionsResponse
modifyVPCEndpointServicePermissionsResponse pResponseStatus_ =
  ModifyVPCEndpointServicePermissionsResponse'
    {_mvesprsReturnValue = Nothing, _mvesprsResponseStatus = pResponseStatus_}


-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
mvesprsReturnValue :: Lens' ModifyVPCEndpointServicePermissionsResponse (Maybe Bool)
mvesprsReturnValue = lens _mvesprsReturnValue (\ s a -> s{_mvesprsReturnValue = a})

-- | -- | The response status code.
mvesprsResponseStatus :: Lens' ModifyVPCEndpointServicePermissionsResponse Int
mvesprsResponseStatus = lens _mvesprsResponseStatus (\ s a -> s{_mvesprsResponseStatus = a})

instance NFData
           ModifyVPCEndpointServicePermissionsResponse
         where
