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
-- Module      : Network.AWS.EC2.CreateNetworkInterfacePermission
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Grants an AWS-authorized account permission to attach the specified network interface to an instance in their account.
--
--
-- You can grant permission to a single AWS account only, and only one account at a time.
--
module Network.AWS.EC2.CreateNetworkInterfacePermission
    (
    -- * Creating a Request
      createNetworkInterfacePermission
    , CreateNetworkInterfacePermission
    -- * Request Lenses
    , cnipAWSAccountId
    , cnipAWSService
    , cnipDryRun
    , cnipNetworkInterfaceId
    , cnipPermission

    -- * Destructuring the Response
    , createNetworkInterfacePermissionResponse
    , CreateNetworkInterfacePermissionResponse
    -- * Response Lenses
    , cniprsInterfacePermission
    , cniprsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreateNetworkInterfacePermission.
--
--
--
-- /See:/ 'createNetworkInterfacePermission' smart constructor.
data CreateNetworkInterfacePermission = CreateNetworkInterfacePermission'
  { _cnipAWSAccountId       :: !(Maybe Text)
  , _cnipAWSService         :: !(Maybe Text)
  , _cnipDryRun             :: !(Maybe Bool)
  , _cnipNetworkInterfaceId :: !Text
  , _cnipPermission         :: !InterfacePermissionType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNetworkInterfacePermission' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cnipAWSAccountId' - The AWS account ID.
--
-- * 'cnipAWSService' - The AWS service. Currently not supported.
--
-- * 'cnipDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cnipNetworkInterfaceId' - The ID of the network interface.
--
-- * 'cnipPermission' - The type of permission to grant.
createNetworkInterfacePermission
    :: Text -- ^ 'cnipNetworkInterfaceId'
    -> InterfacePermissionType -- ^ 'cnipPermission'
    -> CreateNetworkInterfacePermission
createNetworkInterfacePermission pNetworkInterfaceId_ pPermission_ =
  CreateNetworkInterfacePermission'
    { _cnipAWSAccountId = Nothing
    , _cnipAWSService = Nothing
    , _cnipDryRun = Nothing
    , _cnipNetworkInterfaceId = pNetworkInterfaceId_
    , _cnipPermission = pPermission_
    }


-- | The AWS account ID.
cnipAWSAccountId :: Lens' CreateNetworkInterfacePermission (Maybe Text)
cnipAWSAccountId = lens _cnipAWSAccountId (\ s a -> s{_cnipAWSAccountId = a})

-- | The AWS service. Currently not supported.
cnipAWSService :: Lens' CreateNetworkInterfacePermission (Maybe Text)
cnipAWSService = lens _cnipAWSService (\ s a -> s{_cnipAWSService = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cnipDryRun :: Lens' CreateNetworkInterfacePermission (Maybe Bool)
cnipDryRun = lens _cnipDryRun (\ s a -> s{_cnipDryRun = a})

-- | The ID of the network interface.
cnipNetworkInterfaceId :: Lens' CreateNetworkInterfacePermission Text
cnipNetworkInterfaceId = lens _cnipNetworkInterfaceId (\ s a -> s{_cnipNetworkInterfaceId = a})

-- | The type of permission to grant.
cnipPermission :: Lens' CreateNetworkInterfacePermission InterfacePermissionType
cnipPermission = lens _cnipPermission (\ s a -> s{_cnipPermission = a})

instance AWSRequest CreateNetworkInterfacePermission
         where
        type Rs CreateNetworkInterfacePermission =
             CreateNetworkInterfacePermissionResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CreateNetworkInterfacePermissionResponse' <$>
                   (x .@? "interfacePermission") <*>
                     (pure (fromEnum s)))

instance Hashable CreateNetworkInterfacePermission
         where

instance NFData CreateNetworkInterfacePermission
         where

instance ToHeaders CreateNetworkInterfacePermission
         where
        toHeaders = const mempty

instance ToPath CreateNetworkInterfacePermission
         where
        toPath = const "/"

instance ToQuery CreateNetworkInterfacePermission
         where
        toQuery CreateNetworkInterfacePermission'{..}
          = mconcat
              ["Action" =:
                 ("CreateNetworkInterfacePermission" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "AwsAccountId" =: _cnipAWSAccountId,
               "AwsService" =: _cnipAWSService,
               "DryRun" =: _cnipDryRun,
               "NetworkInterfaceId" =: _cnipNetworkInterfaceId,
               "Permission" =: _cnipPermission]

-- | Contains the output of CreateNetworkInterfacePermission.
--
--
--
-- /See:/ 'createNetworkInterfacePermissionResponse' smart constructor.
data CreateNetworkInterfacePermissionResponse = CreateNetworkInterfacePermissionResponse'
  { _cniprsInterfacePermission :: !(Maybe NetworkInterfacePermission)
  , _cniprsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreateNetworkInterfacePermissionResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cniprsInterfacePermission' - Information about the permission for the network interface.
--
-- * 'cniprsResponseStatus' - -- | The response status code.
createNetworkInterfacePermissionResponse
    :: Int -- ^ 'cniprsResponseStatus'
    -> CreateNetworkInterfacePermissionResponse
createNetworkInterfacePermissionResponse pResponseStatus_ =
  CreateNetworkInterfacePermissionResponse'
    { _cniprsInterfacePermission = Nothing
    , _cniprsResponseStatus = pResponseStatus_
    }


-- | Information about the permission for the network interface.
cniprsInterfacePermission :: Lens' CreateNetworkInterfacePermissionResponse (Maybe NetworkInterfacePermission)
cniprsInterfacePermission = lens _cniprsInterfacePermission (\ s a -> s{_cniprsInterfacePermission = a})

-- | -- | The response status code.
cniprsResponseStatus :: Lens' CreateNetworkInterfacePermissionResponse Int
cniprsResponseStatus = lens _cniprsResponseStatus (\ s a -> s{_cniprsResponseStatus = a})

instance NFData
           CreateNetworkInterfacePermissionResponse
         where
