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
-- Module      : Network.AWS.Snowball.UpdateCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- While a cluster's @ClusterState@ value is in the @AwaitingQuorum@ state, you can update some of the information associated with a cluster. Once the cluster changes to a different job state, usually 60 minutes after the cluster being created, this action is no longer available.
--
--
module Network.AWS.Snowball.UpdateCluster
    (
    -- * Creating a Request
      updateCluster
    , UpdateCluster
    -- * Request Lenses
    , ucNotification
    , ucForwardingAddressId
    , ucAddressId
    , ucShippingOption
    , ucResources
    , ucDescription
    , ucRoleARN
    , ucClusterId

    -- * Destructuring the Response
    , updateClusterResponse
    , UpdateClusterResponse
    -- * Response Lenses
    , ucrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Snowball.Types
import Network.AWS.Snowball.Types.Product

-- | /See:/ 'updateCluster' smart constructor.
data UpdateCluster = UpdateCluster'
  { _ucNotification        :: !(Maybe Notification)
  , _ucForwardingAddressId :: !(Maybe Text)
  , _ucAddressId           :: !(Maybe Text)
  , _ucShippingOption      :: !(Maybe ShippingOption)
  , _ucResources           :: !(Maybe JobResource)
  , _ucDescription         :: !(Maybe Text)
  , _ucRoleARN             :: !(Maybe Text)
  , _ucClusterId           :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucNotification' - The new or updated 'Notification' object.
--
-- * 'ucForwardingAddressId' - The updated ID for the forwarding address for a cluster. This field is not supported in most regions.
--
-- * 'ucAddressId' - The ID of the updated 'Address' object.
--
-- * 'ucShippingOption' - The updated shipping option value of this cluster's 'ShippingDetails' object.
--
-- * 'ucResources' - The updated arrays of 'JobResource' objects that can include updated 'S3Resource' objects or 'LambdaResource' objects.
--
-- * 'ucDescription' - The updated description of this cluster.
--
-- * 'ucRoleARN' - The new role Amazon Resource Name (ARN) that you want to associate with this cluster. To create a role ARN, use the <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
--
-- * 'ucClusterId' - The cluster ID of the cluster that you want to update, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
updateCluster
    :: Text -- ^ 'ucClusterId'
    -> UpdateCluster
updateCluster pClusterId_ =
  UpdateCluster'
    { _ucNotification = Nothing
    , _ucForwardingAddressId = Nothing
    , _ucAddressId = Nothing
    , _ucShippingOption = Nothing
    , _ucResources = Nothing
    , _ucDescription = Nothing
    , _ucRoleARN = Nothing
    , _ucClusterId = pClusterId_
    }


-- | The new or updated 'Notification' object.
ucNotification :: Lens' UpdateCluster (Maybe Notification)
ucNotification = lens _ucNotification (\ s a -> s{_ucNotification = a})

-- | The updated ID for the forwarding address for a cluster. This field is not supported in most regions.
ucForwardingAddressId :: Lens' UpdateCluster (Maybe Text)
ucForwardingAddressId = lens _ucForwardingAddressId (\ s a -> s{_ucForwardingAddressId = a})

-- | The ID of the updated 'Address' object.
ucAddressId :: Lens' UpdateCluster (Maybe Text)
ucAddressId = lens _ucAddressId (\ s a -> s{_ucAddressId = a})

-- | The updated shipping option value of this cluster's 'ShippingDetails' object.
ucShippingOption :: Lens' UpdateCluster (Maybe ShippingOption)
ucShippingOption = lens _ucShippingOption (\ s a -> s{_ucShippingOption = a})

-- | The updated arrays of 'JobResource' objects that can include updated 'S3Resource' objects or 'LambdaResource' objects.
ucResources :: Lens' UpdateCluster (Maybe JobResource)
ucResources = lens _ucResources (\ s a -> s{_ucResources = a})

-- | The updated description of this cluster.
ucDescription :: Lens' UpdateCluster (Maybe Text)
ucDescription = lens _ucDescription (\ s a -> s{_ucDescription = a})

-- | The new role Amazon Resource Name (ARN) that you want to associate with this cluster. To create a role ARN, use the <http://docs.aws.amazon.com/IAM/latest/APIReference/API_CreateRole.html CreateRole> API action in AWS Identity and Access Management (IAM).
ucRoleARN :: Lens' UpdateCluster (Maybe Text)
ucRoleARN = lens _ucRoleARN (\ s a -> s{_ucRoleARN = a})

-- | The cluster ID of the cluster that you want to update, for example @CID123e4567-e89b-12d3-a456-426655440000@ .
ucClusterId :: Lens' UpdateCluster Text
ucClusterId = lens _ucClusterId (\ s a -> s{_ucClusterId = a})

instance AWSRequest UpdateCluster where
        type Rs UpdateCluster = UpdateClusterResponse
        request = postJSON snowball
        response
          = receiveEmpty
              (\ s h x ->
                 UpdateClusterResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateCluster where

instance NFData UpdateCluster where

instance ToHeaders UpdateCluster where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("AWSIESnowballJobManagementService.UpdateCluster" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateCluster where
        toJSON UpdateCluster'{..}
          = object
              (catMaybes
                 [("Notification" .=) <$> _ucNotification,
                  ("ForwardingAddressId" .=) <$>
                    _ucForwardingAddressId,
                  ("AddressId" .=) <$> _ucAddressId,
                  ("ShippingOption" .=) <$> _ucShippingOption,
                  ("Resources" .=) <$> _ucResources,
                  ("Description" .=) <$> _ucDescription,
                  ("RoleARN" .=) <$> _ucRoleARN,
                  Just ("ClusterId" .= _ucClusterId)])

instance ToPath UpdateCluster where
        toPath = const "/"

instance ToQuery UpdateCluster where
        toQuery = const mempty

-- | /See:/ 'updateClusterResponse' smart constructor.
newtype UpdateClusterResponse = UpdateClusterResponse'
  { _ucrsResponseStatus :: Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'UpdateClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ucrsResponseStatus' - -- | The response status code.
updateClusterResponse
    :: Int -- ^ 'ucrsResponseStatus'
    -> UpdateClusterResponse
updateClusterResponse pResponseStatus_ =
  UpdateClusterResponse' {_ucrsResponseStatus = pResponseStatus_}


-- | -- | The response status code.
ucrsResponseStatus :: Lens' UpdateClusterResponse Int
ucrsResponseStatus = lens _ucrsResponseStatus (\ s a -> s{_ucrsResponseStatus = a})

instance NFData UpdateClusterResponse where
