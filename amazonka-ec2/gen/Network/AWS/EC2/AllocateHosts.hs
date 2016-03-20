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
-- Module      : Network.AWS.EC2.AllocateHosts
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allocates a Dedicated host to your account. At minimum you need to
-- specify the instance size type, Availability Zone, and quantity of hosts
-- you want to allocate.
module Network.AWS.EC2.AllocateHosts
    (
    -- * Creating a Request
      allocateHosts
    , AllocateHosts
    -- * Request Lenses
    , ahClientToken
    , ahAutoPlacement
    , ahInstanceType
    , ahQuantity
    , ahAvailabilityZone

    -- * Destructuring the Response
    , allocateHostsResponse
    , AllocateHostsResponse
    -- * Response Lenses
    , ahrsHostIds
    , ahrsResponseStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'allocateHosts' smart constructor.
data AllocateHosts = AllocateHosts'
    { _ahClientToken      :: !(Maybe Text)
    , _ahAutoPlacement    :: !(Maybe AutoPlacement)
    , _ahInstanceType     :: !Text
    , _ahQuantity         :: !Int
    , _ahAvailabilityZone :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AllocateHosts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ahClientToken'
--
-- * 'ahAutoPlacement'
--
-- * 'ahInstanceType'
--
-- * 'ahQuantity'
--
-- * 'ahAvailabilityZone'
allocateHosts
    :: Text -- ^ 'ahInstanceType'
    -> Int -- ^ 'ahQuantity'
    -> Text -- ^ 'ahAvailabilityZone'
    -> AllocateHosts
allocateHosts pInstanceType_ pQuantity_ pAvailabilityZone_ =
    AllocateHosts'
    { _ahClientToken = Nothing
    , _ahAutoPlacement = Nothing
    , _ahInstanceType = pInstanceType_
    , _ahQuantity = pQuantity_
    , _ahAvailabilityZone = pAvailabilityZone_
    }

-- | Unique, case-sensitive identifier you provide to ensure idempotency of
-- the request. For more information, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency>
-- in the /Amazon Elastic Compute Cloud User Guide/.
ahClientToken :: Lens' AllocateHosts (Maybe Text)
ahClientToken = lens _ahClientToken (\ s a -> s{_ahClientToken = a});

-- | This is enabled by default. This property allows instances to be
-- automatically placed onto available Dedicated hosts, when you are
-- launching instances without specifying a host ID.
--
-- Default: Enabled
ahAutoPlacement :: Lens' AllocateHosts (Maybe AutoPlacement)
ahAutoPlacement = lens _ahAutoPlacement (\ s a -> s{_ahAutoPlacement = a});

-- | Specify the instance type that you want your Dedicated hosts to be
-- configured for. When you specify the instance type, that is the only
-- instance type that you can launch onto that host.
ahInstanceType :: Lens' AllocateHosts Text
ahInstanceType = lens _ahInstanceType (\ s a -> s{_ahInstanceType = a});

-- | The number of Dedicated hosts you want to allocate to your account with
-- these parameters.
ahQuantity :: Lens' AllocateHosts Int
ahQuantity = lens _ahQuantity (\ s a -> s{_ahQuantity = a});

-- | The Availability Zone for the Dedicated hosts.
ahAvailabilityZone :: Lens' AllocateHosts Text
ahAvailabilityZone = lens _ahAvailabilityZone (\ s a -> s{_ahAvailabilityZone = a});

instance AWSRequest AllocateHosts where
        type Rs AllocateHosts = AllocateHostsResponse
        request = postQuery eC2
        response
          = receiveXML
              (\ s h x ->
                 AllocateHostsResponse' <$>
                   (x .@? "hostIdSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable AllocateHosts

instance ToHeaders AllocateHosts where
        toHeaders = const mempty

instance ToPath AllocateHosts where
        toPath = const "/"

instance ToQuery AllocateHosts where
        toQuery AllocateHosts'{..}
          = mconcat
              ["Action" =: ("AllocateHosts" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "ClientToken" =: _ahClientToken,
               "AutoPlacement" =: _ahAutoPlacement,
               "InstanceType" =: _ahInstanceType,
               "Quantity" =: _ahQuantity,
               "AvailabilityZone" =: _ahAvailabilityZone]

-- | /See:/ 'allocateHostsResponse' smart constructor.
data AllocateHostsResponse = AllocateHostsResponse'
    { _ahrsHostIds        :: !(Maybe [Text])
    , _ahrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'AllocateHostsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ahrsHostIds'
--
-- * 'ahrsResponseStatus'
allocateHostsResponse
    :: Int -- ^ 'ahrsResponseStatus'
    -> AllocateHostsResponse
allocateHostsResponse pResponseStatus_ =
    AllocateHostsResponse'
    { _ahrsHostIds = Nothing
    , _ahrsResponseStatus = pResponseStatus_
    }

-- | The ID of the allocated Dedicated host. This is used when you want to
-- launch an instance onto a specific host.
ahrsHostIds :: Lens' AllocateHostsResponse [Text]
ahrsHostIds = lens _ahrsHostIds (\ s a -> s{_ahrsHostIds = a}) . _Default . _Coerce;

-- | The response status code.
ahrsResponseStatus :: Lens' AllocateHostsResponse Int
ahrsResponseStatus = lens _ahrsResponseStatus (\ s a -> s{_ahrsResponseStatus = a});
