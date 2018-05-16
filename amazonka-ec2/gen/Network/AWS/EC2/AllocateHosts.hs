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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Allocates a Dedicated Host to your account. At minimum you need to specify the instance size type, Availability Zone, and quantity of hosts you want to allocate.
--
--
module Network.AWS.EC2.AllocateHosts
    (
    -- * Creating a Request
      allocateHosts
    , AllocateHosts
    -- * Request Lenses
    , ahClientToken
    , ahAutoPlacement
    , ahAvailabilityZone
    , ahInstanceType
    , ahQuantity

    -- * Destructuring the Response
    , allocateHostsResponse
    , AllocateHostsResponse
    -- * Response Lenses
    , ahrsHostIds
    , ahrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for AllocateHosts.
--
--
--
-- /See:/ 'allocateHosts' smart constructor.
data AllocateHosts = AllocateHosts'
  { _ahClientToken      :: !(Maybe Text)
  , _ahAutoPlacement    :: !(Maybe AutoPlacement)
  , _ahAvailabilityZone :: !Text
  , _ahInstanceType     :: !Text
  , _ahQuantity         :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AllocateHosts' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ahClientToken' - Unique, case-sensitive identifier you provide to ensure idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- * 'ahAutoPlacement' - This is enabled by default. This property allows instances to be automatically placed onto available Dedicated Hosts, when you are launching instances without specifying a host ID. Default: Enabled
--
-- * 'ahAvailabilityZone' - The Availability Zone for the Dedicated Hosts.
--
-- * 'ahInstanceType' - Specify the instance type that you want your Dedicated Hosts to be configured for. When you specify the instance type, that is the only instance type that you can launch onto that host.
--
-- * 'ahQuantity' - The number of Dedicated Hosts you want to allocate to your account with these parameters.
allocateHosts
    :: Text -- ^ 'ahAvailabilityZone'
    -> Text -- ^ 'ahInstanceType'
    -> Int -- ^ 'ahQuantity'
    -> AllocateHosts
allocateHosts pAvailabilityZone_ pInstanceType_ pQuantity_ =
  AllocateHosts'
    { _ahClientToken = Nothing
    , _ahAutoPlacement = Nothing
    , _ahAvailabilityZone = pAvailabilityZone_
    , _ahInstanceType = pInstanceType_
    , _ahQuantity = pQuantity_
    }


-- | Unique, case-sensitive identifier you provide to ensure idempotency of the request. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/Run_Instance_Idempotency.html How to Ensure Idempotency> in the /Amazon Elastic Compute Cloud User Guide/ .
ahClientToken :: Lens' AllocateHosts (Maybe Text)
ahClientToken = lens _ahClientToken (\ s a -> s{_ahClientToken = a})

-- | This is enabled by default. This property allows instances to be automatically placed onto available Dedicated Hosts, when you are launching instances without specifying a host ID. Default: Enabled
ahAutoPlacement :: Lens' AllocateHosts (Maybe AutoPlacement)
ahAutoPlacement = lens _ahAutoPlacement (\ s a -> s{_ahAutoPlacement = a})

-- | The Availability Zone for the Dedicated Hosts.
ahAvailabilityZone :: Lens' AllocateHosts Text
ahAvailabilityZone = lens _ahAvailabilityZone (\ s a -> s{_ahAvailabilityZone = a})

-- | Specify the instance type that you want your Dedicated Hosts to be configured for. When you specify the instance type, that is the only instance type that you can launch onto that host.
ahInstanceType :: Lens' AllocateHosts Text
ahInstanceType = lens _ahInstanceType (\ s a -> s{_ahInstanceType = a})

-- | The number of Dedicated Hosts you want to allocate to your account with these parameters.
ahQuantity :: Lens' AllocateHosts Int
ahQuantity = lens _ahQuantity (\ s a -> s{_ahQuantity = a})

instance AWSRequest AllocateHosts where
        type Rs AllocateHosts = AllocateHostsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 AllocateHostsResponse' <$>
                   (x .@? "hostIdSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable AllocateHosts where

instance NFData AllocateHosts where

instance ToHeaders AllocateHosts where
        toHeaders = const mempty

instance ToPath AllocateHosts where
        toPath = const "/"

instance ToQuery AllocateHosts where
        toQuery AllocateHosts'{..}
          = mconcat
              ["Action" =: ("AllocateHosts" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "ClientToken" =: _ahClientToken,
               "AutoPlacement" =: _ahAutoPlacement,
               "AvailabilityZone" =: _ahAvailabilityZone,
               "InstanceType" =: _ahInstanceType,
               "Quantity" =: _ahQuantity]

-- | Contains the output of AllocateHosts.
--
--
--
-- /See:/ 'allocateHostsResponse' smart constructor.
data AllocateHostsResponse = AllocateHostsResponse'
  { _ahrsHostIds        :: !(Maybe [Text])
  , _ahrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AllocateHostsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ahrsHostIds' - The ID of the allocated Dedicated Host. This is used when you want to launch an instance onto a specific host.
--
-- * 'ahrsResponseStatus' - -- | The response status code.
allocateHostsResponse
    :: Int -- ^ 'ahrsResponseStatus'
    -> AllocateHostsResponse
allocateHostsResponse pResponseStatus_ =
  AllocateHostsResponse'
    {_ahrsHostIds = Nothing, _ahrsResponseStatus = pResponseStatus_}


-- | The ID of the allocated Dedicated Host. This is used when you want to launch an instance onto a specific host.
ahrsHostIds :: Lens' AllocateHostsResponse [Text]
ahrsHostIds = lens _ahrsHostIds (\ s a -> s{_ahrsHostIds = a}) . _Default . _Coerce

-- | -- | The response status code.
ahrsResponseStatus :: Lens' AllocateHostsResponse Int
ahrsResponseStatus = lens _ahrsResponseStatus (\ s a -> s{_ahrsResponseStatus = a})

instance NFData AllocateHostsResponse where
