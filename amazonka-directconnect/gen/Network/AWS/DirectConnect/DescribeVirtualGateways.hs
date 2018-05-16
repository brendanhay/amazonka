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
-- Module      : Network.AWS.DirectConnect.DescribeVirtualGateways
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of virtual private gateways owned by the AWS account.
--
--
-- You can create one or more AWS Direct Connect private virtual interfaces linking to a virtual private gateway. A virtual private gateway can be managed via Amazon Virtual Private Cloud (VPC) console or the <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html EC2 CreateVpnGateway> action.
--
module Network.AWS.DirectConnect.DescribeVirtualGateways
    (
    -- * Creating a Request
      describeVirtualGateways
    , DescribeVirtualGateways

    -- * Destructuring the Response
    , describeVirtualGatewaysResponse
    , DescribeVirtualGatewaysResponse
    -- * Response Lenses
    , dvgrsVirtualGateways
    , dvgrsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeVirtualGateways' smart constructor.
data DescribeVirtualGateways =
  DescribeVirtualGateways'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVirtualGateways' with the minimum fields required to make a request.
--
describeVirtualGateways
    :: DescribeVirtualGateways
describeVirtualGateways = DescribeVirtualGateways'


instance AWSRequest DescribeVirtualGateways where
        type Rs DescribeVirtualGateways =
             DescribeVirtualGatewaysResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 DescribeVirtualGatewaysResponse' <$>
                   (x .?> "virtualGateways" .!@ mempty) <*>
                     (pure (fromEnum s)))

instance Hashable DescribeVirtualGateways where

instance NFData DescribeVirtualGateways where

instance ToHeaders DescribeVirtualGateways where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.DescribeVirtualGateways" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeVirtualGateways where
        toJSON = const (Object mempty)

instance ToPath DescribeVirtualGateways where
        toPath = const "/"

instance ToQuery DescribeVirtualGateways where
        toQuery = const mempty

-- | A structure containing a list of virtual private gateways.
--
--
--
-- /See:/ 'describeVirtualGatewaysResponse' smart constructor.
data DescribeVirtualGatewaysResponse = DescribeVirtualGatewaysResponse'
  { _dvgrsVirtualGateways :: !(Maybe [VirtualGateway])
  , _dvgrsResponseStatus  :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeVirtualGatewaysResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvgrsVirtualGateways' - A list of virtual private gateways.
--
-- * 'dvgrsResponseStatus' - -- | The response status code.
describeVirtualGatewaysResponse
    :: Int -- ^ 'dvgrsResponseStatus'
    -> DescribeVirtualGatewaysResponse
describeVirtualGatewaysResponse pResponseStatus_ =
  DescribeVirtualGatewaysResponse'
    {_dvgrsVirtualGateways = Nothing, _dvgrsResponseStatus = pResponseStatus_}


-- | A list of virtual private gateways.
dvgrsVirtualGateways :: Lens' DescribeVirtualGatewaysResponse [VirtualGateway]
dvgrsVirtualGateways = lens _dvgrsVirtualGateways (\ s a -> s{_dvgrsVirtualGateways = a}) . _Default . _Coerce

-- | -- | The response status code.
dvgrsResponseStatus :: Lens' DescribeVirtualGatewaysResponse Int
dvgrsResponseStatus = lens _dvgrsResponseStatus (\ s a -> s{_dvgrsResponseStatus = a})

instance NFData DescribeVirtualGatewaysResponse where
