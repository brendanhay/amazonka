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
-- Module      : Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accept ownership of a private virtual interface created by another customer.
--
--
-- After the virtual interface owner calls this function, the virtual interface will be created and attached to the given virtual private gateway or direct connect gateway, and will be available for handling traffic.
--
module Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
    (
    -- * Creating a Request
      confirmPrivateVirtualInterface
    , ConfirmPrivateVirtualInterface
    -- * Request Lenses
    , cpviVirtualGatewayId
    , cpviDirectConnectGatewayId
    , cpviVirtualInterfaceId

    -- * Destructuring the Response
    , confirmPrivateVirtualInterfaceResponse
    , ConfirmPrivateVirtualInterfaceResponse
    -- * Response Lenses
    , cpvirsVirtualInterfaceState
    , cpvirsResponseStatus
    ) where

import Network.AWS.DirectConnect.Types
import Network.AWS.DirectConnect.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the ConfirmPrivateVirtualInterface operation.
--
--
--
-- /See:/ 'confirmPrivateVirtualInterface' smart constructor.
data ConfirmPrivateVirtualInterface = ConfirmPrivateVirtualInterface'
  { _cpviVirtualGatewayId       :: !(Maybe Text)
  , _cpviDirectConnectGatewayId :: !(Maybe Text)
  , _cpviVirtualInterfaceId     :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfirmPrivateVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpviVirtualGatewayId' - ID of the virtual private gateway that will be attached to the virtual interface. A virtual private gateway can be managed via the Amazon Virtual Private Cloud (VPC) console or the <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html EC2 CreateVpnGateway> action. Default: None
--
-- * 'cpviDirectConnectGatewayId' - ID of the direct connect gateway that will be attached to the virtual interface. A direct connect gateway can be managed via the AWS Direct Connect console or the 'CreateDirectConnectGateway' action. Default: None
--
-- * 'cpviVirtualInterfaceId' - Undocumented member.
confirmPrivateVirtualInterface
    :: Text -- ^ 'cpviVirtualInterfaceId'
    -> ConfirmPrivateVirtualInterface
confirmPrivateVirtualInterface pVirtualInterfaceId_ =
  ConfirmPrivateVirtualInterface'
    { _cpviVirtualGatewayId = Nothing
    , _cpviDirectConnectGatewayId = Nothing
    , _cpviVirtualInterfaceId = pVirtualInterfaceId_
    }


-- | ID of the virtual private gateway that will be attached to the virtual interface. A virtual private gateway can be managed via the Amazon Virtual Private Cloud (VPC) console or the <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html EC2 CreateVpnGateway> action. Default: None
cpviVirtualGatewayId :: Lens' ConfirmPrivateVirtualInterface (Maybe Text)
cpviVirtualGatewayId = lens _cpviVirtualGatewayId (\ s a -> s{_cpviVirtualGatewayId = a})

-- | ID of the direct connect gateway that will be attached to the virtual interface. A direct connect gateway can be managed via the AWS Direct Connect console or the 'CreateDirectConnectGateway' action. Default: None
cpviDirectConnectGatewayId :: Lens' ConfirmPrivateVirtualInterface (Maybe Text)
cpviDirectConnectGatewayId = lens _cpviDirectConnectGatewayId (\ s a -> s{_cpviDirectConnectGatewayId = a})

-- | Undocumented member.
cpviVirtualInterfaceId :: Lens' ConfirmPrivateVirtualInterface Text
cpviVirtualInterfaceId = lens _cpviVirtualInterfaceId (\ s a -> s{_cpviVirtualInterfaceId = a})

instance AWSRequest ConfirmPrivateVirtualInterface
         where
        type Rs ConfirmPrivateVirtualInterface =
             ConfirmPrivateVirtualInterfaceResponse
        request = postJSON directConnect
        response
          = receiveJSON
              (\ s h x ->
                 ConfirmPrivateVirtualInterfaceResponse' <$>
                   (x .?> "virtualInterfaceState") <*>
                     (pure (fromEnum s)))

instance Hashable ConfirmPrivateVirtualInterface
         where

instance NFData ConfirmPrivateVirtualInterface where

instance ToHeaders ConfirmPrivateVirtualInterface
         where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.ConfirmPrivateVirtualInterface" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ConfirmPrivateVirtualInterface where
        toJSON ConfirmPrivateVirtualInterface'{..}
          = object
              (catMaybes
                 [("virtualGatewayId" .=) <$> _cpviVirtualGatewayId,
                  ("directConnectGatewayId" .=) <$>
                    _cpviDirectConnectGatewayId,
                  Just
                    ("virtualInterfaceId" .= _cpviVirtualInterfaceId)])

instance ToPath ConfirmPrivateVirtualInterface where
        toPath = const "/"

instance ToQuery ConfirmPrivateVirtualInterface where
        toQuery = const mempty

-- | The response received when ConfirmPrivateVirtualInterface is called.
--
--
--
-- /See:/ 'confirmPrivateVirtualInterfaceResponse' smart constructor.
data ConfirmPrivateVirtualInterfaceResponse = ConfirmPrivateVirtualInterfaceResponse'
  { _cpvirsVirtualInterfaceState :: !(Maybe VirtualInterfaceState)
  , _cpvirsResponseStatus        :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ConfirmPrivateVirtualInterfaceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpvirsVirtualInterfaceState' - Undocumented member.
--
-- * 'cpvirsResponseStatus' - -- | The response status code.
confirmPrivateVirtualInterfaceResponse
    :: Int -- ^ 'cpvirsResponseStatus'
    -> ConfirmPrivateVirtualInterfaceResponse
confirmPrivateVirtualInterfaceResponse pResponseStatus_ =
  ConfirmPrivateVirtualInterfaceResponse'
    { _cpvirsVirtualInterfaceState = Nothing
    , _cpvirsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
cpvirsVirtualInterfaceState :: Lens' ConfirmPrivateVirtualInterfaceResponse (Maybe VirtualInterfaceState)
cpvirsVirtualInterfaceState = lens _cpvirsVirtualInterfaceState (\ s a -> s{_cpvirsVirtualInterfaceState = a})

-- | -- | The response status code.
cpvirsResponseStatus :: Lens' ConfirmPrivateVirtualInterfaceResponse Int
cpvirsResponseStatus = lens _cpvirsResponseStatus (\ s a -> s{_cpvirsResponseStatus = a})

instance NFData
           ConfirmPrivateVirtualInterfaceResponse
         where
