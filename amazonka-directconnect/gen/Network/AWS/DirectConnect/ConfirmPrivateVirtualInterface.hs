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
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accept ownership of a private virtual interface created by another customer.
--
-- After the virtual interface owner calls this function, the virtual interface will be created and attached to the given virtual private gateway, and will be available for handling traffic.
module Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
    (
    -- * Creating a Request
      confirmPrivateVirtualInterface
    , ConfirmPrivateVirtualInterface
    -- * Request Lenses
    , cpviVirtualInterfaceId
    , cpviVirtualGatewayId

    -- * Destructuring the Response
    , confirmPrivateVirtualInterfaceResponse
    , ConfirmPrivateVirtualInterfaceResponse
    -- * Response Lenses
    , cpvirsVirtualInterfaceState
    , cpvirsResponseStatus
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.DirectConnect.Types.Product
import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the ConfirmPrivateVirtualInterface operation.
--
-- /See:/ 'confirmPrivateVirtualInterface' smart constructor.
data ConfirmPrivateVirtualInterface = ConfirmPrivateVirtualInterface'
    { _cpviVirtualInterfaceId :: !Text
    , _cpviVirtualGatewayId   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfirmPrivateVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpviVirtualInterfaceId'
--
-- * 'cpviVirtualGatewayId'
confirmPrivateVirtualInterface
    :: Text -- ^ 'cpviVirtualInterfaceId'
    -> Text -- ^ 'cpviVirtualGatewayId'
    -> ConfirmPrivateVirtualInterface
confirmPrivateVirtualInterface pVirtualInterfaceId_ pVirtualGatewayId_ =
    ConfirmPrivateVirtualInterface'
    { _cpviVirtualInterfaceId = pVirtualInterfaceId_
    , _cpviVirtualGatewayId = pVirtualGatewayId_
    }

-- | Undocumented member.
cpviVirtualInterfaceId :: Lens' ConfirmPrivateVirtualInterface Text
cpviVirtualInterfaceId = lens _cpviVirtualInterfaceId (\ s a -> s{_cpviVirtualInterfaceId = a});

-- | ID of the virtual private gateway that will be attached to the virtual interface.
--
-- A virtual private gateway can be managed via the Amazon Virtual Private Cloud (VPC) console or the <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html EC2 CreateVpnGateway> action.
--
-- Default: None
cpviVirtualGatewayId :: Lens' ConfirmPrivateVirtualInterface Text
cpviVirtualGatewayId = lens _cpviVirtualGatewayId (\ s a -> s{_cpviVirtualGatewayId = a});

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

instance NFData ConfirmPrivateVirtualInterface

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
                 [Just
                    ("virtualInterfaceId" .= _cpviVirtualInterfaceId),
                  Just ("virtualGatewayId" .= _cpviVirtualGatewayId)])

instance ToPath ConfirmPrivateVirtualInterface where
        toPath = const "/"

instance ToQuery ConfirmPrivateVirtualInterface where
        toQuery = const mempty

-- | The response received when ConfirmPrivateVirtualInterface is called.
--
-- /See:/ 'confirmPrivateVirtualInterfaceResponse' smart constructor.
data ConfirmPrivateVirtualInterfaceResponse = ConfirmPrivateVirtualInterfaceResponse'
    { _cpvirsVirtualInterfaceState :: !(Maybe VirtualInterfaceState)
    , _cpvirsResponseStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ConfirmPrivateVirtualInterfaceResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpvirsVirtualInterfaceState'
--
-- * 'cpvirsResponseStatus'
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
cpvirsVirtualInterfaceState = lens _cpvirsVirtualInterfaceState (\ s a -> s{_cpvirsVirtualInterfaceState = a});

-- | The response status code.
cpvirsResponseStatus :: Lens' ConfirmPrivateVirtualInterfaceResponse Int
cpvirsResponseStatus = lens _cpvirsResponseStatus (\ s a -> s{_cpvirsResponseStatus = a});

instance NFData
         ConfirmPrivateVirtualInterfaceResponse
