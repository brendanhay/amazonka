{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Accept ownership of a private virtual interface created by another
-- customer.
--
-- After the virtual interface owner calls this function, the virtual
-- interface will be created and attached to the given virtual private
-- gateway, and will be available for handling traffic.
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_ConfirmPrivateVirtualInterface.html>
module Network.AWS.DirectConnect.ConfirmPrivateVirtualInterface
    (
    -- * Request
      ConfirmPrivateVirtualInterface
    -- ** Request constructor
    , confirmPrivateVirtualInterface
    -- ** Request lenses
    , cpviVirtualInterfaceId
    , cpviVirtualGatewayId

    -- * Response
    , ConfirmPrivateVirtualInterfaceResponse
    -- ** Response constructor
    , confirmPrivateVirtualInterfaceResponse
    -- ** Response lenses
    , cpvirsVirtualInterfaceState
    , cpvirsStatus
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the ConfirmPrivateVirtualInterface
-- operation.
--
-- /See:/ 'confirmPrivateVirtualInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpviVirtualInterfaceId'
--
-- * 'cpviVirtualGatewayId'
data ConfirmPrivateVirtualInterface = ConfirmPrivateVirtualInterface'
    { _cpviVirtualInterfaceId :: !Text
    , _cpviVirtualGatewayId   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConfirmPrivateVirtualInterface' smart constructor.
confirmPrivateVirtualInterface :: Text -> Text -> ConfirmPrivateVirtualInterface
confirmPrivateVirtualInterface pVirtualInterfaceId_ pVirtualGatewayId_ =
    ConfirmPrivateVirtualInterface'
    { _cpviVirtualInterfaceId = pVirtualInterfaceId_
    , _cpviVirtualGatewayId = pVirtualGatewayId_
    }

-- | FIXME: Undocumented member.
cpviVirtualInterfaceId :: Lens' ConfirmPrivateVirtualInterface Text
cpviVirtualInterfaceId = lens _cpviVirtualInterfaceId (\ s a -> s{_cpviVirtualInterfaceId = a});

-- | ID of the virtual private gateway that will be attached to the virtual
-- interface.
--
-- A virtual private gateway can be managed via the Amazon Virtual Private
-- Cloud (VPC) console or the
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateVpnGateway.html EC2 CreateVpnGateway>
-- action.
--
-- Default: None
cpviVirtualGatewayId :: Lens' ConfirmPrivateVirtualInterface Text
cpviVirtualGatewayId = lens _cpviVirtualGatewayId (\ s a -> s{_cpviVirtualGatewayId = a});

instance AWSRequest ConfirmPrivateVirtualInterface
         where
        type Sv ConfirmPrivateVirtualInterface =
             DirectConnect
        type Rs ConfirmPrivateVirtualInterface =
             ConfirmPrivateVirtualInterfaceResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ConfirmPrivateVirtualInterfaceResponse' <$>
                   (x .?> "virtualInterfaceState") <*>
                     (pure (fromEnum s)))

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
              ["virtualInterfaceId" .= _cpviVirtualInterfaceId,
               "virtualGatewayId" .= _cpviVirtualGatewayId]

instance ToPath ConfirmPrivateVirtualInterface where
        toPath = const mempty

instance ToQuery ConfirmPrivateVirtualInterface where
        toQuery = const mempty

-- | The response received when ConfirmPrivateVirtualInterface is called.
--
-- /See:/ 'confirmPrivateVirtualInterfaceResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpvirsVirtualInterfaceState'
--
-- * 'cpvirsStatus'
data ConfirmPrivateVirtualInterfaceResponse = ConfirmPrivateVirtualInterfaceResponse'
    { _cpvirsVirtualInterfaceState :: !(Maybe VirtualInterfaceState)
    , _cpvirsStatus                :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ConfirmPrivateVirtualInterfaceResponse' smart constructor.
confirmPrivateVirtualInterfaceResponse :: Int -> ConfirmPrivateVirtualInterfaceResponse
confirmPrivateVirtualInterfaceResponse pStatus_ =
    ConfirmPrivateVirtualInterfaceResponse'
    { _cpvirsVirtualInterfaceState = Nothing
    , _cpvirsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
cpvirsVirtualInterfaceState :: Lens' ConfirmPrivateVirtualInterfaceResponse (Maybe VirtualInterfaceState)
cpvirsVirtualInterfaceState = lens _cpvirsVirtualInterfaceState (\ s a -> s{_cpvirsVirtualInterfaceState = a});

-- | FIXME: Undocumented member.
cpvirsStatus :: Lens' ConfirmPrivateVirtualInterfaceResponse Int
cpvirsStatus = lens _cpvirsStatus (\ s a -> s{_cpvirsStatus = a});
