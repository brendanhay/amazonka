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
-- Module      : Network.AWS.DirectConnect.CreatePublicVirtualInterface
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new public virtual interface. A virtual interface is the VLAN
-- that transports AWS Direct Connect traffic. A public virtual interface
-- supports sending traffic to public services of AWS such as Amazon Simple
-- Storage Service (Amazon S3).
--
-- /See:/ <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_CreatePublicVirtualInterface.html AWS API Reference> for CreatePublicVirtualInterface.
module Network.AWS.DirectConnect.CreatePublicVirtualInterface
    (
    -- * Creating a Request
      createPublicVirtualInterface
    , CreatePublicVirtualInterface
    -- * Request Lenses
    , cpviConnectionId
    , cpviNewPublicVirtualInterface

    -- * Destructuring the Response
    , virtualInterface
    , VirtualInterface
    -- * Response Lenses
    , viVirtualGatewayId
    , viRouteFilterPrefixes
    , viCustomerAddress
    , viVlan
    , viLocation
    , viAmazonAddress
    , viVirtualInterfaceState
    , viConnectionId
    , viAsn
    , viVirtualInterfaceType
    , viAuthKey
    , viCustomerRouterConfig
    , viOwnerAccount
    , viVirtualInterfaceName
    , viVirtualInterfaceId
    ) where

import           Network.AWS.DirectConnect.Types
import           Network.AWS.DirectConnect.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the CreatePublicVirtualInterface
-- operation.
--
-- /See:/ 'createPublicVirtualInterface' smart constructor.
data CreatePublicVirtualInterface = CreatePublicVirtualInterface'
    { _cpviConnectionId              :: !Text
    , _cpviNewPublicVirtualInterface :: !NewPublicVirtualInterface
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CreatePublicVirtualInterface' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpviConnectionId'
--
-- * 'cpviNewPublicVirtualInterface'
createPublicVirtualInterface
    :: Text -- ^ 'cpviConnectionId'
    -> NewPublicVirtualInterface -- ^ 'cpviNewPublicVirtualInterface'
    -> CreatePublicVirtualInterface
createPublicVirtualInterface pConnectionId_ pNewPublicVirtualInterface_ =
    CreatePublicVirtualInterface'
    { _cpviConnectionId = pConnectionId_
    , _cpviNewPublicVirtualInterface = pNewPublicVirtualInterface_
    }

-- | Undocumented member.
cpviConnectionId :: Lens' CreatePublicVirtualInterface Text
cpviConnectionId = lens _cpviConnectionId (\ s a -> s{_cpviConnectionId = a});

-- | Detailed information for the public virtual interface to be created.
--
-- Default: None
cpviNewPublicVirtualInterface :: Lens' CreatePublicVirtualInterface NewPublicVirtualInterface
cpviNewPublicVirtualInterface = lens _cpviNewPublicVirtualInterface (\ s a -> s{_cpviNewPublicVirtualInterface = a});

instance AWSRequest CreatePublicVirtualInterface
         where
        type Rs CreatePublicVirtualInterface =
             VirtualInterface
        request = postJSON directConnect
        response = receiveJSON (\ s h x -> eitherParseJSON x)

instance ToHeaders CreatePublicVirtualInterface where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("OvertureService.CreatePublicVirtualInterface" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON CreatePublicVirtualInterface where
        toJSON CreatePublicVirtualInterface'{..}
          = object
              (catMaybes
                 [Just ("connectionId" .= _cpviConnectionId),
                  Just
                    ("newPublicVirtualInterface" .=
                       _cpviNewPublicVirtualInterface)])

instance ToPath CreatePublicVirtualInterface where
        toPath = const "/"

instance ToQuery CreatePublicVirtualInterface where
        toQuery = const mempty
