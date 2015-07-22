{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreatePublicVirtualInterface
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a new public virtual interface. A virtual interface is the VLAN
-- that transports AWS Direct Connect traffic. A public virtual interface
-- supports sending traffic to public services of AWS such as Amazon Simple
-- Storage Service (Amazon S3).
--
-- <http://docs.aws.amazon.com/directconnect/latest/APIReference/API_CreatePublicVirtualInterface.html>
module Network.AWS.DirectConnect.CreatePublicVirtualInterface
    (
    -- * Request
      CreatePublicVirtualInterface
    -- ** Request constructor
    , createPublicVirtualInterface
    -- ** Request lenses
    , cpvirqConnectionId
    , cpvirqNewPublicVirtualInterface

    -- * Response
    , VirtualInterface
    -- ** Response constructor
    , virtualInterface
    -- ** Response lenses
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
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Container for the parameters to the CreatePublicVirtualInterface
-- operation.
--
-- /See:/ 'createPublicVirtualInterface' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpvirqConnectionId'
--
-- * 'cpvirqNewPublicVirtualInterface'
data CreatePublicVirtualInterface = CreatePublicVirtualInterface'
    { _cpvirqConnectionId              :: !Text
    , _cpvirqNewPublicVirtualInterface :: !NewPublicVirtualInterface
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePublicVirtualInterface' smart constructor.
createPublicVirtualInterface :: Text -> NewPublicVirtualInterface -> CreatePublicVirtualInterface
createPublicVirtualInterface pConnectionId_ pNewPublicVirtualInterface_ =
    CreatePublicVirtualInterface'
    { _cpvirqConnectionId = pConnectionId_
    , _cpvirqNewPublicVirtualInterface = pNewPublicVirtualInterface_
    }

-- | FIXME: Undocumented member.
cpvirqConnectionId :: Lens' CreatePublicVirtualInterface Text
cpvirqConnectionId = lens _cpvirqConnectionId (\ s a -> s{_cpvirqConnectionId = a});

-- | Detailed information for the public virtual interface to be created.
--
-- Default: None
cpvirqNewPublicVirtualInterface :: Lens' CreatePublicVirtualInterface NewPublicVirtualInterface
cpvirqNewPublicVirtualInterface = lens _cpvirqNewPublicVirtualInterface (\ s a -> s{_cpvirqNewPublicVirtualInterface = a});

instance AWSRequest CreatePublicVirtualInterface
         where
        type Sv CreatePublicVirtualInterface = DirectConnect
        type Rs CreatePublicVirtualInterface =
             VirtualInterface
        request = postJSON
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
              ["connectionId" .= _cpvirqConnectionId,
               "newPublicVirtualInterface" .=
                 _cpvirqNewPublicVirtualInterface]

instance ToPath CreatePublicVirtualInterface where
        toPath = const "/"

instance ToQuery CreatePublicVirtualInterface where
        toQuery = const mempty
