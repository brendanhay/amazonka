{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.DirectConnect.V2012_10_25.AllocatePublicVirtualInterface
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provisions a public virtual interface to be owned by a different customer.
-- The owner of a connection calls this function to provision a public virtual
-- interface which will be owned by another AWS customer. Virtual interfaces
-- created using this function must be confirmed by the virtual interface
-- owner by calling ConfirmPublicVirtualInterface. Until this step has been
-- completed, the virtual interface will be in 'Confirming' state, and will
-- not be available for handling traffic.
module Network.AWS.DirectConnect.V2012_10_25.AllocatePublicVirtualInterface
    (
    -- * Request
      AllocatePublicVirtualInterface
    -- ** Request constructor
    , allocatePublicVirtualInterface
    -- ** Request lenses
    , apvisConnectionId
    , apvisNewPublicVirtualInterfaceAllocation
    , apvisOwnerAccount

    -- * Response
    , AllocatePublicVirtualInterfaceResponse
    -- ** Response lenses
    , vjAsn
    , vjAmazonAddress
    , vjAuthKey
    , vjConnectionId
    , vjCustomerAddress
    , vjLocation
    , vjOwnerAccount
    , vjRouteFilterPrefixes
    , vjCustomerRouterConfig
    , vjVlan
    , vjVirtualGatewayId
    , vjVirtualInterfaceId
    , vjVirtualInterfaceName
    , vjVirtualInterfaceState
    , vjVirtualInterfaceType
    ) where

import           Network.AWS.DirectConnect.V2012_10_25.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'AllocatePublicVirtualInterface' request.
allocatePublicVirtualInterface :: Text -- ^ 'apvisConnectionId'
                               -> NewPublicVirtualInterfaceAllocation -- ^ 'apvisNewPublicVirtualInterfaceAllocation'
                               -> Text -- ^ 'apvisOwnerAccount'
                               -> AllocatePublicVirtualInterface
allocatePublicVirtualInterface p1 p2 p3 = AllocatePublicVirtualInterface
    { _apvisConnectionId = p1
    , _apvisNewPublicVirtualInterfaceAllocation = p2
    , _apvisOwnerAccount = p3
    }

data AllocatePublicVirtualInterface = AllocatePublicVirtualInterface
    { _apvisConnectionId :: Text
      -- ^ The connection ID on which the public virtual interface is
      -- provisioned. Default: None.
    , _apvisNewPublicVirtualInterfaceAllocation :: NewPublicVirtualInterfaceAllocation
      -- ^ Detailed information for the public virtual interface to be
      -- provisioned. Default: None.
    , _apvisOwnerAccount :: Text
      -- ^ The AWS account that will own the new public virtual interface.
      -- Default: None.
    } deriving (Show, Generic)

-- | The connection ID on which the public virtual interface is provisioned.
-- Default: None.
apvisConnectionId
    :: Functor f
    => (Text
    -> f (Text))
    -> AllocatePublicVirtualInterface
    -> f AllocatePublicVirtualInterface
apvisConnectionId f x =
    (\y -> x { _apvisConnectionId = y })
       <$> f (_apvisConnectionId x)
{-# INLINE apvisConnectionId #-}

-- | Detailed information for the public virtual interface to be provisioned.
-- Default: None.
apvisNewPublicVirtualInterfaceAllocation
    :: Functor f
    => (NewPublicVirtualInterfaceAllocation
    -> f (NewPublicVirtualInterfaceAllocation))
    -> AllocatePublicVirtualInterface
    -> f AllocatePublicVirtualInterface
apvisNewPublicVirtualInterfaceAllocation f x =
    (\y -> x { _apvisNewPublicVirtualInterfaceAllocation = y })
       <$> f (_apvisNewPublicVirtualInterfaceAllocation x)
{-# INLINE apvisNewPublicVirtualInterfaceAllocation #-}

-- | The AWS account that will own the new public virtual interface. Default:
-- None.
apvisOwnerAccount
    :: Functor f
    => (Text
    -> f (Text))
    -> AllocatePublicVirtualInterface
    -> f AllocatePublicVirtualInterface
apvisOwnerAccount f x =
    (\y -> x { _apvisOwnerAccount = y })
       <$> f (_apvisOwnerAccount x)
{-# INLINE apvisOwnerAccount #-}

instance ToPath AllocatePublicVirtualInterface

instance ToQuery AllocatePublicVirtualInterface

instance ToHeaders AllocatePublicVirtualInterface

instance ToJSON AllocatePublicVirtualInterface

data AllocatePublicVirtualInterfaceResponse = AllocatePublicVirtualInterfaceResponse
    { _vjAsn :: Maybe Integer
      -- ^ Autonomous system (AS) number for Border Gateway Protocol (BGP)
      -- configuration. Example: 65000.
    , _vjAmazonAddress :: Maybe Text
      -- ^ IP address assigned to the Amazon interface. Example:
      -- 192.168.1.1/30.
    , _vjAuthKey :: Maybe Text
      -- ^ Authentication key for BGP configuration. Example: asdf34example.
    , _vjConnectionId :: Maybe Text
      -- ^ ID of the connection. Example: dxcon-fg5678gh Default: None.
    , _vjCustomerAddress :: Maybe Text
      -- ^ IP address assigned to the customer interface. Example:
      -- 192.168.1.2/30.
    , _vjLocation :: Maybe Text
      -- ^ Where the connection is located. Example: EqSV5 Default: None.
    , _vjOwnerAccount :: Maybe Text
    , _vjRouteFilterPrefixes :: [RouteFilterPrefix]
      -- ^ A list of routes to be advertised to the AWS network in this
      -- region (public virtual interface) or your VPC (private virtual
      -- interface).
    , _vjCustomerRouterConfig :: Maybe Text
      -- ^ Information for generating the customer router configuration.
    , _vjVlan :: Maybe Integer
      -- ^ The VLAN ID. Example: 101.
    , _vjVirtualGatewayId :: Maybe Text
      -- ^ The ID of the virtual private gateway to a VPC. This only applies
      -- to private virtual interfaces. Example: vgw-123er56.
    , _vjVirtualInterfaceId :: Maybe Text
      -- ^ ID of the virtual interface. Example: dxvif-123dfg56 Default:
      -- None.
    , _vjVirtualInterfaceName :: Maybe Text
      -- ^ The name of the virtual interface assigned by the customer.
      -- Example: "My VPC".
    , _vjVirtualInterfaceState :: Maybe VirtualInterfaceState
      -- ^ State of the virtual interface. Confirming: The creation of the
      -- virtual interface is pending confirmation from the virtual
      -- interface owner. If the owner of the virtual interface is
      -- different from the owner of the connection on which it is
      -- provisioned, then the virtual interface will remain in this state
      -- until it is confirmed by the virtual interface owner. Verifying:
      -- This state only applies to public virtual interfaces. Each public
      -- virtual interface needs validation before the virtual interface
      -- can be created. Pending: A virtual interface is in this state
      -- from the time that it is created until the virtual interface is
      -- ready to forward traffic. Available: A virtual interface that is
      -- able to forward traffic. Deleting: A virtual interface is in this
      -- state immediately after calling DeleteVirtualInterface until it
      -- can no longer forward traffic. Deleted: A virtual interface that
      -- cannot forward traffic. Rejected: The virtual interface owner has
      -- declined creation of the virtual interface. If a virtual
      -- interface in the 'Confirming' state is deleted by the virtual
      -- interface owner, the virtual interface will enter the 'Rejected'
      -- state.
    , _vjVirtualInterfaceType :: Maybe Text
      -- ^ The type of virtual interface. Example: private (Amazon VPC) or
      -- public (Amazon S3, Amazon DynamoDB, and so on.).
    } deriving (Show, Generic)

-- | Autonomous system (AS) number for Border Gateway Protocol (BGP)
-- configuration. Example: 65000.
vjAsn
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> AllocatePublicVirtualInterfaceResponse
    -> f AllocatePublicVirtualInterfaceResponse
vjAsn f x =
    (\y -> x { _vjAsn = y })
       <$> f (_vjAsn x)
{-# INLINE vjAsn #-}

-- | IP address assigned to the Amazon interface. Example: 192.168.1.1/30.
vjAmazonAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AllocatePublicVirtualInterfaceResponse
    -> f AllocatePublicVirtualInterfaceResponse
vjAmazonAddress f x =
    (\y -> x { _vjAmazonAddress = y })
       <$> f (_vjAmazonAddress x)
{-# INLINE vjAmazonAddress #-}

-- | Authentication key for BGP configuration. Example: asdf34example.
vjAuthKey
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AllocatePublicVirtualInterfaceResponse
    -> f AllocatePublicVirtualInterfaceResponse
vjAuthKey f x =
    (\y -> x { _vjAuthKey = y })
       <$> f (_vjAuthKey x)
{-# INLINE vjAuthKey #-}

-- | ID of the connection. Example: dxcon-fg5678gh Default: None.
vjConnectionId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AllocatePublicVirtualInterfaceResponse
    -> f AllocatePublicVirtualInterfaceResponse
vjConnectionId f x =
    (\y -> x { _vjConnectionId = y })
       <$> f (_vjConnectionId x)
{-# INLINE vjConnectionId #-}

-- | IP address assigned to the customer interface. Example: 192.168.1.2/30.
vjCustomerAddress
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AllocatePublicVirtualInterfaceResponse
    -> f AllocatePublicVirtualInterfaceResponse
vjCustomerAddress f x =
    (\y -> x { _vjCustomerAddress = y })
       <$> f (_vjCustomerAddress x)
{-# INLINE vjCustomerAddress #-}

-- | Where the connection is located. Example: EqSV5 Default: None.
vjLocation
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AllocatePublicVirtualInterfaceResponse
    -> f AllocatePublicVirtualInterfaceResponse
vjLocation f x =
    (\y -> x { _vjLocation = y })
       <$> f (_vjLocation x)
{-# INLINE vjLocation #-}

vjOwnerAccount
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AllocatePublicVirtualInterfaceResponse
    -> f AllocatePublicVirtualInterfaceResponse
vjOwnerAccount f x =
    (\y -> x { _vjOwnerAccount = y })
       <$> f (_vjOwnerAccount x)
{-# INLINE vjOwnerAccount #-}

-- | A list of routes to be advertised to the AWS network in this region (public
-- virtual interface) or your VPC (private virtual interface).
vjRouteFilterPrefixes
    :: Functor f
    => ([RouteFilterPrefix]
    -> f ([RouteFilterPrefix]))
    -> AllocatePublicVirtualInterfaceResponse
    -> f AllocatePublicVirtualInterfaceResponse
vjRouteFilterPrefixes f x =
    (\y -> x { _vjRouteFilterPrefixes = y })
       <$> f (_vjRouteFilterPrefixes x)
{-# INLINE vjRouteFilterPrefixes #-}

-- | Information for generating the customer router configuration.
vjCustomerRouterConfig
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AllocatePublicVirtualInterfaceResponse
    -> f AllocatePublicVirtualInterfaceResponse
vjCustomerRouterConfig f x =
    (\y -> x { _vjCustomerRouterConfig = y })
       <$> f (_vjCustomerRouterConfig x)
{-# INLINE vjCustomerRouterConfig #-}

-- | The VLAN ID. Example: 101.
vjVlan
    :: Functor f
    => (Maybe Integer
    -> f (Maybe Integer))
    -> AllocatePublicVirtualInterfaceResponse
    -> f AllocatePublicVirtualInterfaceResponse
vjVlan f x =
    (\y -> x { _vjVlan = y })
       <$> f (_vjVlan x)
{-# INLINE vjVlan #-}

-- | The ID of the virtual private gateway to a VPC. This only applies to
-- private virtual interfaces. Example: vgw-123er56.
vjVirtualGatewayId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AllocatePublicVirtualInterfaceResponse
    -> f AllocatePublicVirtualInterfaceResponse
vjVirtualGatewayId f x =
    (\y -> x { _vjVirtualGatewayId = y })
       <$> f (_vjVirtualGatewayId x)
{-# INLINE vjVirtualGatewayId #-}

-- | ID of the virtual interface. Example: dxvif-123dfg56 Default: None.
vjVirtualInterfaceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AllocatePublicVirtualInterfaceResponse
    -> f AllocatePublicVirtualInterfaceResponse
vjVirtualInterfaceId f x =
    (\y -> x { _vjVirtualInterfaceId = y })
       <$> f (_vjVirtualInterfaceId x)
{-# INLINE vjVirtualInterfaceId #-}

-- | The name of the virtual interface assigned by the customer. Example: "My
-- VPC".
vjVirtualInterfaceName
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AllocatePublicVirtualInterfaceResponse
    -> f AllocatePublicVirtualInterfaceResponse
vjVirtualInterfaceName f x =
    (\y -> x { _vjVirtualInterfaceName = y })
       <$> f (_vjVirtualInterfaceName x)
{-# INLINE vjVirtualInterfaceName #-}

-- | State of the virtual interface. Confirming: The creation of the virtual
-- interface is pending confirmation from the virtual interface owner. If the
-- owner of the virtual interface is different from the owner of the
-- connection on which it is provisioned, then the virtual interface will
-- remain in this state until it is confirmed by the virtual interface owner.
-- Verifying: This state only applies to public virtual interfaces. Each
-- public virtual interface needs validation before the virtual interface can
-- be created. Pending: A virtual interface is in this state from the time
-- that it is created until the virtual interface is ready to forward traffic.
-- Available: A virtual interface that is able to forward traffic. Deleting: A
-- virtual interface is in this state immediately after calling
-- DeleteVirtualInterface until it can no longer forward traffic. Deleted: A
-- virtual interface that cannot forward traffic. Rejected: The virtual
-- interface owner has declined creation of the virtual interface. If a
-- virtual interface in the 'Confirming' state is deleted by the virtual
-- interface owner, the virtual interface will enter the 'Rejected' state.
vjVirtualInterfaceState
    :: Functor f
    => (Maybe VirtualInterfaceState
    -> f (Maybe VirtualInterfaceState))
    -> AllocatePublicVirtualInterfaceResponse
    -> f AllocatePublicVirtualInterfaceResponse
vjVirtualInterfaceState f x =
    (\y -> x { _vjVirtualInterfaceState = y })
       <$> f (_vjVirtualInterfaceState x)
{-# INLINE vjVirtualInterfaceState #-}

-- | The type of virtual interface. Example: private (Amazon VPC) or public
-- (Amazon S3, Amazon DynamoDB, and so on.).
vjVirtualInterfaceType
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> AllocatePublicVirtualInterfaceResponse
    -> f AllocatePublicVirtualInterfaceResponse
vjVirtualInterfaceType f x =
    (\y -> x { _vjVirtualInterfaceType = y })
       <$> f (_vjVirtualInterfaceType x)
{-# INLINE vjVirtualInterfaceType #-}

instance FromJSON AllocatePublicVirtualInterfaceResponse

instance AWSRequest AllocatePublicVirtualInterface where
    type Sv AllocatePublicVirtualInterface = DirectConnect
    type Rs AllocatePublicVirtualInterface = AllocatePublicVirtualInterfaceResponse

    request = get
    response _ = jsonResponse
