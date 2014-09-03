{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ModifyNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies the specified network interface attribute. You can specify only
-- one attribute at a time. Example This example sets source/destination
-- checking to false for the specified network interface.
-- https://ec2.amazonaws.com/?Action=ModifyNetworkInterfaceAttribute
-- &amp;NetworkInterfaceId=eni-ffda3197 &amp;SourceDestCheck.Value=false
-- &amp;AUTHPARAMS &lt;ModifyNetworkInterfaceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;657a4623-5620-4232-b03b-427e852d71cf&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/ModifyNetworkInterfaceAttributeResponse&gt;.
module Network.AWS.EC2.V2014_06_15.ModifyNetworkInterfaceAttribute
    (
    -- * Request
      ModifyNetworkInterfaceAttribute
    -- ** Request constructor
    , modifyNetworkInterfaceAttribute
    -- ** Request lenses
    , mniarNetworkInterfaceId
    , mniarSourceDestCheck
    , mniarDescription
    , mniarAttachment
    , mniarGroups

    -- * Response
    , ModifyNetworkInterfaceAttributeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ModifyNetworkInterfaceAttribute' request.
modifyNetworkInterfaceAttribute :: Text -- ^ 'mniarNetworkInterfaceId'
                                -> ModifyNetworkInterfaceAttribute
modifyNetworkInterfaceAttribute p1 = ModifyNetworkInterfaceAttribute
    { _mniarNetworkInterfaceId = p1
    , _mniarSourceDestCheck = Nothing
    , _mniarDescription = Nothing
    , _mniarAttachment = Nothing
    , _mniarGroups = mempty
    }

data ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttribute
    { _mniarNetworkInterfaceId :: Text
      -- ^ The ID of the network interface.
    , _mniarSourceDestCheck :: Maybe AttributeBooleanValue
      -- ^ Indicates whether source/destination checking is enabled. A value
      -- of true means checking is enabled, and false means checking is
      -- disabled. This value must be false for a NAT instance to perform
      -- NAT. For more information, see NAT Instances in the Amazon
      -- Virtual Private Cloud User Guide.
    , _mniarDescription :: Maybe AttributeValue
      -- ^ A description for the network interface.
    , _mniarAttachment :: Maybe NetworkInterfaceAttachmentChanges
      -- ^ The ID of the interface attachment.
    , _mniarGroups :: [Text]
      -- ^ Changes the security groups for the network interface. The new
      -- set of groups you specify replaces the current set. You must
      -- specify at least one group, even if it's just the default
      -- security group in the VPC. You must specify the ID of the
      -- security group, not the name.
    } deriving (Show, Generic)

-- | The ID of the network interface.
mniarNetworkInterfaceId
    :: Functor f
    => (Text
    -> f (Text))
    -> ModifyNetworkInterfaceAttribute
    -> f ModifyNetworkInterfaceAttribute
mniarNetworkInterfaceId f x =
    (\y -> x { _mniarNetworkInterfaceId = y })
       <$> f (_mniarNetworkInterfaceId x)
{-# INLINE mniarNetworkInterfaceId #-}

-- | Indicates whether source/destination checking is enabled. A value of true
-- means checking is enabled, and false means checking is disabled. This value
-- must be false for a NAT instance to perform NAT. For more information, see
-- NAT Instances in the Amazon Virtual Private Cloud User Guide.
mniarSourceDestCheck
    :: Functor f
    => (Maybe AttributeBooleanValue
    -> f (Maybe AttributeBooleanValue))
    -> ModifyNetworkInterfaceAttribute
    -> f ModifyNetworkInterfaceAttribute
mniarSourceDestCheck f x =
    (\y -> x { _mniarSourceDestCheck = y })
       <$> f (_mniarSourceDestCheck x)
{-# INLINE mniarSourceDestCheck #-}

-- | A description for the network interface.
mniarDescription
    :: Functor f
    => (Maybe AttributeValue
    -> f (Maybe AttributeValue))
    -> ModifyNetworkInterfaceAttribute
    -> f ModifyNetworkInterfaceAttribute
mniarDescription f x =
    (\y -> x { _mniarDescription = y })
       <$> f (_mniarDescription x)
{-# INLINE mniarDescription #-}

-- | The ID of the interface attachment.
mniarAttachment
    :: Functor f
    => (Maybe NetworkInterfaceAttachmentChanges
    -> f (Maybe NetworkInterfaceAttachmentChanges))
    -> ModifyNetworkInterfaceAttribute
    -> f ModifyNetworkInterfaceAttribute
mniarAttachment f x =
    (\y -> x { _mniarAttachment = y })
       <$> f (_mniarAttachment x)
{-# INLINE mniarAttachment #-}

-- | Changes the security groups for the network interface. The new set of
-- groups you specify replaces the current set. You must specify at least one
-- group, even if it's just the default security group in the VPC. You must
-- specify the ID of the security group, not the name.
mniarGroups
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ModifyNetworkInterfaceAttribute
    -> f ModifyNetworkInterfaceAttribute
mniarGroups f x =
    (\y -> x { _mniarGroups = y })
       <$> f (_mniarGroups x)
{-# INLINE mniarGroups #-}

instance ToQuery ModifyNetworkInterfaceAttribute where
    toQuery = genericQuery def

data ModifyNetworkInterfaceAttributeResponse = ModifyNetworkInterfaceAttributeResponse
    deriving (Eq, Show, Generic)

instance AWSRequest ModifyNetworkInterfaceAttribute where
    type Sv ModifyNetworkInterfaceAttribute = EC2
    type Rs ModifyNetworkInterfaceAttribute = ModifyNetworkInterfaceAttributeResponse

    request = post "ModifyNetworkInterfaceAttribute"
    response _ = nullaryResponse ModifyNetworkInterfaceAttributeResponse
