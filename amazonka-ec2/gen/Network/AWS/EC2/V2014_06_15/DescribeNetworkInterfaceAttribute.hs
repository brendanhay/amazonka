{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes a network interface attribute. You can specify only one attribute
-- at a time. Example This example describes the sourceDestCheck attribute of
-- the specified network interface.
-- https://ec2.amazonaws.com/?Action=DescribeNetworkInterfaceAttribute
-- &amp;NetworkInterfaceId=eni-686ea200 &amp;Attribute=sourceDestCheck
-- &amp;AUTHPARAMS &lt;DescribeNetworkInterfaceAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a20c6b2-d71c-45fb-bba7-37306850544b&lt;/requestId&gt;
-- &lt;networkInterfaceId&gt;eni-686ea200&lt;/networkInterfaceId&gt;
-- &lt;sourceDestCheck&gt; &lt;value&gt;true&lt;/value&gt;
-- &lt;/sourceDestCheck&gt;
-- &lt;/DescribeNetworkInterfaceAttributeResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DescribeNetworkInterfaceAttribute
    (
    -- * Request
      DescribeNetworkInterfaceAttribute
    -- ** Request constructor
    , describeNetworkInterfaceAttribute
    -- ** Request lenses
    , dniarNetworkInterfaceId
    , dniarAttribute

    -- * Response
    , DescribeNetworkInterfaceAttributeResponse
    -- ** Response lenses
    , dniasSourceDestCheck
    , dniasDescription
    , dniasGroups
    , dniasAttachment
    , dniasNetworkInterfaceId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeNetworkInterfaceAttribute' request.
describeNetworkInterfaceAttribute :: Text -- ^ 'dniarNetworkInterfaceId'
                                  -> DescribeNetworkInterfaceAttribute
describeNetworkInterfaceAttribute p1 = DescribeNetworkInterfaceAttribute
    { _dniarNetworkInterfaceId = p1
    , _dniarAttribute = Nothing
    }

data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute
    { _dniarNetworkInterfaceId :: Text
      -- ^ The ID of the network interface.
    , _dniarAttribute :: Maybe NetworkInterfaceAttribute
      -- ^ The attribute of the network interface.
    } deriving (Show, Generic)

-- | The ID of the network interface.
dniarNetworkInterfaceId
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeNetworkInterfaceAttribute
    -> f DescribeNetworkInterfaceAttribute
dniarNetworkInterfaceId f x =
    (\y -> x { _dniarNetworkInterfaceId = y })
       <$> f (_dniarNetworkInterfaceId x)
{-# INLINE dniarNetworkInterfaceId #-}

-- | The attribute of the network interface.
dniarAttribute
    :: Functor f
    => (Maybe NetworkInterfaceAttribute
    -> f (Maybe NetworkInterfaceAttribute))
    -> DescribeNetworkInterfaceAttribute
    -> f DescribeNetworkInterfaceAttribute
dniarAttribute f x =
    (\y -> x { _dniarAttribute = y })
       <$> f (_dniarAttribute x)
{-# INLINE dniarAttribute #-}

instance ToQuery DescribeNetworkInterfaceAttribute where
    toQuery = genericQuery def

data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse
    { _dniasSourceDestCheck :: Maybe AttributeBooleanValue
      -- ^ Indicates whether source/destination checking is enabled.
    , _dniasDescription :: Maybe AttributeValue
      -- ^ The description of the network interface.
    , _dniasGroups :: [GroupIdentifier]
      -- ^ The security groups associated with the network interface.
    , _dniasAttachment :: Maybe NetworkInterfaceAttachment
      -- ^ The attachment (if any) of the network interface.
    , _dniasNetworkInterfaceId :: Maybe Text
      -- ^ The ID of the network interface.
    } deriving (Show, Generic)

-- | Indicates whether source/destination checking is enabled.
dniasSourceDestCheck
    :: Functor f
    => (Maybe AttributeBooleanValue
    -> f (Maybe AttributeBooleanValue))
    -> DescribeNetworkInterfaceAttributeResponse
    -> f DescribeNetworkInterfaceAttributeResponse
dniasSourceDestCheck f x =
    (\y -> x { _dniasSourceDestCheck = y })
       <$> f (_dniasSourceDestCheck x)
{-# INLINE dniasSourceDestCheck #-}

-- | The description of the network interface.
dniasDescription
    :: Functor f
    => (Maybe AttributeValue
    -> f (Maybe AttributeValue))
    -> DescribeNetworkInterfaceAttributeResponse
    -> f DescribeNetworkInterfaceAttributeResponse
dniasDescription f x =
    (\y -> x { _dniasDescription = y })
       <$> f (_dniasDescription x)
{-# INLINE dniasDescription #-}

-- | The security groups associated with the network interface.
dniasGroups
    :: Functor f
    => ([GroupIdentifier]
    -> f ([GroupIdentifier]))
    -> DescribeNetworkInterfaceAttributeResponse
    -> f DescribeNetworkInterfaceAttributeResponse
dniasGroups f x =
    (\y -> x { _dniasGroups = y })
       <$> f (_dniasGroups x)
{-# INLINE dniasGroups #-}

-- | The attachment (if any) of the network interface.
dniasAttachment
    :: Functor f
    => (Maybe NetworkInterfaceAttachment
    -> f (Maybe NetworkInterfaceAttachment))
    -> DescribeNetworkInterfaceAttributeResponse
    -> f DescribeNetworkInterfaceAttributeResponse
dniasAttachment f x =
    (\y -> x { _dniasAttachment = y })
       <$> f (_dniasAttachment x)
{-# INLINE dniasAttachment #-}

-- | The ID of the network interface.
dniasNetworkInterfaceId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeNetworkInterfaceAttributeResponse
    -> f DescribeNetworkInterfaceAttributeResponse
dniasNetworkInterfaceId f x =
    (\y -> x { _dniasNetworkInterfaceId = y })
       <$> f (_dniasNetworkInterfaceId x)
{-# INLINE dniasNetworkInterfaceId #-}

instance FromXML DescribeNetworkInterfaceAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeNetworkInterfaceAttribute where
    type Sv DescribeNetworkInterfaceAttribute = EC2
    type Rs DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttributeResponse

    request = post "DescribeNetworkInterfaceAttribute"
    response _ = xmlResponse
