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
    , mkDescribeNetworkInterfaceAttributeRequest
    -- ** Request lenses
    , dniarNetworkInterfaceId
    , dniarAttribute

    -- * Response
    , DescribeNetworkInterfaceAttributeResponse
    -- ** Response lenses
    , dniasNetworkInterfaceId
    , dniasDescription
    , dniasSourceDestCheck
    , dniasGroups
    , dniasAttachment
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeNetworkInterfaceAttribute' request.
mkDescribeNetworkInterfaceAttributeRequest :: Text -- ^ 'dniarNetworkInterfaceId'
                                           -> DescribeNetworkInterfaceAttribute
mkDescribeNetworkInterfaceAttributeRequest p1 = DescribeNetworkInterfaceAttribute
    { _dniarNetworkInterfaceId = p1
    , _dniarAttribute = Nothing
    }
{-# INLINE mkDescribeNetworkInterfaceAttributeRequest #-}

data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute
    { _dniarNetworkInterfaceId :: Text
      -- ^ The ID of the network interface.
    , _dniarAttribute :: Maybe NetworkInterfaceAttribute
      -- ^ The attribute of the network interface.
    } deriving (Show, Generic)

-- | The ID of the network interface.
dniarNetworkInterfaceId :: Lens' DescribeNetworkInterfaceAttribute (Text)
dniarNetworkInterfaceId = lens _dniarNetworkInterfaceId (\s a -> s { _dniarNetworkInterfaceId = a })
{-# INLINE dniarNetworkInterfaceId #-}

-- | The attribute of the network interface.
dniarAttribute :: Lens' DescribeNetworkInterfaceAttribute (Maybe NetworkInterfaceAttribute)
dniarAttribute = lens _dniarAttribute (\s a -> s { _dniarAttribute = a })
{-# INLINE dniarAttribute #-}

instance ToQuery DescribeNetworkInterfaceAttribute where
    toQuery = genericQuery def

data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse
    { _dniasNetworkInterfaceId :: Maybe Text
      -- ^ The ID of the network interface.
    , _dniasDescription :: Maybe AttributeValue
      -- ^ The description of the network interface.
    , _dniasSourceDestCheck :: Maybe AttributeBooleanValue
      -- ^ Indicates whether source/destination checking is enabled.
    , _dniasGroups :: [GroupIdentifier]
      -- ^ The security groups associated with the network interface.
    , _dniasAttachment :: Maybe NetworkInterfaceAttachment
      -- ^ The attachment (if any) of the network interface.
    } deriving (Show, Generic)

-- | The ID of the network interface.
dniasNetworkInterfaceId :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe Text)
dniasNetworkInterfaceId = lens _dniasNetworkInterfaceId (\s a -> s { _dniasNetworkInterfaceId = a })
{-# INLINE dniasNetworkInterfaceId #-}

-- | The description of the network interface.
dniasDescription :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe AttributeValue)
dniasDescription = lens _dniasDescription (\s a -> s { _dniasDescription = a })
{-# INLINE dniasDescription #-}

-- | Indicates whether source/destination checking is enabled.
dniasSourceDestCheck :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe AttributeBooleanValue)
dniasSourceDestCheck = lens _dniasSourceDestCheck (\s a -> s { _dniasSourceDestCheck = a })
{-# INLINE dniasSourceDestCheck #-}

-- | The security groups associated with the network interface.
dniasGroups :: Lens' DescribeNetworkInterfaceAttributeResponse ([GroupIdentifier])
dniasGroups = lens _dniasGroups (\s a -> s { _dniasGroups = a })
{-# INLINE dniasGroups #-}

-- | The attachment (if any) of the network interface.
dniasAttachment :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe NetworkInterfaceAttachment)
dniasAttachment = lens _dniasAttachment (\s a -> s { _dniasAttachment = a })
{-# INLINE dniasAttachment #-}

instance FromXML DescribeNetworkInterfaceAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeNetworkInterfaceAttribute where
    type Sv DescribeNetworkInterfaceAttribute = EC2
    type Rs DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttributeResponse

    request = post "DescribeNetworkInterfaceAttribute"
    response _ = xmlResponse
