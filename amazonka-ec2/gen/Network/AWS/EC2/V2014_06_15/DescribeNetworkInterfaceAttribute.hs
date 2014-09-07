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
    , mkDescribeNetworkInterfaceAttribute
    -- ** Request lenses
    , dniaNetworkInterfaceId
    , dniaAttribute

    -- * Response
    , DescribeNetworkInterfaceAttributeResponse
    -- ** Response lenses
    , dniarsNetworkInterfaceId
    , dniarsDescription
    , dniarsSourceDestCheck
    , dniarsGroups
    , dniarsAttachment
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute
    { _dniaNetworkInterfaceId :: Text
    , _dniaAttribute :: Maybe NetworkInterfaceAttribute
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeNetworkInterfaceAttribute' request.
mkDescribeNetworkInterfaceAttribute :: Text -- ^ 'dniaNetworkInterfaceId'
                                    -> DescribeNetworkInterfaceAttribute
mkDescribeNetworkInterfaceAttribute p1 = DescribeNetworkInterfaceAttribute
    { _dniaNetworkInterfaceId = p1
    , _dniaAttribute = Nothing
    }

-- | The ID of the network interface.
dniaNetworkInterfaceId :: Lens' DescribeNetworkInterfaceAttribute Text
dniaNetworkInterfaceId =
    lens _dniaNetworkInterfaceId (\s a -> s { _dniaNetworkInterfaceId = a })

-- | The attribute of the network interface.
dniaAttribute :: Lens' DescribeNetworkInterfaceAttribute (Maybe NetworkInterfaceAttribute)
dniaAttribute = lens _dniaAttribute (\s a -> s { _dniaAttribute = a })

instance ToQuery DescribeNetworkInterfaceAttribute where
    toQuery = genericQuery def

-- | 
data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse
    { _dniarsNetworkInterfaceId :: Maybe Text
    , _dniarsDescription :: Maybe AttributeValue
    , _dniarsSourceDestCheck :: Maybe AttributeBooleanValue
    , _dniarsGroups :: [GroupIdentifier]
    , _dniarsAttachment :: Maybe NetworkInterfaceAttachment
    } deriving (Show, Generic)

-- | The ID of the network interface.
dniarsNetworkInterfaceId :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe Text)
dniarsNetworkInterfaceId =
    lens _dniarsNetworkInterfaceId
         (\s a -> s { _dniarsNetworkInterfaceId = a })

-- | The description of the network interface.
dniarsDescription :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe AttributeValue)
dniarsDescription =
    lens _dniarsDescription (\s a -> s { _dniarsDescription = a })

-- | Indicates whether source/destination checking is enabled.
dniarsSourceDestCheck :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe AttributeBooleanValue)
dniarsSourceDestCheck =
    lens _dniarsSourceDestCheck (\s a -> s { _dniarsSourceDestCheck = a })

-- | The security groups associated with the network interface.
dniarsGroups :: Lens' DescribeNetworkInterfaceAttributeResponse [GroupIdentifier]
dniarsGroups = lens _dniarsGroups (\s a -> s { _dniarsGroups = a })

-- | The attachment (if any) of the network interface.
dniarsAttachment :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe NetworkInterfaceAttachment)
dniarsAttachment =
    lens _dniarsAttachment (\s a -> s { _dniarsAttachment = a })

instance FromXML DescribeNetworkInterfaceAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeNetworkInterfaceAttribute where
    type Sv DescribeNetworkInterfaceAttribute = EC2
    type Rs DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttributeResponse

    request = post "DescribeNetworkInterfaceAttribute"
    response _ = xmlResponse
