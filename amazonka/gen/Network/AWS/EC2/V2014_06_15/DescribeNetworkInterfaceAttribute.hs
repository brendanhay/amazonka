{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a20c6b2-d71c-45fb-bba7-37306850544b&lt;/requestId&gt;
-- &lt;networkInterfaceId&gt;eni-686ea200&lt;/networkInterfaceId&gt;
-- &lt;sourceDestCheck&gt; &lt;value&gt;true&lt;/value&gt;
-- &lt;/sourceDestCheck&gt;
-- &lt;/DescribeNetworkInterfaceAttributeResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DescribeNetworkInterfaceAttribute where

import Control.Lens
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeNetworkInterfaceAttribute' request.
describeNetworkInterfaceAttribute :: Text -- ^ '_dniarNetworkInterfaceId'
                                  -> DescribeNetworkInterfaceAttribute
describeNetworkInterfaceAttribute p1 = DescribeNetworkInterfaceAttribute
    { _dniarNetworkInterfaceId = p1
    , _dniarDryRun = Nothing
    , _dniarGroups = Nothing
    , _dniarSourceDestCheck = Nothing
    , _dniarAttachment = Nothing
    , _dniarDescription = Nothing
    }

data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute
    { _dniarNetworkInterfaceId :: Text
      -- ^ The ID of the network interface.
    , _dniarDryRun :: Maybe Bool
      -- ^ 
    , _dniarGroups :: Maybe Text
      -- ^ The groupSet attribute.
    , _dniarSourceDestCheck :: Maybe Text
      -- ^ The sourceDestCheck attribute.
    , _dniarAttachment :: Maybe Text
      -- ^ The attachment attribute.
    , _dniarDescription :: Maybe Text
      -- ^ The description attribute.
    } deriving (Generic)

makeLenses ''DescribeNetworkInterfaceAttribute

instance ToQuery DescribeNetworkInterfaceAttribute where
    toQuery = genericToQuery def

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
    } deriving (Generic)

makeLenses ''DescribeNetworkInterfaceAttributeResponse

instance FromXML DescribeNetworkInterfaceAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeNetworkInterfaceAttribute where
    type Sv DescribeNetworkInterfaceAttribute = EC2
    type Rs DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttributeResponse

    request = post "DescribeNetworkInterfaceAttribute"
    response _ = xmlResponse
