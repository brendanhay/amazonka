{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeNetworkInterfaceAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes a network interface attribute. You can specify only one attribute
-- at a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeNetworkInterfaceAttribute.html>
module Network.AWS.EC2.DescribeNetworkInterfaceAttribute
    (
    -- * Request
      DescribeNetworkInterfaceAttribute
    -- ** Request constructor
    , describeNetworkInterfaceAttribute
    -- ** Request lenses
    , dniaAttribute
    , dniaDryRun
    , dniaNetworkInterfaceId

    -- * Response
    , DescribeNetworkInterfaceAttributeResponse
    -- ** Response constructor
    , describeNetworkInterfaceAttributeResponse
    -- ** Response lenses
    , dniarAttachment
    , dniarDescription
    , dniarGroups
    , dniarNetworkInterfaceId
    , dniarSourceDestCheck
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttribute
    { _dniaAttribute          :: Maybe NetworkInterfaceAttribute
    , _dniaDryRun             :: Maybe Bool
    , _dniaNetworkInterfaceId :: Text
    } deriving (Eq, Read, Show)

-- | 'DescribeNetworkInterfaceAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dniaAttribute' @::@ 'Maybe' 'NetworkInterfaceAttribute'
--
-- * 'dniaDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dniaNetworkInterfaceId' @::@ 'Text'
--
describeNetworkInterfaceAttribute :: Text -- ^ 'dniaNetworkInterfaceId'
                                  -> DescribeNetworkInterfaceAttribute
describeNetworkInterfaceAttribute p1 = DescribeNetworkInterfaceAttribute
    { _dniaNetworkInterfaceId = p1
    , _dniaDryRun             = Nothing
    , _dniaAttribute          = Nothing
    }

-- | The attribute of the network interface.
dniaAttribute :: Lens' DescribeNetworkInterfaceAttribute (Maybe NetworkInterfaceAttribute)
dniaAttribute = lens _dniaAttribute (\s a -> s { _dniaAttribute = a })

dniaDryRun :: Lens' DescribeNetworkInterfaceAttribute (Maybe Bool)
dniaDryRun = lens _dniaDryRun (\s a -> s { _dniaDryRun = a })

-- | The ID of the network interface.
dniaNetworkInterfaceId :: Lens' DescribeNetworkInterfaceAttribute Text
dniaNetworkInterfaceId =
    lens _dniaNetworkInterfaceId (\s a -> s { _dniaNetworkInterfaceId = a })

data DescribeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse
    { _dniarAttachment         :: Maybe NetworkInterfaceAttachment
    , _dniarDescription        :: Maybe AttributeValue
    , _dniarGroups             :: List "item" GroupIdentifier
    , _dniarNetworkInterfaceId :: Maybe Text
    , _dniarSourceDestCheck    :: Maybe AttributeBooleanValue
    } deriving (Eq, Read, Show)

-- | 'DescribeNetworkInterfaceAttributeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dniarAttachment' @::@ 'Maybe' 'NetworkInterfaceAttachment'
--
-- * 'dniarDescription' @::@ 'Maybe' 'AttributeValue'
--
-- * 'dniarGroups' @::@ ['GroupIdentifier']
--
-- * 'dniarNetworkInterfaceId' @::@ 'Maybe' 'Text'
--
-- * 'dniarSourceDestCheck' @::@ 'Maybe' 'AttributeBooleanValue'
--
describeNetworkInterfaceAttributeResponse :: DescribeNetworkInterfaceAttributeResponse
describeNetworkInterfaceAttributeResponse = DescribeNetworkInterfaceAttributeResponse
    { _dniarNetworkInterfaceId = Nothing
    , _dniarDescription        = Nothing
    , _dniarSourceDestCheck    = Nothing
    , _dniarGroups             = mempty
    , _dniarAttachment         = Nothing
    }

-- | The attachment (if any) of the network interface.
dniarAttachment :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe NetworkInterfaceAttachment)
dniarAttachment = lens _dniarAttachment (\s a -> s { _dniarAttachment = a })

-- | The description of the network interface.
dniarDescription :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe AttributeValue)
dniarDescription = lens _dniarDescription (\s a -> s { _dniarDescription = a })

-- | The security groups associated with the network interface.
dniarGroups :: Lens' DescribeNetworkInterfaceAttributeResponse [GroupIdentifier]
dniarGroups = lens _dniarGroups (\s a -> s { _dniarGroups = a }) . _List

-- | The ID of the network interface.
dniarNetworkInterfaceId :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe Text)
dniarNetworkInterfaceId =
    lens _dniarNetworkInterfaceId (\s a -> s { _dniarNetworkInterfaceId = a })

-- | Indicates whether source/destination checking is enabled.
dniarSourceDestCheck :: Lens' DescribeNetworkInterfaceAttributeResponse (Maybe AttributeBooleanValue)
dniarSourceDestCheck =
    lens _dniarSourceDestCheck (\s a -> s { _dniarSourceDestCheck = a })

instance ToPath DescribeNetworkInterfaceAttribute where
    toPath = const "/"

instance ToQuery DescribeNetworkInterfaceAttribute where
    toQuery DescribeNetworkInterfaceAttribute{..} = mconcat
        [ "Attribute"          =? _dniaAttribute
        , "DryRun"             =? _dniaDryRun
        , "NetworkInterfaceId" =? _dniaNetworkInterfaceId
        ]

instance ToHeaders DescribeNetworkInterfaceAttribute

instance AWSRequest DescribeNetworkInterfaceAttribute where
    type Sv DescribeNetworkInterfaceAttribute = EC2
    type Rs DescribeNetworkInterfaceAttribute = DescribeNetworkInterfaceAttributeResponse

    request  = post "DescribeNetworkInterfaceAttribute"
    response = xmlResponse

instance FromXML DescribeNetworkInterfaceAttributeResponse where
    parseXML x = DescribeNetworkInterfaceAttributeResponse
        <$> x .@? "attachment"
        <*> x .@? "description"
        <*> x .@? "groupSet" .!@ mempty
        <*> x .@? "networkInterfaceId"
        <*> x .@? "sourceDestCheck"
