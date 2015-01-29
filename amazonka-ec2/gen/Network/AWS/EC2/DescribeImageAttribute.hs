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

-- Module      : Network.AWS.EC2.DescribeImageAttribute
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

-- | Describes the specified attribute of the specified AMI. You can specify only
-- one attribute at a time.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeImageAttribute.html>
module Network.AWS.EC2.DescribeImageAttribute
    (
    -- * Request
      DescribeImageAttribute
    -- ** Request constructor
    , describeImageAttribute
    -- ** Request lenses
    , dia1Attribute
    , dia1DryRun
    , dia1ImageId

    -- * Response
    , DescribeImageAttributeResponse
    -- ** Response constructor
    , describeImageAttributeResponse
    -- ** Response lenses
    , diarBlockDeviceMappings
    , diarDescription
    , diarImageId
    , diarKernelId
    , diarLaunchPermissions
    , diarProductCodes
    , diarRamdiskId
    , diarSriovNetSupport
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeImageAttribute = DescribeImageAttribute
    { _dia1Attribute :: ImageAttributeName
    , _dia1DryRun    :: Maybe Bool
    , _dia1ImageId   :: Text
    } deriving (Eq, Read, Show)

-- | 'DescribeImageAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dia1Attribute' @::@ 'ImageAttributeName'
--
-- * 'dia1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dia1ImageId' @::@ 'Text'
--
describeImageAttribute :: Text -- ^ 'dia1ImageId'
                       -> ImageAttributeName -- ^ 'dia1Attribute'
                       -> DescribeImageAttribute
describeImageAttribute p1 p2 = DescribeImageAttribute
    { _dia1ImageId   = p1
    , _dia1Attribute = p2
    , _dia1DryRun    = Nothing
    }

-- | The AMI attribute.
dia1Attribute :: Lens' DescribeImageAttribute ImageAttributeName
dia1Attribute = lens _dia1Attribute (\s a -> s { _dia1Attribute = a })

dia1DryRun :: Lens' DescribeImageAttribute (Maybe Bool)
dia1DryRun = lens _dia1DryRun (\s a -> s { _dia1DryRun = a })

-- | The ID of the AMI.
dia1ImageId :: Lens' DescribeImageAttribute Text
dia1ImageId = lens _dia1ImageId (\s a -> s { _dia1ImageId = a })

data DescribeImageAttributeResponse = DescribeImageAttributeResponse
    { _diarBlockDeviceMappings :: List "item" BlockDeviceMapping
    , _diarDescription         :: Maybe AttributeValue
    , _diarImageId             :: Maybe Text
    , _diarKernelId            :: Maybe AttributeValue
    , _diarLaunchPermissions   :: List "item" LaunchPermission
    , _diarProductCodes        :: List "item" ProductCode
    , _diarRamdiskId           :: Maybe AttributeValue
    , _diarSriovNetSupport     :: Maybe AttributeValue
    } deriving (Eq, Read, Show)

-- | 'DescribeImageAttributeResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'diarBlockDeviceMappings' @::@ ['BlockDeviceMapping']
--
-- * 'diarDescription' @::@ 'Maybe' 'AttributeValue'
--
-- * 'diarImageId' @::@ 'Maybe' 'Text'
--
-- * 'diarKernelId' @::@ 'Maybe' 'AttributeValue'
--
-- * 'diarLaunchPermissions' @::@ ['LaunchPermission']
--
-- * 'diarProductCodes' @::@ ['ProductCode']
--
-- * 'diarRamdiskId' @::@ 'Maybe' 'AttributeValue'
--
-- * 'diarSriovNetSupport' @::@ 'Maybe' 'AttributeValue'
--
describeImageAttributeResponse :: DescribeImageAttributeResponse
describeImageAttributeResponse = DescribeImageAttributeResponse
    { _diarImageId             = Nothing
    , _diarLaunchPermissions   = mempty
    , _diarProductCodes        = mempty
    , _diarKernelId            = Nothing
    , _diarRamdiskId           = Nothing
    , _diarDescription         = Nothing
    , _diarSriovNetSupport     = Nothing
    , _diarBlockDeviceMappings = mempty
    }

-- | One or more block device mapping entries.
diarBlockDeviceMappings :: Lens' DescribeImageAttributeResponse [BlockDeviceMapping]
diarBlockDeviceMappings =
    lens _diarBlockDeviceMappings (\s a -> s { _diarBlockDeviceMappings = a })
        . _List

-- | A description for the AMI.
diarDescription :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarDescription = lens _diarDescription (\s a -> s { _diarDescription = a })

-- | The ID of the AMI.
diarImageId :: Lens' DescribeImageAttributeResponse (Maybe Text)
diarImageId = lens _diarImageId (\s a -> s { _diarImageId = a })

-- | The kernel ID.
diarKernelId :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarKernelId = lens _diarKernelId (\s a -> s { _diarKernelId = a })

-- | One or more launch permissions.
diarLaunchPermissions :: Lens' DescribeImageAttributeResponse [LaunchPermission]
diarLaunchPermissions =
    lens _diarLaunchPermissions (\s a -> s { _diarLaunchPermissions = a })
        . _List

-- | One or more product codes.
diarProductCodes :: Lens' DescribeImageAttributeResponse [ProductCode]
diarProductCodes = lens _diarProductCodes (\s a -> s { _diarProductCodes = a }) . _List

-- | The RAM disk ID.
diarRamdiskId :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarRamdiskId = lens _diarRamdiskId (\s a -> s { _diarRamdiskId = a })

diarSriovNetSupport :: Lens' DescribeImageAttributeResponse (Maybe AttributeValue)
diarSriovNetSupport =
    lens _diarSriovNetSupport (\s a -> s { _diarSriovNetSupport = a })

instance ToPath DescribeImageAttribute where
    toPath = const "/"

instance ToQuery DescribeImageAttribute where
    toQuery DescribeImageAttribute{..} = mconcat
        [ "Attribute" =? _dia1Attribute
        , "DryRun"    =? _dia1DryRun
        , "ImageId"   =? _dia1ImageId
        ]

instance ToHeaders DescribeImageAttribute

instance AWSRequest DescribeImageAttribute where
    type Sv DescribeImageAttribute = EC2
    type Rs DescribeImageAttribute = DescribeImageAttributeResponse

    request  = post "DescribeImageAttribute"
    response = xmlResponse

instance FromXML DescribeImageAttributeResponse where
    parseXML x = DescribeImageAttributeResponse
        <$> x .@? "blockDeviceMapping" .!@ mempty
        <*> x .@? "description"
        <*> x .@? "imageId"
        <*> x .@? "kernel"
        <*> x .@? "launchPermission" .!@ mempty
        <*> x .@? "productCodes" .!@ mempty
        <*> x .@? "ramdisk"
        <*> x .@? "sriovNetSupport"
