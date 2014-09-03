{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeImageAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the specified attribute of the specified AMI. You can specify
-- only one attribute at a time. Example 1 This example lists the launch
-- permissions for the specified AMI.
-- https://ec2.amazonaws.com/?Action=DescribeImageAttribute
-- &amp;ImageId=ami-61a54008 &amp;Attribute=launchPermission &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE ami-61a54008 all 495219933132 Example
-- 2 This example lists the product codes for the specified AMI.
-- https://ec2.amazonaws.com/?Action=DescribeImageAttribute
-- &amp;ImageId=ami-2bb65342 &amp;Attribute=productCodes &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE ami-2bb65342 a1b2c3d4e5f6g7h8i9j10k11
-- marketplace.
module Network.AWS.EC2.V2014_06_15.DescribeImageAttribute
    (
    -- * Request
      DescribeImageAttribute
    -- ** Request constructor
    , describeImageAttribute
    -- ** Request lenses
    , diarAttribute
    , diarImageId

    -- * Response
    , DescribeImageAttributeResponse
    -- ** Response lenses
    , iaKernelId
    , iaRamdiskId
    , iaDescription
    , iaSriovNetSupport
    , iaBlockDeviceMappings
    , iaLaunchPermissions
    , iaProductCodes
    , iaImageId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DescribeImageAttribute' request.
describeImageAttribute :: ImageAttributeName -- ^ 'diarAttribute'
                       -> Text -- ^ 'diarImageId'
                       -> DescribeImageAttribute
describeImageAttribute p1 p2 = DescribeImageAttribute
    { _diarAttribute = p1
    , _diarImageId = p2
    }

data DescribeImageAttribute = DescribeImageAttribute
    { _diarAttribute :: ImageAttributeName
      -- ^ The AMI attribute.
    , _diarImageId :: Text
      -- ^ The ID of the AMI.
    } deriving (Show, Generic)

-- | The AMI attribute.
diarAttribute
    :: Functor f
    => (ImageAttributeName
    -> f (ImageAttributeName))
    -> DescribeImageAttribute
    -> f DescribeImageAttribute
diarAttribute f x =
    (\y -> x { _diarAttribute = y })
       <$> f (_diarAttribute x)
{-# INLINE diarAttribute #-}

-- | The ID of the AMI.
diarImageId
    :: Functor f
    => (Text
    -> f (Text))
    -> DescribeImageAttribute
    -> f DescribeImageAttribute
diarImageId f x =
    (\y -> x { _diarImageId = y })
       <$> f (_diarImageId x)
{-# INLINE diarImageId #-}

instance ToQuery DescribeImageAttribute where
    toQuery = genericQuery def

data DescribeImageAttributeResponse = DescribeImageAttributeResponse
    { _iaKernelId :: Maybe AttributeValue
      -- ^ The kernel ID.
    , _iaRamdiskId :: Maybe AttributeValue
      -- ^ The RAM disk ID.
    , _iaDescription :: Maybe AttributeValue
      -- ^ A description for the AMI.
    , _iaSriovNetSupport :: Maybe AttributeValue
      -- ^ 
    , _iaBlockDeviceMappings :: [BlockDeviceMapping]
      -- ^ One or more block device mapping entries.
    , _iaLaunchPermissions :: [LaunchPermission]
      -- ^ One or more launch permissions.
    , _iaProductCodes :: [ProductCode]
      -- ^ One or more product codes.
    , _iaImageId :: Maybe Text
      -- ^ The ID of the AMI.
    } deriving (Show, Generic)

-- | The kernel ID.
iaKernelId
    :: Functor f
    => (Maybe AttributeValue
    -> f (Maybe AttributeValue))
    -> DescribeImageAttributeResponse
    -> f DescribeImageAttributeResponse
iaKernelId f x =
    (\y -> x { _iaKernelId = y })
       <$> f (_iaKernelId x)
{-# INLINE iaKernelId #-}

-- | The RAM disk ID.
iaRamdiskId
    :: Functor f
    => (Maybe AttributeValue
    -> f (Maybe AttributeValue))
    -> DescribeImageAttributeResponse
    -> f DescribeImageAttributeResponse
iaRamdiskId f x =
    (\y -> x { _iaRamdiskId = y })
       <$> f (_iaRamdiskId x)
{-# INLINE iaRamdiskId #-}

-- | A description for the AMI.
iaDescription
    :: Functor f
    => (Maybe AttributeValue
    -> f (Maybe AttributeValue))
    -> DescribeImageAttributeResponse
    -> f DescribeImageAttributeResponse
iaDescription f x =
    (\y -> x { _iaDescription = y })
       <$> f (_iaDescription x)
{-# INLINE iaDescription #-}

-- | 
iaSriovNetSupport
    :: Functor f
    => (Maybe AttributeValue
    -> f (Maybe AttributeValue))
    -> DescribeImageAttributeResponse
    -> f DescribeImageAttributeResponse
iaSriovNetSupport f x =
    (\y -> x { _iaSriovNetSupport = y })
       <$> f (_iaSriovNetSupport x)
{-# INLINE iaSriovNetSupport #-}

-- | One or more block device mapping entries.
iaBlockDeviceMappings
    :: Functor f
    => ([BlockDeviceMapping]
    -> f ([BlockDeviceMapping]))
    -> DescribeImageAttributeResponse
    -> f DescribeImageAttributeResponse
iaBlockDeviceMappings f x =
    (\y -> x { _iaBlockDeviceMappings = y })
       <$> f (_iaBlockDeviceMappings x)
{-# INLINE iaBlockDeviceMappings #-}

-- | One or more launch permissions.
iaLaunchPermissions
    :: Functor f
    => ([LaunchPermission]
    -> f ([LaunchPermission]))
    -> DescribeImageAttributeResponse
    -> f DescribeImageAttributeResponse
iaLaunchPermissions f x =
    (\y -> x { _iaLaunchPermissions = y })
       <$> f (_iaLaunchPermissions x)
{-# INLINE iaLaunchPermissions #-}

-- | One or more product codes.
iaProductCodes
    :: Functor f
    => ([ProductCode]
    -> f ([ProductCode]))
    -> DescribeImageAttributeResponse
    -> f DescribeImageAttributeResponse
iaProductCodes f x =
    (\y -> x { _iaProductCodes = y })
       <$> f (_iaProductCodes x)
{-# INLINE iaProductCodes #-}

-- | The ID of the AMI.
iaImageId
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DescribeImageAttributeResponse
    -> f DescribeImageAttributeResponse
iaImageId f x =
    (\y -> x { _iaImageId = y })
       <$> f (_iaImageId x)
{-# INLINE iaImageId #-}

instance FromXML DescribeImageAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeImageAttribute where
    type Sv DescribeImageAttribute = EC2
    type Rs DescribeImageAttribute = DescribeImageAttributeResponse

    request = post "DescribeImageAttribute"
    response _ = xmlResponse
