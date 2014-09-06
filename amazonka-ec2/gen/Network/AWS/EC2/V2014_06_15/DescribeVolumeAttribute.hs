{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DescribeVolumeAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Describes the specified attribute of the specified volume. You can specify
-- only one attribute at a time. For more information about Amazon EBS
-- volumes, see Amazon EBS Volumes in the Amazon Elastic Compute Cloud User
-- Guide. Example This example describes the autoEnableIO attribute of the
-- volume vol-12345678.
-- https://ec2.amazonaws.com/?Action=DescribeVolumeAttribute
-- &amp;Attribute=autoEnableIO &amp;VolumeId=vol-12345678 &amp;AUTHPARAMS
-- &lt;DescribeVolumeAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;5jkdf074-37ed-4004-8671-a78ee82bf1cbEXAMPLE&lt;/requestId&gt;
-- &lt;volumeId&gt;vol-12345678&lt;/volumeId&gt; &lt;autoEnableIO&gt;
-- &lt;value&gt;false&lt;/value&gt; &lt;/autoEnableIO&gt;
-- &lt;/DescribeVolumeAttributeResponse&gt; Example This example describes the
-- productCodes attribute of the volume vol-12345678.
-- https://ec2.amazonaws.com/?Action=DescribeVolumeAttribute
-- &amp;Attribute=productCodes &amp;VolumeId=vol-12345678 &amp;AUTHPARAMS
-- &lt;DescribeVolumeAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;5jkdf074-37ed-4004-8671-a78ee82bf1cbEXAMPLE&lt;/requestId&gt;
-- &lt;volumeId&gt;vol-12345678&lt;/volumeId&gt; &lt;productCodes&gt;
-- &lt;item&gt;
-- &lt;productCode&gt;a1b2c3d4e5f6g7h8i9j10k11&lt;/productCode&gt;
-- &lt;type&gt;marketplace&lt;/type&gt; &lt;/item&gt; &lt;/productCodes&gt;
-- &lt;/DescribeVolumeAttributeResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DescribeVolumeAttribute
    (
    -- * Request
      DescribeVolumeAttribute
    -- ** Request constructor
    , mkDescribeVolumeAttribute
    -- ** Request lenses
    , dvaVolumeId
    , dvaAttribute

    -- * Response
    , DescribeVolumeAttributeResponse
    -- ** Response lenses
    , dvarsVolumeId
    , dvarsAutoEnableIO
    , dvarsProductCodes
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data DescribeVolumeAttribute = DescribeVolumeAttribute
    { _dvaVolumeId :: Text
    , _dvaAttribute :: Maybe VolumeAttributeName
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DescribeVolumeAttribute' request.
mkDescribeVolumeAttribute :: Text -- ^ 'dvaVolumeId'
                          -> DescribeVolumeAttribute
mkDescribeVolumeAttribute p1 = DescribeVolumeAttribute
    { _dvaVolumeId = p1
    , _dvaAttribute = Nothing
    }
{-# INLINE mkDescribeVolumeAttribute #-}

-- | The ID of the volume.
dvaVolumeId :: Lens' DescribeVolumeAttribute Text
dvaVolumeId = lens _dvaVolumeId (\s a -> s { _dvaVolumeId = a })
{-# INLINE dvaVolumeId #-}

-- | The instance attribute.
dvaAttribute :: Lens' DescribeVolumeAttribute (Maybe VolumeAttributeName)
dvaAttribute = lens _dvaAttribute (\s a -> s { _dvaAttribute = a })
{-# INLINE dvaAttribute #-}

instance ToQuery DescribeVolumeAttribute where
    toQuery = genericQuery def

-- | 
data DescribeVolumeAttributeResponse = DescribeVolumeAttributeResponse
    { _dvarsVolumeId :: Maybe Text
    , _dvarsAutoEnableIO :: Maybe AttributeBooleanValue
    , _dvarsProductCodes :: [ProductCode]
    } deriving (Show, Generic)

-- | The ID of the volume.
dvarsVolumeId :: Lens' DescribeVolumeAttributeResponse (Maybe Text)
dvarsVolumeId = lens _dvarsVolumeId (\s a -> s { _dvarsVolumeId = a })
{-# INLINE dvarsVolumeId #-}

-- | The state of autoEnableIO attribute.
dvarsAutoEnableIO :: Lens' DescribeVolumeAttributeResponse (Maybe AttributeBooleanValue)
dvarsAutoEnableIO =
    lens _dvarsAutoEnableIO (\s a -> s { _dvarsAutoEnableIO = a })
{-# INLINE dvarsAutoEnableIO #-}

-- | A list of product codes.
dvarsProductCodes :: Lens' DescribeVolumeAttributeResponse [ProductCode]
dvarsProductCodes =
    lens _dvarsProductCodes (\s a -> s { _dvarsProductCodes = a })
{-# INLINE dvarsProductCodes #-}

instance FromXML DescribeVolumeAttributeResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DescribeVolumeAttribute where
    type Sv DescribeVolumeAttribute = EC2
    type Rs DescribeVolumeAttribute = DescribeVolumeAttributeResponse

    request = post "DescribeVolumeAttribute"
    response _ = xmlResponse
