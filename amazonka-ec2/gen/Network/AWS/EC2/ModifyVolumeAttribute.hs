{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies a volume attribute. By default, all I/O operations for the volume
-- are suspended when the data on the volume is determined to be potentially
-- inconsistent, to prevent undetectable, latent data corruption. The I/O
-- access to the volume can be resumed by first enabling I/O access and then
-- checking the data consistency on your volume. You can change the default
-- behavior to resume I/O operations. We recommend that you change this only
-- for boot volumes or for volumes that are stateless or disposable. Example
-- This example modifies the attribute of the volume vol-12345678.
-- https://ec2.amazonaws.com/?Action=ModifyVolumeAttribute
-- &amp;VolumeId=vol-12345678 &amp;AutoEnableIO.Value=true &amp;AUTHPARAMS
-- &lt;ModifyVolumeAttributeResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;5jkdf074-37ed-4004-8671-a78ee82bf1cbEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/ModifyVolumeAttributeResponse&gt;.
module Network.AWS.EC2
    (
    -- * Request
      ModifyVolumeAttribute
    -- ** Request constructor
    , mkModifyVolumeAttribute
    -- ** Request lenses
    , mvaVolumeId
    , mvaAutoEnableIO

    -- * Response
    , ModifyVolumeAttributeResponse
    -- ** Response constructor
    , mkModifyVolumeAttributeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

data ModifyVolumeAttribute = ModifyVolumeAttribute
    { _mvaVolumeId :: Text
    , _mvaAutoEnableIO :: Maybe AttributeBooleanValue
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyVolumeAttribute' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VolumeId ::@ @Text@
--
-- * @AutoEnableIO ::@ @Maybe AttributeBooleanValue@
--
mkModifyVolumeAttribute :: Text -- ^ 'mvaVolumeId'
                        -> ModifyVolumeAttribute
mkModifyVolumeAttribute p1 = ModifyVolumeAttribute
    { _mvaVolumeId = p1
    , _mvaAutoEnableIO = Nothing
    }

-- | The ID of the volume.
mvaVolumeId :: Lens' ModifyVolumeAttribute Text
mvaVolumeId = lens _mvaVolumeId (\s a -> s { _mvaVolumeId = a })

-- | Indicates whether the volume should be auto-enabled for I/O operations.
mvaAutoEnableIO :: Lens' ModifyVolumeAttribute (Maybe AttributeBooleanValue)
mvaAutoEnableIO = lens _mvaAutoEnableIO (\s a -> s { _mvaAutoEnableIO = a })

instance ToQuery ModifyVolumeAttribute where
    toQuery = genericQuery def

data ModifyVolumeAttributeResponse = ModifyVolumeAttributeResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyVolumeAttributeResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkModifyVolumeAttributeResponse :: ModifyVolumeAttributeResponse
mkModifyVolumeAttributeResponse = ModifyVolumeAttributeResponse

instance AWSRequest ModifyVolumeAttribute where
    type Sv ModifyVolumeAttribute = EC2
    type Rs ModifyVolumeAttribute = ModifyVolumeAttributeResponse

    request = post "ModifyVolumeAttribute"
    response _ = nullaryResponse ModifyVolumeAttributeResponse
