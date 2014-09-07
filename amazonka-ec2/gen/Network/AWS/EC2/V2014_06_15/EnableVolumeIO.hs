{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.EnableVolumeIO
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables I/O operations for a volume that had I/O operations disabled
-- because the data on the volume was potentially inconsistent. Example This
-- example enables the I/O operations of the volume vol-8888888.
-- https://ec2.amazonaws.com/?Action=EnableVolumeIO &amp;VolumeId= vol-8888888
-- &amp;AUTHPARAMS &lt;EnableVolumeIOResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-05-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/EnableVolumeIOResponse&gt;.
module Network.AWS.EC2.V2014_06_15.EnableVolumeIO
    (
    -- * Request
      EnableVolumeIO
    -- ** Request constructor
    , mkEnableVolumeIO
    -- ** Request lenses
    , evioVolumeId

    -- * Response
    , EnableVolumeIOResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
newtype EnableVolumeIO = EnableVolumeIO
    { _evioVolumeId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'EnableVolumeIO' request.
mkEnableVolumeIO :: Text -- ^ 'evioVolumeId'
                 -> EnableVolumeIO
mkEnableVolumeIO p1 = EnableVolumeIO
    { _evioVolumeId = p1
    }

-- | The ID of the volume.
evioVolumeId :: Lens' EnableVolumeIO Text
evioVolumeId = lens _evioVolumeId (\s a -> s { _evioVolumeId = a })

instance ToQuery EnableVolumeIO where
    toQuery = genericQuery def

data EnableVolumeIOResponse = EnableVolumeIOResponse
    deriving (Eq, Show, Generic)

instance AWSRequest EnableVolumeIO where
    type Sv EnableVolumeIO = EC2
    type Rs EnableVolumeIO = EnableVolumeIOResponse

    request = post "EnableVolumeIO"
    response _ = nullaryResponse EnableVolumeIOResponse
