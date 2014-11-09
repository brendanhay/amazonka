{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.EnableVolumeIO
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables I/O operations for a volume that had I/O operations disabled
-- because the data on the volume was potentially inconsistent.
module Network.AWS.EC2.EnableVolumeIO
    (
    -- * Request
      EnableVolumeIO
    -- ** Request constructor
    , enableVolumeIO
    -- ** Request lenses
    , evioDryRun
    , evioVolumeId

    -- * Response
    , EnableVolumeIOResponse
    -- ** Response constructor
    , enableVolumeIOResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data EnableVolumeIO = EnableVolumeIO
    { _evioDryRun   :: Maybe Bool
    , _evioVolumeId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'EnableVolumeIO' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'evioDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'evioVolumeId' @::@ 'Text'
--
enableVolumeIO :: Text -- ^ 'evioVolumeId'
               -> EnableVolumeIO
enableVolumeIO p1 = EnableVolumeIO
    { _evioVolumeId = p1
    , _evioDryRun   = Nothing
    }

evioDryRun :: Lens' EnableVolumeIO (Maybe Bool)
evioDryRun = lens _evioDryRun (\s a -> s { _evioDryRun = a })

-- | The ID of the volume.
evioVolumeId :: Lens' EnableVolumeIO Text
evioVolumeId = lens _evioVolumeId (\s a -> s { _evioVolumeId = a })

instance ToPath EnableVolumeIO where
    toPath = const "/"

instance ToQuery EnableVolumeIO

data EnableVolumeIOResponse = EnableVolumeIOResponse

-- | 'EnableVolumeIOResponse' constructor.
enableVolumeIOResponse :: EnableVolumeIOResponse
enableVolumeIOResponse = EnableVolumeIOResponse

instance AWSRequest EnableVolumeIO where
    type Sv EnableVolumeIO = EC2
    type Rs EnableVolumeIO = EnableVolumeIOResponse

    request  = post "EnableVolumeIO"
    response = const (nullaryResponse EnableVolumeIOResponse)
