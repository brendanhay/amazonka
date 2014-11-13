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

-- Module      : Network.AWS.CloudTrail.StopLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Suspends the recording of AWS API calls and log file delivery for the
-- specified trail. Under most circumstances, there is no need to use this
-- action. You can update a trail without stopping it first. This action is
-- the only way to stop recording.
module Network.AWS.CloudTrail.StopLogging
    (
    -- * Request
      StopLogging
    -- ** Request constructor
    , stopLogging
    -- ** Request lenses
    , slName

    -- * Response
    , StopLoggingResponse
    -- ** Response constructor
    , stopLoggingResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudTrail.Types

newtype StopLogging = StopLogging
    { _slName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'StopLogging' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'slName' @::@ 'Text'
--
stopLogging :: Text -- ^ 'slName'
            -> StopLogging
stopLogging p1 = StopLogging
    { _slName = p1
    }

-- | Communicates to CloudTrail the name of the trail for which to stop
-- logging AWS API calls.
slName :: Lens' StopLogging Text
slName = lens _slName (\s a -> s { _slName = a })

instance ToPath StopLogging where
    toPath = const "/"

instance ToQuery StopLogging where
    toQuery = const mempty

instance ToHeaders StopLogging

instance ToBody StopLogging where
    toBody = toBody . encode . _slName

data StopLoggingResponse = StopLoggingResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'StopLoggingResponse' constructor.
stopLoggingResponse :: StopLoggingResponse
stopLoggingResponse = StopLoggingResponse

-- FromJSON

instance AWSRequest StopLogging where
    type Sv StopLogging = CloudTrail
    type Rs StopLogging = StopLoggingResponse

    request  = post'
    response = nullaryResponse StopLoggingResponse
