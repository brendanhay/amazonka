{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.CloudTrail.StartLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Starts the recording of AWS API calls and log file delivery for a trail.
module Network.AWS.CloudTrail.StartLogging
    (
    -- * Request
      StartLogging
    -- ** Request constructor
    , startLogging
    -- ** Request lenses
    , sl1Name

    -- * Response
    , StartLoggingResponse
    -- ** Response constructor
    , startLoggingResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudTrail.Types

newtype StartLogging = StartLogging
    { _sl1Name :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'StartLogging' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'sl1Name' @::@ 'Text'
--
startLogging :: Text -- ^ 'sl1Name'
             -> StartLogging
startLogging p1 = StartLogging
    { _sl1Name = p1
    }

-- | The name of the trail for which CloudTrail logs AWS API calls.
sl1Name :: Lens' StartLogging Text
sl1Name = lens _sl1Name (\s a -> s { _sl1Name = a })

instance ToPath StartLogging where
    toPath = const "/"

instance ToQuery StartLogging where
    toQuery = const mempty

instance ToHeaders StartLogging

instance ToBody StartLogging where
    toBody = toBody . encode . _sl1Name

data StartLoggingResponse = StartLoggingResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'StartLoggingResponse' constructor.
startLoggingResponse :: StartLoggingResponse
startLoggingResponse = StartLoggingResponse

instance AWSRequest StartLogging where
    type Sv StartLogging = CloudTrail
    type Rs StartLogging = StartLoggingResponse

    request  = post
    response = nullaryResponse StartLoggingResponse
