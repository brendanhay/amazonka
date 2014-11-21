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
--
-- <http://docs.aws.amazon.com/awscloudtrail/latest/APIReference/API_StopLogging.html>
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
import Network.AWS.Request.JSON
import Network.AWS.CloudTrail.Types
import qualified GHC.Exts

newtype StopLogging = StopLogging
    { _slName :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

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

data StopLoggingResponse = StopLoggingResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'StopLoggingResponse' constructor.
stopLoggingResponse :: StopLoggingResponse
stopLoggingResponse = StopLoggingResponse

instance ToPath StopLogging where
    toPath = const "/"

instance ToQuery StopLogging where
    toQuery = const mempty

instance ToHeaders StopLogging

instance ToJSON StopLogging where
    toJSON StopLogging{..} = object
        [ "Name" .= _slName
        ]

instance AWSRequest StopLogging where
    type Sv StopLogging = CloudTrail
    type Rs StopLogging = StopLoggingResponse

    request  = post "StopLogging"
    response = nullResponse StopLoggingResponse
