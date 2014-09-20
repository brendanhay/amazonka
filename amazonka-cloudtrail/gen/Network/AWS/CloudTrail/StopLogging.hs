{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
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
module Network.AWS.CloudTrail.StopLogging
    (
    -- * Request
      StopLogging
    -- ** Request constructor
    , stopLogging
    -- ** Request lenses
    , sl1Name

    -- * Response
    , StopLoggingResponse
    -- ** Response constructor
    , stopLoggingResponse
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | Passes the request to CloudTrail to stop logging AWS API calls for the
-- specified account.
newtype StopLogging = StopLogging
    { _sl1Name :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StopLogging' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
stopLogging :: Text -- ^ 'sl1Name'
            -> StopLogging
stopLogging p1 = StopLogging
    { _sl1Name = p1
    }

-- | Communicates to CloudTrail the name of the trail for which to stop logging
-- AWS API calls.
sl1Name :: Lens' StopLogging Text
sl1Name = lens _sl1Name (\s a -> s { _sl1Name = a })

instance ToPath StopLogging

instance ToQuery StopLogging

instance ToHeaders StopLogging

instance ToJSON StopLogging

-- | Returns the objects or data listed below if successful. Otherwise, returns
-- an error.
data StopLoggingResponse = StopLoggingResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StopLoggingResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
stopLoggingResponse :: StopLoggingResponse
stopLoggingResponse = StopLoggingResponse

instance AWSRequest StopLogging where
    type Sv StopLogging = CloudTrail
    type Rs StopLogging = StopLoggingResponse

    request = get
    response _ = nullaryResponse StopLoggingResponse
