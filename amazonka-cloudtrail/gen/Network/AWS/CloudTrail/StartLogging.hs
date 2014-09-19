{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , slName

    -- * Response
    , StartLoggingResponse
    -- ** Response constructor
    , startLoggingResponse
    ) where

import Network.AWS.CloudTrail.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | The request to CloudTrail to start logging AWS API calls for an account.
newtype StartLogging = StartLogging
    { _slName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StartLogging' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Name ::@ @Text@
--
startLogging :: Text -- ^ 'slName'
             -> StartLogging
startLogging p1 = StartLogging
    { _slName = p1
    }

-- | The name of the trail for which CloudTrail logs AWS API calls.
slName :: Lens' StartLogging Text
slName = lens _slName (\s a -> s { _slName = a })

instance ToPath StartLogging

instance ToQuery StartLogging

instance ToHeaders StartLogging

instance ToJSON StartLogging

-- | Returns the objects or data listed below if successful. Otherwise, returns
-- an error.
data StartLoggingResponse = StartLoggingResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StartLoggingResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
startLoggingResponse :: StartLoggingResponse
startLoggingResponse = StartLoggingResponse

instance AWSRequest StartLogging where
    type Sv StartLogging = CloudTrail
    type Rs StartLogging = StartLoggingResponse

    request = get
    response _ = nullaryResponse StartLoggingResponse
