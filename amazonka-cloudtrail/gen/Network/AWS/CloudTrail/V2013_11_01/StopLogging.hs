{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.V2013_11_01.StopLogging
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
module Network.AWS.CloudTrail.V2013_11_01.StopLogging
    (
    -- * Request
      StopLogging
    -- ** Request constructor
    , mkStopLoggingRequest
    -- ** Request lenses
    , sltName

    -- * Response
    , StopLoggingResponse
    ) where

import           Network.AWS.CloudTrail.V2013_11_01.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StopLogging' request.
mkStopLoggingRequest :: Text -- ^ 'sltName'
                     -> StopLogging
mkStopLoggingRequest p1 = StopLogging
    { _sltName = p1
    }
{-# INLINE mkStopLoggingRequest #-}

newtype StopLogging = StopLogging
    { _sltName :: Text
      -- ^ Communicates to CloudTrail the name of the trail for which to
      -- stop logging AWS API calls.
    } deriving (Show, Generic)

-- | Communicates to CloudTrail the name of the trail for which to stop logging
-- AWS API calls.
sltName :: Lens' StopLogging (Text)
sltName = lens _sltName (\s a -> s { _sltName = a })
{-# INLINE sltName #-}

instance ToPath StopLogging

instance ToQuery StopLogging

instance ToHeaders StopLogging

instance ToJSON StopLogging

    deriving (Eq, Show, Generic)

instance AWSRequest StopLogging where
    type Sv StopLogging = CloudTrail
    type Rs StopLogging = StopLoggingResponse

    request = get
    response _ = nullaryResponse StopLoggingResponse
