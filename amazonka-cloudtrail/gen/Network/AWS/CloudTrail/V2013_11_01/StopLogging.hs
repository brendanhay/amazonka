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
    , stopLogging
    -- ** Request lenses
    , sltName

    -- * Response
    , StopLoggingResponse
    ) where

import           Network.AWS.CloudTrail.V2013_11_01.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'StopLogging' request.
stopLogging :: Text -- ^ 'sltName'
            -> StopLogging
stopLogging p1 = StopLogging
    { _sltName = p1
    }

data StopLogging = StopLogging
    { _sltName :: Text
      -- ^ Communicates to CloudTrail the name of the trail for which to
      -- stop logging AWS API calls.
    } deriving (Show, Generic)

-- | Communicates to CloudTrail the name of the trail for which to stop logging
-- AWS API calls.
sltName
    :: Functor f
    => (Text
    -> f (Text))
    -> StopLogging
    -> f StopLogging
sltName f x =
    (\y -> x { _sltName = y })
       <$> f (_sltName x)
{-# INLINE sltName #-}

instance ToPath StopLogging

instance ToQuery StopLogging

instance ToHeaders StopLogging

instance ToJSON StopLogging

data StopLoggingResponse = StopLoggingResponse
    deriving (Eq, Show, Generic)

instance AWSRequest StopLogging where
    type Sv StopLogging = CloudTrail
    type Rs StopLogging = StopLoggingResponse

    request = get
    response _ = nullaryResponse StopLoggingResponse
