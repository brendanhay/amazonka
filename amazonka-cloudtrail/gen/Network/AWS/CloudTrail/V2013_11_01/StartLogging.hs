{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudTrail.V2013_11_01.StartLogging
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Starts the recording of AWS API calls and log file delivery for a trail.
module Network.AWS.CloudTrail.V2013_11_01.StartLogging
    (
    -- * Request
      StartLogging
    -- ** Request constructor
    , mkStartLogging
    -- ** Request lenses
    , slName

    -- * Response
    , StartLoggingResponse
    ) where

import           Network.AWS.CloudTrail.V2013_11_01.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | The request to CloudTrail to start logging AWS API calls for an account.
newtype StartLogging = StartLogging
    { _slName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StartLogging' request.
mkStartLogging :: Text -- ^ 'slName'
               -> StartLogging
mkStartLogging p1 = StartLogging
    { _slName = p1
    }
{-# INLINE mkStartLogging #-}

-- | The name of the trail for which CloudTrail logs AWS API calls.
slName :: Lens' StartLogging Text
slName = lens _slName (\s a -> s { _slName = a })
{-# INLINE slName #-}

instance ToPath StartLogging

instance ToQuery StartLogging

instance ToHeaders StartLogging

instance ToJSON StartLogging

-- | Returns the objects or data listed below if successful. Otherwise, returns
-- an error.
    deriving (Eq, Show, Generic)

instance AWSRequest StartLogging where
    type Sv StartLogging = CloudTrail
    type Rs StartLogging = StartLoggingResponse

    request = get
    response _ = nullaryResponse StartLoggingResponse
