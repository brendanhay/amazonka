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
    , mkStartLoggingRequest
    -- ** Request lenses
    , slrName

    -- * Response
    , StartLoggingResponse
    ) where

import           Network.AWS.CloudTrail.V2013_11_01.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'StartLogging' request.
mkStartLoggingRequest :: Text -- ^ 'slrName'
                      -> StartLogging
mkStartLoggingRequest p1 = StartLogging
    { _slrName = p1
    }
{-# INLINE mkStartLoggingRequest #-}

newtype StartLogging = StartLogging
    { _slrName :: Text
      -- ^ The name of the trail for which CloudTrail logs AWS API calls.
    } deriving (Show, Generic)

-- | The name of the trail for which CloudTrail logs AWS API calls.
slrName :: Lens' StartLogging (Text)
slrName = lens _slrName (\s a -> s { _slrName = a })
{-# INLINE slrName #-}

instance ToPath StartLogging

instance ToQuery StartLogging

instance ToHeaders StartLogging

instance ToJSON StartLogging

    deriving (Eq, Show, Generic)

instance AWSRequest StartLogging where
    type Sv StartLogging = CloudTrail
    type Rs StartLogging = StartLoggingResponse

    request = get
    response _ = nullaryResponse StartLoggingResponse
