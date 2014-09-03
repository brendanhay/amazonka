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
    , startLogging
    -- ** Request lenses
    , slrName

    -- * Response
    , StartLoggingResponse
    ) where

import           Network.AWS.CloudTrail.V2013_11_01.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'StartLogging' request.
startLogging :: Text -- ^ 'slrName'
             -> StartLogging
startLogging p1 = StartLogging
    { _slrName = p1
    }

data StartLogging = StartLogging
    { _slrName :: Text
      -- ^ The name of the trail for which CloudTrail logs AWS API calls.
    } deriving (Show, Generic)

-- | The name of the trail for which CloudTrail logs AWS API calls.
slrName
    :: Functor f
    => (Text
    -> f (Text))
    -> StartLogging
    -> f StartLogging
slrName f x =
    (\y -> x { _slrName = y })
       <$> f (_slrName x)
{-# INLINE slrName #-}

instance ToPath StartLogging

instance ToQuery StartLogging

instance ToHeaders StartLogging

instance ToJSON StartLogging

data StartLoggingResponse = StartLoggingResponse
    deriving (Eq, Show, Generic)

instance AWSRequest StartLogging where
    type Sv StartLogging = CloudTrail
    type Rs StartLogging = StartLoggingResponse

    request = get
    response _ = nullaryResponse StartLoggingResponse
