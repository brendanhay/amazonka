{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.CloudTrail.V2013_11_01.StartLogging where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CloudTrail.V2013_11_01.Types
import Network.AWS.Prelude

data StartLogging = StartLogging
    { _sltName :: Text
      -- ^ The name of the trail for which CloudTrail logs AWS API calls.
    } deriving (Show, Generic)

makeLenses ''StartLogging

instance ToPath StartLogging

instance ToQuery StartLogging

instance ToHeaders StartLogging

instance ToJSON StartLogging

data StartLoggingResponse = StartLoggingResponse
    deriving (Eq, Show, Generic)

makeLenses ''StartLoggingResponse

instance AWSRequest StartLogging where
    type Sv StartLogging = CloudTrail
    type Rs StartLogging = StartLoggingResponse

    request = get
    response _ _ = return (Right StartLoggingResponse)
