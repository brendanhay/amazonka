{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoImplicitPrelude         #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TypeFamilies              #-}

{-# OPTIONS_GHC -fno-warn-unused-binds #-}

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
module Network.AWS.CloudTrail.V2013_11_01.StopLogging where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.JSON
import Network.AWS.CloudTrail.V2013_11_01.Types
import Network.AWS.Prelude
import qualified Network.AWS.Types.Map as Map

data StopLogging = StopLogging
    { _slrName :: Text
      -- ^ Communicates to CloudTrail the name of the trail for which to
      -- stop logging AWS API calls.
    } deriving (Show, Generic)

makeLenses ''StopLogging

instance ToPath StopLogging

instance ToQuery StopLogging

instance ToHeaders StopLogging

instance ToJSON StopLogging

data StopLoggingResponse = StopLoggingResponse
    deriving (Eq, Show, Generic)

makeLenses ''StopLoggingResponse

instance AWSRequest StopLogging where
    type Sv StopLogging = CloudTrail
    type Rs StopLogging = StopLoggingResponse

    request = get
    response _ = nullaryResponse StopLoggingResponse
