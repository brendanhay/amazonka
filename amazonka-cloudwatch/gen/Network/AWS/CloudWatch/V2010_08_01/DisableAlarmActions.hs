{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.V2010_08_01.DisableAlarmActions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Disables actions for the specified alarms. When an alarm's actions are
-- disabled the alarm's state may change, but none of the alarm's actions will
-- execute.
module Network.AWS.CloudWatch.V2010_08_01.DisableAlarmActions
    (
    -- * Request
      DisableAlarmActions
    -- ** Request constructor
    , disableAlarmActions
    -- ** Request lenses
    , daaiAlarmNames

    -- * Response
    , DisableAlarmActionsResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DisableAlarmActions' request.
disableAlarmActions :: [Text] -- ^ 'daaiAlarmNames'
                    -> DisableAlarmActions
disableAlarmActions p1 = DisableAlarmActions
    { _daaiAlarmNames = p1
    }
{-# INLINE disableAlarmActions #-}

data DisableAlarmActions = DisableAlarmActions
    { _daaiAlarmNames :: [Text]
      -- ^ The names of the alarms to disable actions for.
    } deriving (Show, Generic)

-- | The names of the alarms to disable actions for.
daaiAlarmNames :: Lens' DisableAlarmActions ([Text])
daaiAlarmNames f x =
    f (_daaiAlarmNames x)
        <&> \y -> x { _daaiAlarmNames = y }
{-# INLINE daaiAlarmNames #-}

instance ToQuery DisableAlarmActions where
    toQuery = genericQuery def

data DisableAlarmActionsResponse = DisableAlarmActionsResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DisableAlarmActions where
    type Sv DisableAlarmActions = CloudWatch
    type Rs DisableAlarmActions = DisableAlarmActionsResponse

    request = post "DisableAlarmActions"
    response _ = nullaryResponse DisableAlarmActionsResponse
