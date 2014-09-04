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
    , mkDisableAlarmActionsInput
    -- ** Request lenses
    , daaiAlarmNames

    -- * Response
    , DisableAlarmActionsResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisableAlarmActions' request.
mkDisableAlarmActionsInput :: [Text] -- ^ 'daaiAlarmNames'
                           -> DisableAlarmActions
mkDisableAlarmActionsInput p1 = DisableAlarmActions
    { _daaiAlarmNames = p1
    }
{-# INLINE mkDisableAlarmActionsInput #-}

newtype DisableAlarmActions = DisableAlarmActions
    { _daaiAlarmNames :: [Text]
      -- ^ The names of the alarms to disable actions for.
    } deriving (Show, Generic)

-- | The names of the alarms to disable actions for.
daaiAlarmNames :: Lens' DisableAlarmActions ([Text])
daaiAlarmNames = lens _daaiAlarmNames (\s a -> s { _daaiAlarmNames = a })
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
