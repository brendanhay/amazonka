{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.CloudWatch.DisableAlarmActions
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
module Network.AWS.CloudWatch.DisableAlarmActions
    (
    -- * Request
      DisableAlarmActionsInput
    -- ** Request constructor
    , disableAlarmActionsInput
    -- ** Request lenses
    , daaiAlarmNames

    -- * Response
    , DisableAlarmActionsResponse
    -- ** Response constructor
    , disableAlarmActionsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types

newtype DisableAlarmActionsInput = DisableAlarmActionsInput
    { _daaiAlarmNames :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DisableAlarmActionsInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daaiAlarmNames' @::@ ['Text']
--
disableAlarmActionsInput :: DisableAlarmActionsInput
disableAlarmActionsInput = DisableAlarmActionsInput
    { _daaiAlarmNames = mempty
    }

-- | The names of the alarms to disable actions for.
daaiAlarmNames :: Lens' DisableAlarmActionsInput [Text]
daaiAlarmNames = lens _daaiAlarmNames (\s a -> s { _daaiAlarmNames = a })
instance ToQuery DisableAlarmActionsInput

instance ToPath DisableAlarmActionsInput where
    toPath = const "/"

data DisableAlarmActionsResponse = DisableAlarmActionsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DisableAlarmActionsResponse' constructor.
disableAlarmActionsResponse :: DisableAlarmActionsResponse
disableAlarmActionsResponse = DisableAlarmActionsResponse
instance FromXML DisableAlarmActionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DisableAlarmActionsResponse"

instance AWSRequest DisableAlarmActionsInput where
    type Sv DisableAlarmActionsInput = CloudWatch
    type Rs DisableAlarmActionsInput = DisableAlarmActionsResponse

    request  = post "DisableAlarmActions"
    response = nullaryResponse DisableAlarmActionsResponse
