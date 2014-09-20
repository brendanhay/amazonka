{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
      DisableAlarmActions
    -- ** Request constructor
    , disableAlarmActions
    -- ** Request lenses
    , daaAlarmNames

    -- * Response
    , DisableAlarmActionsResponse
    -- ** Response constructor
    , disableAlarmActionsResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types
import Network.AWS.Prelude

-- | 
newtype DisableAlarmActions = DisableAlarmActions
    { _daaAlarmNames :: [Text]
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisableAlarmActions' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AlarmNames ::@ @[Text]@
--
disableAlarmActions :: [Text] -- ^ 'daaAlarmNames'
                    -> DisableAlarmActions
disableAlarmActions p1 = DisableAlarmActions
    { _daaAlarmNames = p1
    }

-- | The names of the alarms to disable actions for.
daaAlarmNames :: Lens' DisableAlarmActions [Text]
daaAlarmNames = lens _daaAlarmNames (\s a -> s { _daaAlarmNames = a })

instance ToQuery DisableAlarmActions where
    toQuery = genericQuery def

data DisableAlarmActionsResponse = DisableAlarmActionsResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisableAlarmActionsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
disableAlarmActionsResponse :: DisableAlarmActionsResponse
disableAlarmActionsResponse = DisableAlarmActionsResponse

instance AWSRequest DisableAlarmActions where
    type Sv DisableAlarmActions = CloudWatch
    type Rs DisableAlarmActions = DisableAlarmActionsResponse

    request = post "DisableAlarmActions"
    response _ = nullaryResponse DisableAlarmActionsResponse
