{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DisableAlarmActions.html>
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types
import qualified GHC.Exts

newtype DisableAlarmActions = DisableAlarmActions
    { _daaAlarmNames :: List "AlarmNames" Text
    } deriving (Eq, Ord, Show, Monoid, Semigroup)

instance GHC.Exts.IsList DisableAlarmActions where
    type Item DisableAlarmActions = Text

    fromList = DisableAlarmActions . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _daaAlarmNames

-- | 'DisableAlarmActions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daaAlarmNames' @::@ ['Text']
--
disableAlarmActions :: DisableAlarmActions
disableAlarmActions = DisableAlarmActions
    { _daaAlarmNames = mempty
    }

-- | The names of the alarms to disable actions for.
daaAlarmNames :: Lens' DisableAlarmActions [Text]
daaAlarmNames = lens _daaAlarmNames (\s a -> s { _daaAlarmNames = a }) . _List

data DisableAlarmActionsResponse = DisableAlarmActionsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DisableAlarmActionsResponse' constructor.
disableAlarmActionsResponse :: DisableAlarmActionsResponse
disableAlarmActionsResponse = DisableAlarmActionsResponse

instance ToPath DisableAlarmActions where
    toPath = const "/"

instance ToQuery DisableAlarmActions where
    toQuery DisableAlarmActions{..} = mconcat
        [ "AlarmNames" =? _daaAlarmNames
        ]

instance ToHeaders DisableAlarmActions

query

instance AWSRequest DisableAlarmActions where
    type Sv DisableAlarmActions = CloudWatch
    type Rs DisableAlarmActions = DisableAlarmActionsResponse

    request  = post "DisableAlarmActions"
    response = nullResponse DisableAlarmActionsResponse
