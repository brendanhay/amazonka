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

-- Module      : Network.AWS.CloudWatch.EnableAlarmActions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables actions for the specified alarms.
module Network.AWS.CloudWatch.EnableAlarmActions
    (
    -- * Request
      EnableAlarmActions
    -- ** Request constructor
    , enableAlarmActions
    -- ** Request lenses
    , eaaAlarmNames

    -- * Response
    , EnableAlarmActionsResponse
    -- ** Response constructor
    , enableAlarmActionsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types

newtype EnableAlarmActions = EnableAlarmActions
    { _eaaAlarmNames :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup)

instance IsList EnableAlarmActions where
    type Item EnableAlarmActions = Text

    fromList = EnableAlarmActions . fromList
    toList   = toList . _eaaAlarmNames

-- | 'EnableAlarmActions' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eaaAlarmNames' @::@ ['Text']
--
enableAlarmActions :: EnableAlarmActions
enableAlarmActions = EnableAlarmActions
    { _eaaAlarmNames = mempty
    }

-- | The names of the alarms to enable actions for.
eaaAlarmNames :: Lens' EnableAlarmActions [Text]
eaaAlarmNames = lens _eaaAlarmNames (\s a -> s { _eaaAlarmNames = a })

instance ToQuery EnableAlarmActions

instance ToPath EnableAlarmActions where
    toPath = const "/"

data EnableAlarmActionsResponse = EnableAlarmActionsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'EnableAlarmActionsResponse' constructor.
enableAlarmActionsResponse :: EnableAlarmActionsResponse
enableAlarmActionsResponse = EnableAlarmActionsResponse

instance FromXML EnableAlarmActionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnableAlarmActionsResponse"

instance AWSRequest EnableAlarmActions where
    type Sv EnableAlarmActions = CloudWatch
    type Rs EnableAlarmActions = EnableAlarmActionsResponse

    request  = post "EnableAlarmActions"
    response = nullaryResponse EnableAlarmActionsResponse
