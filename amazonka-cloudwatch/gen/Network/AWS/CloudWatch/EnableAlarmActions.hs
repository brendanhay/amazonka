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
      EnableAlarmActionsInput
    -- ** Request constructor
    , enableAlarmActionsInput
    -- ** Request lenses
    , eaaiAlarmNames

    -- * Response
    , EnableAlarmActionsResponse
    -- ** Response constructor
    , enableAlarmActionsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types

newtype EnableAlarmActionsInput = EnableAlarmActionsInput
    { _eaaiAlarmNames :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid, Semigroup, IsString)

instance IsList EnableAlarmActionsInput where
    type Item EnableAlarmActionsInput = Text

    fromList = EnableAlarmActionsInput . fromList
    toList   = toList . _eaaiAlarmNames

-- | 'EnableAlarmActionsInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'eaaiAlarmNames' @::@ ['Text']
--
enableAlarmActionsInput :: EnableAlarmActionsInput
enableAlarmActionsInput = EnableAlarmActionsInput
    { _eaaiAlarmNames = mempty
    }

-- | The names of the alarms to enable actions for.
eaaiAlarmNames :: Lens' EnableAlarmActionsInput [Text]
eaaiAlarmNames = lens _eaaiAlarmNames (\s a -> s { _eaaiAlarmNames = a })

instance ToQuery EnableAlarmActionsInput

instance ToPath EnableAlarmActionsInput where
    toPath = const "/"

data EnableAlarmActionsResponse = EnableAlarmActionsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'EnableAlarmActionsResponse' constructor.
enableAlarmActionsResponse :: EnableAlarmActionsResponse
enableAlarmActionsResponse = EnableAlarmActionsResponse

instance FromXML EnableAlarmActionsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnableAlarmActionsResponse"

instance AWSRequest EnableAlarmActionsInput where
    type Sv EnableAlarmActionsInput = CloudWatch
    type Rs EnableAlarmActionsInput = EnableAlarmActionsResponse

    request  = post "EnableAlarmActions"
    response = nullaryResponse EnableAlarmActionsResponse
