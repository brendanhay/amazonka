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

-- Module      : Network.AWS.CloudWatch.EnableAlarmActions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Enables actions for the specified alarms.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_EnableAlarmActions.html>
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
import qualified GHC.Exts

newtype EnableAlarmActions = EnableAlarmActions
    { _eaaAlarmNames :: List "member" Text
    } deriving (Eq, Ord, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList EnableAlarmActions where
    type Item EnableAlarmActions = Text

    fromList = EnableAlarmActions . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _eaaAlarmNames

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
eaaAlarmNames = lens _eaaAlarmNames (\s a -> s { _eaaAlarmNames = a }) . _List

data EnableAlarmActionsResponse = EnableAlarmActionsResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'EnableAlarmActionsResponse' constructor.
enableAlarmActionsResponse :: EnableAlarmActionsResponse
enableAlarmActionsResponse = EnableAlarmActionsResponse

instance ToPath EnableAlarmActions where
    toPath = const "/"

instance ToQuery EnableAlarmActions where
    toQuery EnableAlarmActions{..} = mconcat
        [ "AlarmNames" =? _eaaAlarmNames
        ]

instance ToHeaders EnableAlarmActions

instance AWSRequest EnableAlarmActions where
    type Sv EnableAlarmActions = CloudWatch
    type Rs EnableAlarmActions = EnableAlarmActionsResponse

    request  = post "EnableAlarmActions"
    response = nullResponse EnableAlarmActionsResponse
