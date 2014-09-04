{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.V2010_08_01.EnableAlarmActions
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables actions for the specified alarms.
module Network.AWS.CloudWatch.V2010_08_01.EnableAlarmActions
    (
    -- * Request
      EnableAlarmActions
    -- ** Request constructor
    , mkEnableAlarmActionsInput
    -- ** Request lenses
    , eaaiAlarmNames

    -- * Response
    , EnableAlarmActionsResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'EnableAlarmActions' request.
mkEnableAlarmActionsInput :: [Text] -- ^ 'eaaiAlarmNames'
                          -> EnableAlarmActions
mkEnableAlarmActionsInput p1 = EnableAlarmActions
    { _eaaiAlarmNames = p1
    }
{-# INLINE mkEnableAlarmActionsInput #-}

newtype EnableAlarmActions = EnableAlarmActions
    { _eaaiAlarmNames :: [Text]
      -- ^ The names of the alarms to enable actions for.
    } deriving (Show, Generic)

-- | The names of the alarms to enable actions for.
eaaiAlarmNames :: Lens' EnableAlarmActions ([Text])
eaaiAlarmNames = lens _eaaiAlarmNames (\s a -> s { _eaaiAlarmNames = a })
{-# INLINE eaaiAlarmNames #-}

instance ToQuery EnableAlarmActions where
    toQuery = genericQuery def

data EnableAlarmActionsResponse = EnableAlarmActionsResponse
    deriving (Eq, Show, Generic)

instance AWSRequest EnableAlarmActions where
    type Sv EnableAlarmActions = CloudWatch
    type Rs EnableAlarmActions = EnableAlarmActionsResponse

    request = post "EnableAlarmActions"
    response _ = nullaryResponse EnableAlarmActionsResponse
