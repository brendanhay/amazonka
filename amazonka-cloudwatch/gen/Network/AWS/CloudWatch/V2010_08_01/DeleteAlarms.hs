{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.V2010_08_01.DeleteAlarms
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes all specified alarms. In the event of an error, no alarms are
-- deleted.
module Network.AWS.CloudWatch.V2010_08_01.DeleteAlarms
    (
    -- * Request
      DeleteAlarms
    -- ** Request constructor
    , deleteAlarms
    -- ** Request lenses
    , daiAlarmNames

    -- * Response
    , DeleteAlarmsResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteAlarms' request.
deleteAlarms :: [Text] -- ^ 'daiAlarmNames'
             -> DeleteAlarms
deleteAlarms p1 = DeleteAlarms
    { _daiAlarmNames = p1
    }
{-# INLINE deleteAlarms #-}

data DeleteAlarms = DeleteAlarms
    { _daiAlarmNames :: [Text]
      -- ^ A list of alarms to be deleted.
    } deriving (Show, Generic)

-- | A list of alarms to be deleted.
daiAlarmNames :: Lens' DeleteAlarms ([Text])
daiAlarmNames f x =
    f (_daiAlarmNames x)
        <&> \y -> x { _daiAlarmNames = y }
{-# INLINE daiAlarmNames #-}

instance ToQuery DeleteAlarms where
    toQuery = genericQuery def

data DeleteAlarmsResponse = DeleteAlarmsResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteAlarms where
    type Sv DeleteAlarms = CloudWatch
    type Rs DeleteAlarms = DeleteAlarmsResponse

    request = post "DeleteAlarms"
    response _ = nullaryResponse DeleteAlarmsResponse
