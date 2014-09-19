{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.DeleteAlarms
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
module Network.AWS.CloudWatch.DeleteAlarms
    (
    -- * Request
      DeleteAlarms
    -- ** Request constructor
    , deleteAlarms
    -- ** Request lenses
    , daAlarmNames

    -- * Response
    , DeleteAlarmsResponse
    -- ** Response constructor
    , deleteAlarmsResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types
import Network.AWS.Prelude

newtype DeleteAlarms = DeleteAlarms
    { _daAlarmNames :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteAlarms' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AlarmNames ::@ @[Text]@
--
deleteAlarms :: [Text] -- ^ 'daAlarmNames'
             -> DeleteAlarms
deleteAlarms p1 = DeleteAlarms
    { _daAlarmNames = p1
    }

-- | A list of alarms to be deleted.
daAlarmNames :: Lens' DeleteAlarms [Text]
daAlarmNames = lens _daAlarmNames (\s a -> s { _daAlarmNames = a })

instance ToQuery DeleteAlarms where
    toQuery = genericQuery def

data DeleteAlarmsResponse = DeleteAlarmsResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteAlarmsResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteAlarmsResponse :: DeleteAlarmsResponse
deleteAlarmsResponse = DeleteAlarmsResponse

instance AWSRequest DeleteAlarms where
    type Sv DeleteAlarms = CloudWatch
    type Rs DeleteAlarms = DeleteAlarmsResponse

    request = post "DeleteAlarms"
    response _ = nullaryResponse DeleteAlarmsResponse
