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
      DeleteAlarmsInput
    -- ** Request constructor
    , deleteAlarmsInput
    -- ** Request lenses
    , daiAlarmNames

    -- * Response
    , DeleteAlarmsResponse
    -- ** Response constructor
    , deleteAlarmsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types

newtype DeleteAlarmsInput = DeleteAlarmsInput
    { _daiAlarmNames :: [Text]
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteAlarmsInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daiAlarmNames' @::@ ['Text']
--
deleteAlarmsInput :: DeleteAlarmsInput
deleteAlarmsInput = DeleteAlarmsInput
    { _daiAlarmNames = mempty
    }

-- | A list of alarms to be deleted.
daiAlarmNames :: Lens' DeleteAlarmsInput [Text]
daiAlarmNames = lens _daiAlarmNames (\s a -> s { _daiAlarmNames = a })
instance ToQuery DeleteAlarmsInput

instance ToPath DeleteAlarmsInput where
    toPath = const "/"

data DeleteAlarmsResponse = DeleteAlarmsResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteAlarmsResponse' constructor.
deleteAlarmsResponse :: DeleteAlarmsResponse
deleteAlarmsResponse = DeleteAlarmsResponse
instance FromXML DeleteAlarmsResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteAlarmsResponse"

instance AWSRequest DeleteAlarmsInput where
    type Sv DeleteAlarmsInput = CloudWatch
    type Rs DeleteAlarmsInput = DeleteAlarmsResponse

    request  = post "DeleteAlarms"
    response = nullaryResponse DeleteAlarmsResponse
