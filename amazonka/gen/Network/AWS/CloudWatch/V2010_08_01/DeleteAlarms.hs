{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.CloudWatch.V2010_08_01.DeleteAlarms where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.V2010_08_01.Types
import Network.AWS.Prelude

data DeleteAlarms = DeleteAlarms
    { _dajAlarmNames :: [Text]
      -- ^ A list of alarms to be deleted.
    } deriving (Show, Generic)

makeLenses ''DeleteAlarms

instance ToQuery DeleteAlarms where
    toQuery = genericToQuery def

data DeleteAlarmsResponse = DeleteAlarmsResponse
    deriving (Eq, Show, Generic)

makeLenses ''DeleteAlarmsResponse

instance AWSRequest DeleteAlarms where
    type Sv DeleteAlarms = CloudWatch
    type Rs DeleteAlarms = DeleteAlarmsResponse

    request = post "DeleteAlarms"
    response _ _ = return (Right DeleteAlarmsResponse)
