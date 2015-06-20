{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.CloudWatch.DeleteAlarms
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes all specified alarms. In the event of an error, no alarms are
-- deleted.
--
-- <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DeleteAlarms.html>
module Network.AWS.CloudWatch.DeleteAlarms
    (
    -- * Request
      DeleteAlarms
    -- ** Request constructor
    , deleteAlarms
    -- ** Request lenses
    , delAlarmNames

    -- * Response
    , DeleteAlarmsResponse
    -- ** Response constructor
    , deleteAlarmsResponse
    ) where

import Network.AWS.CloudWatch.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAlarms' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delAlarmNames'
newtype DeleteAlarms = DeleteAlarms'{_delAlarmNames :: [Text]} deriving (Eq, Read, Show)

-- | 'DeleteAlarms' smart constructor.
deleteAlarms :: DeleteAlarms
deleteAlarms = DeleteAlarms'{_delAlarmNames = mempty};

-- | A list of alarms to be deleted.
delAlarmNames :: Lens' DeleteAlarms [Text]
delAlarmNames = lens _delAlarmNames (\ s a -> s{_delAlarmNames = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DeleteAlarms where
        type Sv DeleteAlarms = CloudWatch
        type Rs DeleteAlarms = DeleteAlarmsResponse
        request = post
        response = receiveNull DeleteAlarmsResponse'

instance ToHeaders DeleteAlarms where
        toHeaders = const mempty

instance ToPath DeleteAlarms where
        toPath = const "/"

instance ToQuery DeleteAlarms where
        toQuery DeleteAlarms'{..}
          = mconcat
              ["Action" =: ("DeleteAlarms" :: ByteString),
               "Version" =: ("2010-08-01" :: ByteString),
               "AlarmNames" =: toQueryList "member" _delAlarmNames]

-- | /See:/ 'deleteAlarmsResponse' smart constructor.
data DeleteAlarmsResponse = DeleteAlarmsResponse' deriving (Eq, Read, Show)

-- | 'DeleteAlarmsResponse' smart constructor.
deleteAlarmsResponse :: DeleteAlarmsResponse
deleteAlarmsResponse = DeleteAlarmsResponse';
