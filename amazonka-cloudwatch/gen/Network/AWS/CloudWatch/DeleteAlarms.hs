{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DeleteAlarms
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes all specified alarms. In the event of an error, no alarms are
-- deleted.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonCloudWatch/latest/APIReference/API_DeleteAlarms.html AWS API Reference> for DeleteAlarms.
module Network.AWS.CloudWatch.DeleteAlarms
    (
    -- * Creating a Request
      DeleteAlarms
    , deleteAlarms
    -- * Request Lenses
    , dAlarmNames

    -- * Destructuring the Response
    , DeleteAlarmsResponse
    , deleteAlarmsResponse
    ) where

import           Network.AWS.CloudWatch.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deleteAlarms' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dAlarmNames'
newtype DeleteAlarms = DeleteAlarms'
    { _dAlarmNames :: [Text]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAlarms' smart constructor.
deleteAlarms :: DeleteAlarms
deleteAlarms =
    DeleteAlarms'
    { _dAlarmNames = mempty
    }

-- | A list of alarms to be deleted.
dAlarmNames :: Lens' DeleteAlarms [Text]
dAlarmNames = lens _dAlarmNames (\ s a -> s{_dAlarmNames = a}) . _Coerce;

instance AWSRequest DeleteAlarms where
        type Sv DeleteAlarms = CloudWatch
        type Rs DeleteAlarms = DeleteAlarmsResponse
        request = postQuery
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
               "AlarmNames" =: toQueryList "member" _dAlarmNames]

-- | /See:/ 'deleteAlarmsResponse' smart constructor.
data DeleteAlarmsResponse =
    DeleteAlarmsResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAlarmsResponse' smart constructor.
deleteAlarmsResponse :: DeleteAlarmsResponse
deleteAlarmsResponse = DeleteAlarmsResponse'
