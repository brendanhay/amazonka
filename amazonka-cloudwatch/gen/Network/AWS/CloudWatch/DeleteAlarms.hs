{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DeleteAlarms
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified alarms. In the event of an error, no alarms are deleted.
--
--
module Network.AWS.CloudWatch.DeleteAlarms
    (
    -- * Creating a Request
      deleteAlarms
    , DeleteAlarms
    -- * Request Lenses
    , dAlarmNames

    -- * Destructuring the Response
    , deleteAlarmsResponse
    , DeleteAlarmsResponse
    ) where

import Network.AWS.CloudWatch.Types
import Network.AWS.CloudWatch.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAlarms' smart constructor.
newtype DeleteAlarms = DeleteAlarms'
  { _dAlarmNames :: [Text]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAlarms' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dAlarmNames' - The alarms to be deleted.
deleteAlarms
    :: DeleteAlarms
deleteAlarms = DeleteAlarms' {_dAlarmNames = mempty}


-- | The alarms to be deleted.
dAlarmNames :: Lens' DeleteAlarms [Text]
dAlarmNames = lens _dAlarmNames (\ s a -> s{_dAlarmNames = a}) . _Coerce

instance AWSRequest DeleteAlarms where
        type Rs DeleteAlarms = DeleteAlarmsResponse
        request = postQuery cloudWatch
        response = receiveNull DeleteAlarmsResponse'

instance Hashable DeleteAlarms where

instance NFData DeleteAlarms where

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
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAlarmsResponse' with the minimum fields required to make a request.
--
deleteAlarmsResponse
    :: DeleteAlarmsResponse
deleteAlarmsResponse = DeleteAlarmsResponse'


instance NFData DeleteAlarmsResponse where
