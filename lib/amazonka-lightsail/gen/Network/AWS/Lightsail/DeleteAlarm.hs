{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.DeleteAlarm
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an alarm.
--
--
-- An alarm is used to monitor a single metric for one of your resources. When a metric condition is met, the alarm can notify you by email, SMS text message, and a banner displayed on the Amazon Lightsail console. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail> .
module Network.AWS.Lightsail.DeleteAlarm
  ( -- * Creating a Request
    deleteAlarm,
    DeleteAlarm,

    -- * Request Lenses
    daAlarmName,

    -- * Destructuring the Response
    deleteAlarmResponse,
    DeleteAlarmResponse,

    -- * Response Lenses
    darsOperations,
    darsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Lightsail.Types
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteAlarm' smart constructor.
newtype DeleteAlarm = DeleteAlarm' {_daAlarmName :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAlarm' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAlarmName' - The name of the alarm to delete.
deleteAlarm ::
  -- | 'daAlarmName'
  Text ->
  DeleteAlarm
deleteAlarm pAlarmName_ = DeleteAlarm' {_daAlarmName = pAlarmName_}

-- | The name of the alarm to delete.
daAlarmName :: Lens' DeleteAlarm Text
daAlarmName = lens _daAlarmName (\s a -> s {_daAlarmName = a})

instance AWSRequest DeleteAlarm where
  type Rs DeleteAlarm = DeleteAlarmResponse
  request = postJSON lightsail
  response =
    receiveJSON
      ( \s h x ->
          DeleteAlarmResponse'
            <$> (x .?> "operations" .!@ mempty) <*> (pure (fromEnum s))
      )

instance Hashable DeleteAlarm

instance NFData DeleteAlarm

instance ToHeaders DeleteAlarm where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("Lightsail_20161128.DeleteAlarm" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteAlarm where
  toJSON DeleteAlarm' {..} =
    object (catMaybes [Just ("alarmName" .= _daAlarmName)])

instance ToPath DeleteAlarm where
  toPath = const "/"

instance ToQuery DeleteAlarm where
  toQuery = const mempty

-- | /See:/ 'deleteAlarmResponse' smart constructor.
data DeleteAlarmResponse = DeleteAlarmResponse'
  { _darsOperations ::
      !(Maybe [Operation]),
    _darsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteAlarmResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'darsOperations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- * 'darsResponseStatus' - -- | The response status code.
deleteAlarmResponse ::
  -- | 'darsResponseStatus'
  Int ->
  DeleteAlarmResponse
deleteAlarmResponse pResponseStatus_ =
  DeleteAlarmResponse'
    { _darsOperations = Nothing,
      _darsResponseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
darsOperations :: Lens' DeleteAlarmResponse [Operation]
darsOperations = lens _darsOperations (\s a -> s {_darsOperations = a}) . _Default . _Coerce

-- | -- | The response status code.
darsResponseStatus :: Lens' DeleteAlarmResponse Int
darsResponseStatus = lens _darsResponseStatus (\s a -> s {_darsResponseStatus = a})

instance NFData DeleteAlarmResponse
