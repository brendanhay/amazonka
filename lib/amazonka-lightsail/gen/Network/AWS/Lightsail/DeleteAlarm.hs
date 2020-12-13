{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- An alarm is used to monitor a single metric for one of your resources. When a metric condition is met, the alarm can notify you by email, SMS text message, and a banner displayed on the Amazon Lightsail console. For more information, see <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-alarms Alarms in Amazon Lightsail> .
module Network.AWS.Lightsail.DeleteAlarm
  ( -- * Creating a request
    DeleteAlarm (..),
    mkDeleteAlarm,

    -- ** Request lenses
    daAlarmName,

    -- * Destructuring the response
    DeleteAlarmResponse (..),
    mkDeleteAlarmResponse,

    -- ** Response lenses
    darsOperations,
    darsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAlarm' smart constructor.
newtype DeleteAlarm = DeleteAlarm'
  { -- | The name of the alarm to delete.
    alarmName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAlarm' with the minimum fields required to make a request.
--
-- * 'alarmName' - The name of the alarm to delete.
mkDeleteAlarm ::
  -- | 'alarmName'
  Lude.Text ->
  DeleteAlarm
mkDeleteAlarm pAlarmName_ = DeleteAlarm' {alarmName = pAlarmName_}

-- | The name of the alarm to delete.
--
-- /Note:/ Consider using 'alarmName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daAlarmName :: Lens.Lens' DeleteAlarm Lude.Text
daAlarmName = Lens.lens (alarmName :: DeleteAlarm -> Lude.Text) (\s a -> s {alarmName = a} :: DeleteAlarm)
{-# DEPRECATED daAlarmName "Use generic-lens or generic-optics with 'alarmName' instead." #-}

instance Lude.AWSRequest DeleteAlarm where
  type Rs DeleteAlarm = DeleteAlarmResponse
  request = Req.postJSON lightsailService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteAlarmResponse'
            Lude.<$> (x Lude..?> "operations" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteAlarm where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Lightsail_20161128.DeleteAlarm" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteAlarm where
  toJSON DeleteAlarm' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("alarmName" Lude..= alarmName)])

instance Lude.ToPath DeleteAlarm where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAlarm where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteAlarmResponse' smart constructor.
data DeleteAlarmResponse = DeleteAlarmResponse'
  { -- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
    operations :: Lude.Maybe [Operation],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAlarmResponse' with the minimum fields required to make a request.
--
-- * 'operations' - An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
-- * 'responseStatus' - The response status code.
mkDeleteAlarmResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteAlarmResponse
mkDeleteAlarmResponse pResponseStatus_ =
  DeleteAlarmResponse'
    { operations = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of objects that describe the result of the action, such as the status of the request, the timestamp of the request, and the resources affected by the request.
--
-- /Note:/ Consider using 'operations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsOperations :: Lens.Lens' DeleteAlarmResponse (Lude.Maybe [Operation])
darsOperations = Lens.lens (operations :: DeleteAlarmResponse -> Lude.Maybe [Operation]) (\s a -> s {operations = a} :: DeleteAlarmResponse)
{-# DEPRECATED darsOperations "Use generic-lens or generic-optics with 'operations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
darsResponseStatus :: Lens.Lens' DeleteAlarmResponse Lude.Int
darsResponseStatus = Lens.lens (responseStatus :: DeleteAlarmResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteAlarmResponse)
{-# DEPRECATED darsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
