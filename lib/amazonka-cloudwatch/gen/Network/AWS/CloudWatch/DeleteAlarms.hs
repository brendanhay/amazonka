{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DeleteAlarms
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified alarms. You can delete up to 100 alarms in one operation. However, this total can include no more than one composite alarm. For example, you could delete 99 metric alarms and one composite alarms with one operation, but you can't delete two composite alarms with one operation.
--
-- In the event of an error, no alarms are deleted.
module Network.AWS.CloudWatch.DeleteAlarms
  ( -- * Creating a request
    DeleteAlarms (..),
    mkDeleteAlarms,

    -- ** Request lenses
    dAlarmNames,

    -- * Destructuring the response
    DeleteAlarmsResponse (..),
    mkDeleteAlarmsResponse,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteAlarms' smart constructor.
newtype DeleteAlarms = DeleteAlarms' {alarmNames :: [Lude.Text]}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAlarms' with the minimum fields required to make a request.
--
-- * 'alarmNames' - The alarms to be deleted.
mkDeleteAlarms ::
  DeleteAlarms
mkDeleteAlarms = DeleteAlarms' {alarmNames = Lude.mempty}

-- | The alarms to be deleted.
--
-- /Note:/ Consider using 'alarmNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAlarmNames :: Lens.Lens' DeleteAlarms [Lude.Text]
dAlarmNames = Lens.lens (alarmNames :: DeleteAlarms -> [Lude.Text]) (\s a -> s {alarmNames = a} :: DeleteAlarms)
{-# DEPRECATED dAlarmNames "Use generic-lens or generic-optics with 'alarmNames' instead." #-}

instance Lude.AWSRequest DeleteAlarms where
  type Rs DeleteAlarms = DeleteAlarmsResponse
  request = Req.postQuery cloudWatchService
  response = Res.receiveNull DeleteAlarmsResponse'

instance Lude.ToHeaders DeleteAlarms where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteAlarms where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteAlarms where
  toQuery DeleteAlarms' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteAlarms" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "AlarmNames" Lude.=: Lude.toQueryList "member" alarmNames
      ]

-- | /See:/ 'mkDeleteAlarmsResponse' smart constructor.
data DeleteAlarmsResponse = DeleteAlarmsResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteAlarmsResponse' with the minimum fields required to make a request.
mkDeleteAlarmsResponse ::
  DeleteAlarmsResponse
mkDeleteAlarmsResponse = DeleteAlarmsResponse'
