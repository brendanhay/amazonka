{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.DisableAlarmActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the actions for the specified alarms. When an alarm's actions are disabled, the alarm actions do not execute when the alarm state changes.
module Network.AWS.CloudWatch.DisableAlarmActions
  ( -- * Creating a request
    DisableAlarmActions (..),
    mkDisableAlarmActions,

    -- ** Request lenses
    daaAlarmNames,

    -- * Destructuring the response
    DisableAlarmActionsResponse (..),
    mkDisableAlarmActionsResponse,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDisableAlarmActions' smart constructor.
newtype DisableAlarmActions = DisableAlarmActions'
  { -- | The names of the alarms.
    alarmNames :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableAlarmActions' with the minimum fields required to make a request.
--
-- * 'alarmNames' - The names of the alarms.
mkDisableAlarmActions ::
  DisableAlarmActions
mkDisableAlarmActions =
  DisableAlarmActions' {alarmNames = Lude.mempty}

-- | The names of the alarms.
--
-- /Note:/ Consider using 'alarmNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daaAlarmNames :: Lens.Lens' DisableAlarmActions [Lude.Text]
daaAlarmNames = Lens.lens (alarmNames :: DisableAlarmActions -> [Lude.Text]) (\s a -> s {alarmNames = a} :: DisableAlarmActions)
{-# DEPRECATED daaAlarmNames "Use generic-lens or generic-optics with 'alarmNames' instead." #-}

instance Lude.AWSRequest DisableAlarmActions where
  type Rs DisableAlarmActions = DisableAlarmActionsResponse
  request = Req.postQuery cloudWatchService
  response = Res.receiveNull DisableAlarmActionsResponse'

instance Lude.ToHeaders DisableAlarmActions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisableAlarmActions where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableAlarmActions where
  toQuery DisableAlarmActions' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DisableAlarmActions" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "AlarmNames" Lude.=: Lude.toQueryList "member" alarmNames
      ]

-- | /See:/ 'mkDisableAlarmActionsResponse' smart constructor.
data DisableAlarmActionsResponse = DisableAlarmActionsResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableAlarmActionsResponse' with the minimum fields required to make a request.
mkDisableAlarmActionsResponse ::
  DisableAlarmActionsResponse
mkDisableAlarmActionsResponse = DisableAlarmActionsResponse'
