{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatch.EnableAlarmActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the actions for the specified alarms.
module Network.AWS.CloudWatch.EnableAlarmActions
  ( -- * Creating a request
    EnableAlarmActions (..),
    mkEnableAlarmActions,

    -- ** Request lenses
    eaaAlarmNames,

    -- * Destructuring the response
    EnableAlarmActionsResponse (..),
    mkEnableAlarmActionsResponse,
  )
where

import Network.AWS.CloudWatch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkEnableAlarmActions' smart constructor.
newtype EnableAlarmActions = EnableAlarmActions'
  { -- | The names of the alarms.
    alarmNames :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableAlarmActions' with the minimum fields required to make a request.
--
-- * 'alarmNames' - The names of the alarms.
mkEnableAlarmActions ::
  EnableAlarmActions
mkEnableAlarmActions =
  EnableAlarmActions' {alarmNames = Lude.mempty}

-- | The names of the alarms.
--
-- /Note:/ Consider using 'alarmNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eaaAlarmNames :: Lens.Lens' EnableAlarmActions [Lude.Text]
eaaAlarmNames = Lens.lens (alarmNames :: EnableAlarmActions -> [Lude.Text]) (\s a -> s {alarmNames = a} :: EnableAlarmActions)
{-# DEPRECATED eaaAlarmNames "Use generic-lens or generic-optics with 'alarmNames' instead." #-}

instance Lude.AWSRequest EnableAlarmActions where
  type Rs EnableAlarmActions = EnableAlarmActionsResponse
  request = Req.postQuery cloudWatchService
  response = Res.receiveNull EnableAlarmActionsResponse'

instance Lude.ToHeaders EnableAlarmActions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath EnableAlarmActions where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableAlarmActions where
  toQuery EnableAlarmActions' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("EnableAlarmActions" :: Lude.ByteString),
        "Version" Lude.=: ("2010-08-01" :: Lude.ByteString),
        "AlarmNames" Lude.=: Lude.toQueryList "member" alarmNames
      ]

-- | /See:/ 'mkEnableAlarmActionsResponse' smart constructor.
data EnableAlarmActionsResponse = EnableAlarmActionsResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableAlarmActionsResponse' with the minimum fields required to make a request.
mkEnableAlarmActionsResponse ::
  EnableAlarmActionsResponse
mkEnableAlarmActionsResponse = EnableAlarmActionsResponse'
