{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.DisableProactiveEngagement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes authorization from the DDoS Response Team (DRT) to notify contacts about escalations to the DRT and to initiate proactive customer support.
module Network.AWS.Shield.DisableProactiveEngagement
  ( -- * Creating a request
    DisableProactiveEngagement (..),
    mkDisableProactiveEngagement,

    -- * Destructuring the response
    DisableProactiveEngagementResponse (..),
    mkDisableProactiveEngagementResponse,

    -- ** Response lenses
    dpersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkDisableProactiveEngagement' smart constructor.
data DisableProactiveEngagement = DisableProactiveEngagement'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableProactiveEngagement' with the minimum fields required to make a request.
mkDisableProactiveEngagement ::
  DisableProactiveEngagement
mkDisableProactiveEngagement = DisableProactiveEngagement'

instance Lude.AWSRequest DisableProactiveEngagement where
  type
    Rs DisableProactiveEngagement =
      DisableProactiveEngagementResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DisableProactiveEngagementResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DisableProactiveEngagement where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShield_20160616.DisableProactiveEngagement" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DisableProactiveEngagement where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DisableProactiveEngagement where
  toPath = Lude.const "/"

instance Lude.ToQuery DisableProactiveEngagement where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDisableProactiveEngagementResponse' smart constructor.
newtype DisableProactiveEngagementResponse = DisableProactiveEngagementResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisableProactiveEngagementResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDisableProactiveEngagementResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DisableProactiveEngagementResponse
mkDisableProactiveEngagementResponse pResponseStatus_ =
  DisableProactiveEngagementResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpersResponseStatus :: Lens.Lens' DisableProactiveEngagementResponse Lude.Int
dpersResponseStatus = Lens.lens (responseStatus :: DisableProactiveEngagementResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DisableProactiveEngagementResponse)
{-# DEPRECATED dpersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
