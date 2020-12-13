{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.EnableProactiveEngagement
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Authorizes the DDoS Response Team (DRT) to use email and phone to notify contacts about escalations to the DRT and to initiate proactive customer support.
module Network.AWS.Shield.EnableProactiveEngagement
  ( -- * Creating a request
    EnableProactiveEngagement (..),
    mkEnableProactiveEngagement,

    -- * Destructuring the response
    EnableProactiveEngagementResponse (..),
    mkEnableProactiveEngagementResponse,

    -- ** Response lenses
    epersResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Shield.Types

-- | /See:/ 'mkEnableProactiveEngagement' smart constructor.
data EnableProactiveEngagement = EnableProactiveEngagement'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableProactiveEngagement' with the minimum fields required to make a request.
mkEnableProactiveEngagement ::
  EnableProactiveEngagement
mkEnableProactiveEngagement = EnableProactiveEngagement'

instance Lude.AWSRequest EnableProactiveEngagement where
  type
    Rs EnableProactiveEngagement =
      EnableProactiveEngagementResponse
  request = Req.postJSON shieldService
  response =
    Res.receiveEmpty
      ( \s h x ->
          EnableProactiveEngagementResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders EnableProactiveEngagement where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShield_20160616.EnableProactiveEngagement" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON EnableProactiveEngagement where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath EnableProactiveEngagement where
  toPath = Lude.const "/"

instance Lude.ToQuery EnableProactiveEngagement where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkEnableProactiveEngagementResponse' smart constructor.
newtype EnableProactiveEngagementResponse = EnableProactiveEngagementResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EnableProactiveEngagementResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkEnableProactiveEngagementResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  EnableProactiveEngagementResponse
mkEnableProactiveEngagementResponse pResponseStatus_ =
  EnableProactiveEngagementResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
epersResponseStatus :: Lens.Lens' EnableProactiveEngagementResponse Lude.Int
epersResponseStatus = Lens.lens (responseStatus :: EnableProactiveEngagementResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: EnableProactiveEngagementResponse)
{-# DEPRECATED epersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
