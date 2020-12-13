{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.ActivateEventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Activates a partner event source that has been deactivated. Once activated, your matching event bus will start receiving events from the event source.
module Network.AWS.CloudWatchEvents.ActivateEventSource
  ( -- * Creating a request
    ActivateEventSource (..),
    mkActivateEventSource,

    -- ** Request lenses
    aesName,

    -- * Destructuring the response
    ActivateEventSourceResponse (..),
    mkActivateEventSourceResponse,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkActivateEventSource' smart constructor.
newtype ActivateEventSource = ActivateEventSource'
  { -- | The name of the partner event source to activate.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivateEventSource' with the minimum fields required to make a request.
--
-- * 'name' - The name of the partner event source to activate.
mkActivateEventSource ::
  -- | 'name'
  Lude.Text ->
  ActivateEventSource
mkActivateEventSource pName_ = ActivateEventSource' {name = pName_}

-- | The name of the partner event source to activate.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aesName :: Lens.Lens' ActivateEventSource Lude.Text
aesName = Lens.lens (name :: ActivateEventSource -> Lude.Text) (\s a -> s {name = a} :: ActivateEventSource)
{-# DEPRECATED aesName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest ActivateEventSource where
  type Rs ActivateEventSource = ActivateEventSourceResponse
  request = Req.postJSON cloudWatchEventsService
  response = Res.receiveNull ActivateEventSourceResponse'

instance Lude.ToHeaders ActivateEventSource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.ActivateEventSource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ActivateEventSource where
  toJSON ActivateEventSource' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath ActivateEventSource where
  toPath = Lude.const "/"

instance Lude.ToQuery ActivateEventSource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkActivateEventSourceResponse' smart constructor.
data ActivateEventSourceResponse = ActivateEventSourceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ActivateEventSourceResponse' with the minimum fields required to make a request.
mkActivateEventSourceResponse ::
  ActivateEventSourceResponse
mkActivateEventSourceResponse = ActivateEventSourceResponse'
