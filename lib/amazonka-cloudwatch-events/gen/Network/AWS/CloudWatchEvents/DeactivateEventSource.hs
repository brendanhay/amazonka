{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.DeactivateEventSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- You can use this operation to temporarily stop receiving events from the specified partner event source. The matching event bus is not deleted.
--
-- When you deactivate a partner event source, the source goes into PENDING state. If it remains in PENDING state for more than two weeks, it is deleted.
-- To activate a deactivated partner event source, use 'ActivateEventSource' .
module Network.AWS.CloudWatchEvents.DeactivateEventSource
  ( -- * Creating a request
    DeactivateEventSource (..),
    mkDeactivateEventSource,

    -- ** Request lenses
    desfName,

    -- * Destructuring the response
    DeactivateEventSourceResponse (..),
    mkDeactivateEventSourceResponse,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeactivateEventSource' smart constructor.
newtype DeactivateEventSource = DeactivateEventSource'
  { -- | The name of the partner event source to deactivate.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeactivateEventSource' with the minimum fields required to make a request.
--
-- * 'name' - The name of the partner event source to deactivate.
mkDeactivateEventSource ::
  -- | 'name'
  Lude.Text ->
  DeactivateEventSource
mkDeactivateEventSource pName_ =
  DeactivateEventSource' {name = pName_}

-- | The name of the partner event source to deactivate.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
desfName :: Lens.Lens' DeactivateEventSource Lude.Text
desfName = Lens.lens (name :: DeactivateEventSource -> Lude.Text) (\s a -> s {name = a} :: DeactivateEventSource)
{-# DEPRECATED desfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeactivateEventSource where
  type Rs DeactivateEventSource = DeactivateEventSourceResponse
  request = Req.postJSON cloudWatchEventsService
  response = Res.receiveNull DeactivateEventSourceResponse'

instance Lude.ToHeaders DeactivateEventSource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSEvents.DeactivateEventSource" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeactivateEventSource where
  toJSON DeactivateEventSource' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeactivateEventSource where
  toPath = Lude.const "/"

instance Lude.ToQuery DeactivateEventSource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeactivateEventSourceResponse' smart constructor.
data DeactivateEventSourceResponse = DeactivateEventSourceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeactivateEventSourceResponse' with the minimum fields required to make a request.
mkDeactivateEventSourceResponse ::
  DeactivateEventSourceResponse
mkDeactivateEventSourceResponse = DeactivateEventSourceResponse'
