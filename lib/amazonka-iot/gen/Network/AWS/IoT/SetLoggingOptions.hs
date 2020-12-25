{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.SetLoggingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the logging options.
--
-- NOTE: use of this command is not recommended. Use @SetV2LoggingOptions@ instead.
module Network.AWS.IoT.SetLoggingOptions
  ( -- * Creating a request
    SetLoggingOptions (..),
    mkSetLoggingOptions,

    -- ** Request lenses
    sloLoggingOptionsPayload,

    -- * Destructuring the response
    SetLoggingOptionsResponse (..),
    mkSetLoggingOptionsResponse,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the SetLoggingOptions operation.
--
-- /See:/ 'mkSetLoggingOptions' smart constructor.
newtype SetLoggingOptions = SetLoggingOptions'
  { -- | The logging options payload.
    loggingOptionsPayload :: Types.LoggingOptionsPayload
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'SetLoggingOptions' value with any optional fields omitted.
mkSetLoggingOptions ::
  -- | 'loggingOptionsPayload'
  Types.LoggingOptionsPayload ->
  SetLoggingOptions
mkSetLoggingOptions loggingOptionsPayload =
  SetLoggingOptions' {loggingOptionsPayload}

-- | The logging options payload.
--
-- /Note:/ Consider using 'loggingOptionsPayload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sloLoggingOptionsPayload :: Lens.Lens' SetLoggingOptions Types.LoggingOptionsPayload
sloLoggingOptionsPayload = Lens.field @"loggingOptionsPayload"
{-# DEPRECATED sloLoggingOptionsPayload "Use generic-lens or generic-optics with 'loggingOptionsPayload' instead." #-}

instance Core.FromJSON SetLoggingOptions where
  toJSON SetLoggingOptions {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("loggingOptionsPayload" Core..= loggingOptionsPayload)
          ]
      )

instance Core.AWSRequest SetLoggingOptions where
  type Rs SetLoggingOptions = SetLoggingOptionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/loggingOptions",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response = Response.receiveNull SetLoggingOptionsResponse'

-- | /See:/ 'mkSetLoggingOptionsResponse' smart constructor.
data SetLoggingOptionsResponse = SetLoggingOptionsResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SetLoggingOptionsResponse' value with any optional fields omitted.
mkSetLoggingOptionsResponse ::
  SetLoggingOptionsResponse
mkSetLoggingOptionsResponse = SetLoggingOptionsResponse'
