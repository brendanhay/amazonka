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

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | The input for the SetLoggingOptions operation.
--
-- /See:/ 'mkSetLoggingOptions' smart constructor.
newtype SetLoggingOptions = SetLoggingOptions'
  { -- | The logging options payload.
    loggingOptionsPayload :: LoggingOptionsPayload
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetLoggingOptions' with the minimum fields required to make a request.
--
-- * 'loggingOptionsPayload' - The logging options payload.
mkSetLoggingOptions ::
  -- | 'loggingOptionsPayload'
  LoggingOptionsPayload ->
  SetLoggingOptions
mkSetLoggingOptions pLoggingOptionsPayload_ =
  SetLoggingOptions'
    { loggingOptionsPayload =
        pLoggingOptionsPayload_
    }

-- | The logging options payload.
--
-- /Note:/ Consider using 'loggingOptionsPayload' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sloLoggingOptionsPayload :: Lens.Lens' SetLoggingOptions LoggingOptionsPayload
sloLoggingOptionsPayload = Lens.lens (loggingOptionsPayload :: SetLoggingOptions -> LoggingOptionsPayload) (\s a -> s {loggingOptionsPayload = a} :: SetLoggingOptions)
{-# DEPRECATED sloLoggingOptionsPayload "Use generic-lens or generic-optics with 'loggingOptionsPayload' instead." #-}

instance Lude.AWSRequest SetLoggingOptions where
  type Rs SetLoggingOptions = SetLoggingOptionsResponse
  request = Req.postJSON ioTService
  response = Res.receiveNull SetLoggingOptionsResponse'

instance Lude.ToHeaders SetLoggingOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON SetLoggingOptions where
  toJSON SetLoggingOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("loggingOptionsPayload" Lude..= loggingOptionsPayload)
          ]
      )

instance Lude.ToPath SetLoggingOptions where
  toPath = Lude.const "/loggingOptions"

instance Lude.ToQuery SetLoggingOptions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetLoggingOptionsResponse' smart constructor.
data SetLoggingOptionsResponse = SetLoggingOptionsResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetLoggingOptionsResponse' with the minimum fields required to make a request.
mkSetLoggingOptionsResponse ::
  SetLoggingOptionsResponse
mkSetLoggingOptionsResponse = SetLoggingOptionsResponse'
