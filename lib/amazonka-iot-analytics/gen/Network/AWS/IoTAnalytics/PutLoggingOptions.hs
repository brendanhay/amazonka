{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.PutLoggingOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets or updates the AWS IoT Analytics logging options.
--
-- If you update the value of any @loggingOptions@ field, it takes up to one minute for the change to take effect. Also, if you change the policy attached to the role you specified in the @roleArn@ field (for example, to correct an invalid policy), it takes up to five minutes for that change to take effect.
module Network.AWS.IoTAnalytics.PutLoggingOptions
  ( -- * Creating a request
    PutLoggingOptions (..),
    mkPutLoggingOptions,

    -- ** Request lenses
    ploLoggingOptions,

    -- * Destructuring the response
    PutLoggingOptionsResponse (..),
    mkPutLoggingOptionsResponse,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkPutLoggingOptions' smart constructor.
newtype PutLoggingOptions = PutLoggingOptions'
  { -- | The new values of the AWS IoT Analytics logging options.
    loggingOptions :: LoggingOptions
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutLoggingOptions' with the minimum fields required to make a request.
--
-- * 'loggingOptions' - The new values of the AWS IoT Analytics logging options.
mkPutLoggingOptions ::
  -- | 'loggingOptions'
  LoggingOptions ->
  PutLoggingOptions
mkPutLoggingOptions pLoggingOptions_ =
  PutLoggingOptions' {loggingOptions = pLoggingOptions_}

-- | The new values of the AWS IoT Analytics logging options.
--
-- /Note:/ Consider using 'loggingOptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ploLoggingOptions :: Lens.Lens' PutLoggingOptions LoggingOptions
ploLoggingOptions = Lens.lens (loggingOptions :: PutLoggingOptions -> LoggingOptions) (\s a -> s {loggingOptions = a} :: PutLoggingOptions)
{-# DEPRECATED ploLoggingOptions "Use generic-lens or generic-optics with 'loggingOptions' instead." #-}

instance Lude.AWSRequest PutLoggingOptions where
  type Rs PutLoggingOptions = PutLoggingOptionsResponse
  request = Req.putJSON ioTAnalyticsService
  response = Res.receiveNull PutLoggingOptionsResponse'

instance Lude.ToHeaders PutLoggingOptions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON PutLoggingOptions where
  toJSON PutLoggingOptions' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("loggingOptions" Lude..= loggingOptions)]
      )

instance Lude.ToPath PutLoggingOptions where
  toPath = Lude.const "/logging"

instance Lude.ToQuery PutLoggingOptions where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkPutLoggingOptionsResponse' smart constructor.
data PutLoggingOptionsResponse = PutLoggingOptionsResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutLoggingOptionsResponse' with the minimum fields required to make a request.
mkPutLoggingOptionsResponse ::
  PutLoggingOptionsResponse
mkPutLoggingOptionsResponse = PutLoggingOptionsResponse'
