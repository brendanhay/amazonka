{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.SetV2LoggingLevel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the logging level.
module Network.AWS.IoT.SetV2LoggingLevel
  ( -- * Creating a request
    SetV2LoggingLevel (..),
    mkSetV2LoggingLevel,

    -- ** Request lenses
    svllLogTarget,
    svllLogLevel,

    -- * Destructuring the response
    SetV2LoggingLevelResponse (..),
    mkSetV2LoggingLevelResponse,
  )
where

import Network.AWS.IoT.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkSetV2LoggingLevel' smart constructor.
data SetV2LoggingLevel = SetV2LoggingLevel'
  { logTarget :: LogTarget,
    logLevel :: LogLevel
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetV2LoggingLevel' with the minimum fields required to make a request.
--
-- * 'logLevel' - The log level.
-- * 'logTarget' - The log target.
mkSetV2LoggingLevel ::
  -- | 'logTarget'
  LogTarget ->
  -- | 'logLevel'
  LogLevel ->
  SetV2LoggingLevel
mkSetV2LoggingLevel pLogTarget_ pLogLevel_ =
  SetV2LoggingLevel'
    { logTarget = pLogTarget_,
      logLevel = pLogLevel_
    }

-- | The log target.
--
-- /Note:/ Consider using 'logTarget' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svllLogTarget :: Lens.Lens' SetV2LoggingLevel LogTarget
svllLogTarget = Lens.lens (logTarget :: SetV2LoggingLevel -> LogTarget) (\s a -> s {logTarget = a} :: SetV2LoggingLevel)
{-# DEPRECATED svllLogTarget "Use generic-lens or generic-optics with 'logTarget' instead." #-}

-- | The log level.
--
-- /Note:/ Consider using 'logLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svllLogLevel :: Lens.Lens' SetV2LoggingLevel LogLevel
svllLogLevel = Lens.lens (logLevel :: SetV2LoggingLevel -> LogLevel) (\s a -> s {logLevel = a} :: SetV2LoggingLevel)
{-# DEPRECATED svllLogLevel "Use generic-lens or generic-optics with 'logLevel' instead." #-}

instance Lude.AWSRequest SetV2LoggingLevel where
  type Rs SetV2LoggingLevel = SetV2LoggingLevelResponse
  request = Req.postJSON ioTService
  response = Res.receiveNull SetV2LoggingLevelResponse'

instance Lude.ToHeaders SetV2LoggingLevel where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON SetV2LoggingLevel where
  toJSON SetV2LoggingLevel' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("logTarget" Lude..= logTarget),
            Lude.Just ("logLevel" Lude..= logLevel)
          ]
      )

instance Lude.ToPath SetV2LoggingLevel where
  toPath = Lude.const "/v2LoggingLevel"

instance Lude.ToQuery SetV2LoggingLevel where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkSetV2LoggingLevelResponse' smart constructor.
data SetV2LoggingLevelResponse = SetV2LoggingLevelResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetV2LoggingLevelResponse' with the minimum fields required to make a request.
mkSetV2LoggingLevelResponse ::
  SetV2LoggingLevelResponse
mkSetV2LoggingLevelResponse = SetV2LoggingLevelResponse'
