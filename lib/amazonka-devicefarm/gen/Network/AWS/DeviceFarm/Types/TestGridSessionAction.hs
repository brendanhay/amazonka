{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestGridSessionAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DeviceFarm.Types.TestGridSessionAction
  ( TestGridSessionAction (..)
  -- * Smart constructor
  , mkTestGridSessionAction
  -- * Lenses
  , tgsaAction
  , tgsaDuration
  , tgsaRequestMethod
  , tgsaStarted
  , tgsaStatusCode
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An action taken by a 'TestGridSession' browser instance.
--
-- /See:/ 'mkTestGridSessionAction' smart constructor.
data TestGridSessionAction = TestGridSessionAction'
  { action :: Core.Maybe Core.Text
    -- ^ The action taken by the session.
  , duration :: Core.Maybe Core.Integer
    -- ^ The time, in milliseconds, that the action took to complete in the browser.
  , requestMethod :: Core.Maybe Core.Text
    -- ^ HTTP method that the browser used to make the request.
  , started :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the session invoked the action.
  , statusCode :: Core.Maybe Core.Text
    -- ^ HTTP status code returned to the browser when the action was taken.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TestGridSessionAction' value with any optional fields omitted.
mkTestGridSessionAction
    :: TestGridSessionAction
mkTestGridSessionAction
  = TestGridSessionAction'{action = Core.Nothing,
                           duration = Core.Nothing, requestMethod = Core.Nothing,
                           started = Core.Nothing, statusCode = Core.Nothing}

-- | The action taken by the session.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsaAction :: Lens.Lens' TestGridSessionAction (Core.Maybe Core.Text)
tgsaAction = Lens.field @"action"
{-# INLINEABLE tgsaAction #-}
{-# DEPRECATED action "Use generic-lens or generic-optics with 'action' instead"  #-}

-- | The time, in milliseconds, that the action took to complete in the browser.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsaDuration :: Lens.Lens' TestGridSessionAction (Core.Maybe Core.Integer)
tgsaDuration = Lens.field @"duration"
{-# INLINEABLE tgsaDuration #-}
{-# DEPRECATED duration "Use generic-lens or generic-optics with 'duration' instead"  #-}

-- | HTTP method that the browser used to make the request.
--
-- /Note:/ Consider using 'requestMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsaRequestMethod :: Lens.Lens' TestGridSessionAction (Core.Maybe Core.Text)
tgsaRequestMethod = Lens.field @"requestMethod"
{-# INLINEABLE tgsaRequestMethod #-}
{-# DEPRECATED requestMethod "Use generic-lens or generic-optics with 'requestMethod' instead"  #-}

-- | The time that the session invoked the action.
--
-- /Note:/ Consider using 'started' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsaStarted :: Lens.Lens' TestGridSessionAction (Core.Maybe Core.NominalDiffTime)
tgsaStarted = Lens.field @"started"
{-# INLINEABLE tgsaStarted #-}
{-# DEPRECATED started "Use generic-lens or generic-optics with 'started' instead"  #-}

-- | HTTP status code returned to the browser when the action was taken.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsaStatusCode :: Lens.Lens' TestGridSessionAction (Core.Maybe Core.Text)
tgsaStatusCode = Lens.field @"statusCode"
{-# INLINEABLE tgsaStatusCode #-}
{-# DEPRECATED statusCode "Use generic-lens or generic-optics with 'statusCode' instead"  #-}

instance Core.FromJSON TestGridSessionAction where
        parseJSON
          = Core.withObject "TestGridSessionAction" Core.$
              \ x ->
                TestGridSessionAction' Core.<$>
                  (x Core..:? "action") Core.<*> x Core..:? "duration" Core.<*>
                    x Core..:? "requestMethod"
                    Core.<*> x Core..:? "started"
                    Core.<*> x Core..:? "statusCode"
