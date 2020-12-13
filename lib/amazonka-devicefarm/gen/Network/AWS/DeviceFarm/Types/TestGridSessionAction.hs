{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DeviceFarm.Types.TestGridSessionAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DeviceFarm.Types.TestGridSessionAction
  ( TestGridSessionAction (..),

    -- * Smart constructor
    mkTestGridSessionAction,

    -- * Lenses
    tgsaAction,
    tgsaDuration,
    tgsaRequestMethod,
    tgsaStarted,
    tgsaStatusCode,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An action taken by a 'TestGridSession' browser instance.
--
-- /See:/ 'mkTestGridSessionAction' smart constructor.
data TestGridSessionAction = TestGridSessionAction'
  { -- | The action taken by the session.
    action :: Lude.Maybe Lude.Text,
    -- | The time, in milliseconds, that the action took to complete in the browser.
    duration :: Lude.Maybe Lude.Integer,
    -- | HTTP method that the browser used to make the request.
    requestMethod :: Lude.Maybe Lude.Text,
    -- | The time that the session invoked the action.
    started :: Lude.Maybe Lude.Timestamp,
    -- | HTTP status code returned to the browser when the action was taken.
    statusCode :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TestGridSessionAction' with the minimum fields required to make a request.
--
-- * 'action' - The action taken by the session.
-- * 'duration' - The time, in milliseconds, that the action took to complete in the browser.
-- * 'requestMethod' - HTTP method that the browser used to make the request.
-- * 'started' - The time that the session invoked the action.
-- * 'statusCode' - HTTP status code returned to the browser when the action was taken.
mkTestGridSessionAction ::
  TestGridSessionAction
mkTestGridSessionAction =
  TestGridSessionAction'
    { action = Lude.Nothing,
      duration = Lude.Nothing,
      requestMethod = Lude.Nothing,
      started = Lude.Nothing,
      statusCode = Lude.Nothing
    }

-- | The action taken by the session.
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsaAction :: Lens.Lens' TestGridSessionAction (Lude.Maybe Lude.Text)
tgsaAction = Lens.lens (action :: TestGridSessionAction -> Lude.Maybe Lude.Text) (\s a -> s {action = a} :: TestGridSessionAction)
{-# DEPRECATED tgsaAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The time, in milliseconds, that the action took to complete in the browser.
--
-- /Note:/ Consider using 'duration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsaDuration :: Lens.Lens' TestGridSessionAction (Lude.Maybe Lude.Integer)
tgsaDuration = Lens.lens (duration :: TestGridSessionAction -> Lude.Maybe Lude.Integer) (\s a -> s {duration = a} :: TestGridSessionAction)
{-# DEPRECATED tgsaDuration "Use generic-lens or generic-optics with 'duration' instead." #-}

-- | HTTP method that the browser used to make the request.
--
-- /Note:/ Consider using 'requestMethod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsaRequestMethod :: Lens.Lens' TestGridSessionAction (Lude.Maybe Lude.Text)
tgsaRequestMethod = Lens.lens (requestMethod :: TestGridSessionAction -> Lude.Maybe Lude.Text) (\s a -> s {requestMethod = a} :: TestGridSessionAction)
{-# DEPRECATED tgsaRequestMethod "Use generic-lens or generic-optics with 'requestMethod' instead." #-}

-- | The time that the session invoked the action.
--
-- /Note:/ Consider using 'started' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsaStarted :: Lens.Lens' TestGridSessionAction (Lude.Maybe Lude.Timestamp)
tgsaStarted = Lens.lens (started :: TestGridSessionAction -> Lude.Maybe Lude.Timestamp) (\s a -> s {started = a} :: TestGridSessionAction)
{-# DEPRECATED tgsaStarted "Use generic-lens or generic-optics with 'started' instead." #-}

-- | HTTP status code returned to the browser when the action was taken.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tgsaStatusCode :: Lens.Lens' TestGridSessionAction (Lude.Maybe Lude.Text)
tgsaStatusCode = Lens.lens (statusCode :: TestGridSessionAction -> Lude.Maybe Lude.Text) (\s a -> s {statusCode = a} :: TestGridSessionAction)
{-# DEPRECATED tgsaStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Lude.FromJSON TestGridSessionAction where
  parseJSON =
    Lude.withObject
      "TestGridSessionAction"
      ( \x ->
          TestGridSessionAction'
            Lude.<$> (x Lude..:? "action")
            Lude.<*> (x Lude..:? "duration")
            Lude.<*> (x Lude..:? "requestMethod")
            Lude.<*> (x Lude..:? "started")
            Lude.<*> (x Lude..:? "statusCode")
      )
