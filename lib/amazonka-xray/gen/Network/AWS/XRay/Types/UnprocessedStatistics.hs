-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.UnprocessedStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.UnprocessedStatistics
  ( UnprocessedStatistics (..),

    -- * Smart constructor
    mkUnprocessedStatistics,

    -- * Lenses
    usRuleName,
    usErrorCode,
    usMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Sampling statistics from a call to 'GetSamplingTargets' that X-Ray could not process.
--
-- /See:/ 'mkUnprocessedStatistics' smart constructor.
data UnprocessedStatistics = UnprocessedStatistics'
  { ruleName ::
      Lude.Maybe Lude.Text,
    errorCode :: Lude.Maybe Lude.Text,
    message :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnprocessedStatistics' with the minimum fields required to make a request.
--
-- * 'errorCode' - The error code.
-- * 'message' - The error message.
-- * 'ruleName' - The name of the sampling rule.
mkUnprocessedStatistics ::
  UnprocessedStatistics
mkUnprocessedStatistics =
  UnprocessedStatistics'
    { ruleName = Lude.Nothing,
      errorCode = Lude.Nothing,
      message = Lude.Nothing
    }

-- | The name of the sampling rule.
--
-- /Note:/ Consider using 'ruleName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usRuleName :: Lens.Lens' UnprocessedStatistics (Lude.Maybe Lude.Text)
usRuleName = Lens.lens (ruleName :: UnprocessedStatistics -> Lude.Maybe Lude.Text) (\s a -> s {ruleName = a} :: UnprocessedStatistics)
{-# DEPRECATED usRuleName "Use generic-lens or generic-optics with 'ruleName' instead." #-}

-- | The error code.
--
-- /Note:/ Consider using 'errorCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usErrorCode :: Lens.Lens' UnprocessedStatistics (Lude.Maybe Lude.Text)
usErrorCode = Lens.lens (errorCode :: UnprocessedStatistics -> Lude.Maybe Lude.Text) (\s a -> s {errorCode = a} :: UnprocessedStatistics)
{-# DEPRECATED usErrorCode "Use generic-lens or generic-optics with 'errorCode' instead." #-}

-- | The error message.
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
usMessage :: Lens.Lens' UnprocessedStatistics (Lude.Maybe Lude.Text)
usMessage = Lens.lens (message :: UnprocessedStatistics -> Lude.Maybe Lude.Text) (\s a -> s {message = a} :: UnprocessedStatistics)
{-# DEPRECATED usMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Lude.FromJSON UnprocessedStatistics where
  parseJSON =
    Lude.withObject
      "UnprocessedStatistics"
      ( \x ->
          UnprocessedStatistics'
            Lude.<$> (x Lude..:? "RuleName")
            Lude.<*> (x Lude..:? "ErrorCode")
            Lude.<*> (x Lude..:? "Message")
      )
