{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StepFunctions.Types.HistoryEventExecutionDataDetails
  ( HistoryEventExecutionDataDetails (..),

    -- * Smart constructor
    mkHistoryEventExecutionDataDetails,

    -- * Lenses
    heeddTruncated,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides details about input or output in an execution history event.
--
-- /See:/ 'mkHistoryEventExecutionDataDetails' smart constructor.
newtype HistoryEventExecutionDataDetails = HistoryEventExecutionDataDetails'
  { truncated ::
      Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HistoryEventExecutionDataDetails' with the minimum fields required to make a request.
--
-- * 'truncated' - Indicates whether input or output was truncated in the response. Always @false@ for API calls.
mkHistoryEventExecutionDataDetails ::
  HistoryEventExecutionDataDetails
mkHistoryEventExecutionDataDetails =
  HistoryEventExecutionDataDetails' {truncated = Lude.Nothing}

-- | Indicates whether input or output was truncated in the response. Always @false@ for API calls.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heeddTruncated :: Lens.Lens' HistoryEventExecutionDataDetails (Lude.Maybe Lude.Bool)
heeddTruncated = Lens.lens (truncated :: HistoryEventExecutionDataDetails -> Lude.Maybe Lude.Bool) (\s a -> s {truncated = a} :: HistoryEventExecutionDataDetails)
{-# DEPRECATED heeddTruncated "Use generic-lens or generic-optics with 'truncated' instead." #-}

instance Lude.FromJSON HistoryEventExecutionDataDetails where
  parseJSON =
    Lude.withObject
      "HistoryEventExecutionDataDetails"
      ( \x ->
          HistoryEventExecutionDataDetails'
            Lude.<$> (x Lude..:? "truncated")
      )
