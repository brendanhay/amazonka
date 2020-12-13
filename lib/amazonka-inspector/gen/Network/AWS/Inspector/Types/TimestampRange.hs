{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.TimestampRange
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.TimestampRange
  ( TimestampRange (..),

    -- * Smart constructor
    mkTimestampRange,

    -- * Lenses
    trEndDate,
    trBeginDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This data type is used in the 'AssessmentRunFilter' data type.
--
-- /See:/ 'mkTimestampRange' smart constructor.
data TimestampRange = TimestampRange'
  { -- | The maximum value of the timestamp range.
    endDate :: Lude.Maybe Lude.Timestamp,
    -- | The minimum value of the timestamp range.
    beginDate :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TimestampRange' with the minimum fields required to make a request.
--
-- * 'endDate' - The maximum value of the timestamp range.
-- * 'beginDate' - The minimum value of the timestamp range.
mkTimestampRange ::
  TimestampRange
mkTimestampRange =
  TimestampRange' {endDate = Lude.Nothing, beginDate = Lude.Nothing}

-- | The maximum value of the timestamp range.
--
-- /Note:/ Consider using 'endDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trEndDate :: Lens.Lens' TimestampRange (Lude.Maybe Lude.Timestamp)
trEndDate = Lens.lens (endDate :: TimestampRange -> Lude.Maybe Lude.Timestamp) (\s a -> s {endDate = a} :: TimestampRange)
{-# DEPRECATED trEndDate "Use generic-lens or generic-optics with 'endDate' instead." #-}

-- | The minimum value of the timestamp range.
--
-- /Note:/ Consider using 'beginDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trBeginDate :: Lens.Lens' TimestampRange (Lude.Maybe Lude.Timestamp)
trBeginDate = Lens.lens (beginDate :: TimestampRange -> Lude.Maybe Lude.Timestamp) (\s a -> s {beginDate = a} :: TimestampRange)
{-# DEPRECATED trBeginDate "Use generic-lens or generic-optics with 'beginDate' instead." #-}

instance Lude.ToJSON TimestampRange where
  toJSON TimestampRange' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("endDate" Lude..=) Lude.<$> endDate,
            ("beginDate" Lude..=) Lude.<$> beginDate
          ]
      )
