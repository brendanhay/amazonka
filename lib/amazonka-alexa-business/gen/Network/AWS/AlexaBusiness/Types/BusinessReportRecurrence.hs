{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.BusinessReportRecurrence
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.BusinessReportRecurrence
  ( BusinessReportRecurrence (..),

    -- * Smart constructor
    mkBusinessReportRecurrence,

    -- * Lenses
    brrStartDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The recurrence of the reports.
--
-- /See:/ 'mkBusinessReportRecurrence' smart constructor.
newtype BusinessReportRecurrence = BusinessReportRecurrence'
  { -- | The start date.
    startDate :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BusinessReportRecurrence' with the minimum fields required to make a request.
--
-- * 'startDate' - The start date.
mkBusinessReportRecurrence ::
  BusinessReportRecurrence
mkBusinessReportRecurrence =
  BusinessReportRecurrence' {startDate = Lude.Nothing}

-- | The start date.
--
-- /Note:/ Consider using 'startDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
brrStartDate :: Lens.Lens' BusinessReportRecurrence (Lude.Maybe Lude.Text)
brrStartDate = Lens.lens (startDate :: BusinessReportRecurrence -> Lude.Maybe Lude.Text) (\s a -> s {startDate = a} :: BusinessReportRecurrence)
{-# DEPRECATED brrStartDate "Use generic-lens or generic-optics with 'startDate' instead." #-}

instance Lude.FromJSON BusinessReportRecurrence where
  parseJSON =
    Lude.withObject
      "BusinessReportRecurrence"
      ( \x ->
          BusinessReportRecurrence' Lude.<$> (x Lude..:? "StartDate")
      )

instance Lude.ToJSON BusinessReportRecurrence where
  toJSON BusinessReportRecurrence' {..} =
    Lude.object
      (Lude.catMaybes [("StartDate" Lude..=) Lude.<$> startDate])
