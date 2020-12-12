{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.StatusReport
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.StatusReport
  ( StatusReport (..),

    -- * Smart constructor
    mkStatusReport,

    -- * Lenses
    srStatus,
    srCheckedTime,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal

-- | A complex type that contains the status that one Amazon Route 53 health checker reports and the time of the health check.
--
-- /See:/ 'mkStatusReport' smart constructor.
data StatusReport = StatusReport'
  { status :: Lude.Maybe Lude.Text,
    checkedTime :: Lude.Maybe Lude.DateTime
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StatusReport' with the minimum fields required to make a request.
--
-- * 'checkedTime' - The date and time that the health checker performed the health check in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@ represents March 27, 2017 at 17:48:16.751 UTC.
-- * 'status' - A description of the status of the health check endpoint as reported by one of the Amazon Route 53 health checkers.
mkStatusReport ::
  StatusReport
mkStatusReport =
  StatusReport' {status = Lude.Nothing, checkedTime = Lude.Nothing}

-- | A description of the status of the health check endpoint as reported by one of the Amazon Route 53 health checkers.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srStatus :: Lens.Lens' StatusReport (Lude.Maybe Lude.Text)
srStatus = Lens.lens (status :: StatusReport -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: StatusReport)
{-# DEPRECATED srStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time that the health checker performed the health check in <https://en.wikipedia.org/wiki/ISO_8601 ISO 8601 format> and Coordinated Universal Time (UTC). For example, the value @2017-03-27T17:48:16.751Z@ represents March 27, 2017 at 17:48:16.751 UTC.
--
-- /Note:/ Consider using 'checkedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srCheckedTime :: Lens.Lens' StatusReport (Lude.Maybe Lude.DateTime)
srCheckedTime = Lens.lens (checkedTime :: StatusReport -> Lude.Maybe Lude.DateTime) (\s a -> s {checkedTime = a} :: StatusReport)
{-# DEPRECATED srCheckedTime "Use generic-lens or generic-optics with 'checkedTime' instead." #-}

instance Lude.FromXML StatusReport where
  parseXML x =
    StatusReport'
      Lude.<$> (x Lude..@? "Status") Lude.<*> (x Lude..@? "CheckedTime")
