{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.SeveritySummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.SeveritySummary
  ( SeveritySummary (..),

    -- * Smart constructor
    mkSeveritySummary,

    -- * Lenses
    ssCriticalCount,
    ssHighCount,
    ssInformationalCount,
    ssLowCount,
    ssMediumCount,
    ssUnspecifiedCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The number of managed instances found for each patch severity level defined in the request filter.
--
-- /See:/ 'mkSeveritySummary' smart constructor.
data SeveritySummary = SeveritySummary'
  { -- | The total number of resources or compliance items that have a severity level of critical. Critical severity is determined by the organization that published the compliance items.
    criticalCount :: Core.Maybe Core.Int,
    -- | The total number of resources or compliance items that have a severity level of high. High severity is determined by the organization that published the compliance items.
    highCount :: Core.Maybe Core.Int,
    -- | The total number of resources or compliance items that have a severity level of informational. Informational severity is determined by the organization that published the compliance items.
    informationalCount :: Core.Maybe Core.Int,
    -- | The total number of resources or compliance items that have a severity level of low. Low severity is determined by the organization that published the compliance items.
    lowCount :: Core.Maybe Core.Int,
    -- | The total number of resources or compliance items that have a severity level of medium. Medium severity is determined by the organization that published the compliance items.
    mediumCount :: Core.Maybe Core.Int,
    -- | The total number of resources or compliance items that have a severity level of unspecified. Unspecified severity is determined by the organization that published the compliance items.
    unspecifiedCount :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SeveritySummary' value with any optional fields omitted.
mkSeveritySummary ::
  SeveritySummary
mkSeveritySummary =
  SeveritySummary'
    { criticalCount = Core.Nothing,
      highCount = Core.Nothing,
      informationalCount = Core.Nothing,
      lowCount = Core.Nothing,
      mediumCount = Core.Nothing,
      unspecifiedCount = Core.Nothing
    }

-- | The total number of resources or compliance items that have a severity level of critical. Critical severity is determined by the organization that published the compliance items.
--
-- /Note:/ Consider using 'criticalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCriticalCount :: Lens.Lens' SeveritySummary (Core.Maybe Core.Int)
ssCriticalCount = Lens.field @"criticalCount"
{-# DEPRECATED ssCriticalCount "Use generic-lens or generic-optics with 'criticalCount' instead." #-}

-- | The total number of resources or compliance items that have a severity level of high. High severity is determined by the organization that published the compliance items.
--
-- /Note:/ Consider using 'highCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssHighCount :: Lens.Lens' SeveritySummary (Core.Maybe Core.Int)
ssHighCount = Lens.field @"highCount"
{-# DEPRECATED ssHighCount "Use generic-lens or generic-optics with 'highCount' instead." #-}

-- | The total number of resources or compliance items that have a severity level of informational. Informational severity is determined by the organization that published the compliance items.
--
-- /Note:/ Consider using 'informationalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssInformationalCount :: Lens.Lens' SeveritySummary (Core.Maybe Core.Int)
ssInformationalCount = Lens.field @"informationalCount"
{-# DEPRECATED ssInformationalCount "Use generic-lens or generic-optics with 'informationalCount' instead." #-}

-- | The total number of resources or compliance items that have a severity level of low. Low severity is determined by the organization that published the compliance items.
--
-- /Note:/ Consider using 'lowCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssLowCount :: Lens.Lens' SeveritySummary (Core.Maybe Core.Int)
ssLowCount = Lens.field @"lowCount"
{-# DEPRECATED ssLowCount "Use generic-lens or generic-optics with 'lowCount' instead." #-}

-- | The total number of resources or compliance items that have a severity level of medium. Medium severity is determined by the organization that published the compliance items.
--
-- /Note:/ Consider using 'mediumCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssMediumCount :: Lens.Lens' SeveritySummary (Core.Maybe Core.Int)
ssMediumCount = Lens.field @"mediumCount"
{-# DEPRECATED ssMediumCount "Use generic-lens or generic-optics with 'mediumCount' instead." #-}

-- | The total number of resources or compliance items that have a severity level of unspecified. Unspecified severity is determined by the organization that published the compliance items.
--
-- /Note:/ Consider using 'unspecifiedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssUnspecifiedCount :: Lens.Lens' SeveritySummary (Core.Maybe Core.Int)
ssUnspecifiedCount = Lens.field @"unspecifiedCount"
{-# DEPRECATED ssUnspecifiedCount "Use generic-lens or generic-optics with 'unspecifiedCount' instead." #-}

instance Core.FromJSON SeveritySummary where
  parseJSON =
    Core.withObject "SeveritySummary" Core.$
      \x ->
        SeveritySummary'
          Core.<$> (x Core..:? "CriticalCount")
          Core.<*> (x Core..:? "HighCount")
          Core.<*> (x Core..:? "InformationalCount")
          Core.<*> (x Core..:? "LowCount")
          Core.<*> (x Core..:? "MediumCount")
          Core.<*> (x Core..:? "UnspecifiedCount")
