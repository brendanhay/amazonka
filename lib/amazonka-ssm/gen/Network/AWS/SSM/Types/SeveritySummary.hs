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
    ssLowCount,
    ssUnspecifiedCount,
    ssHighCount,
    ssMediumCount,
    ssInformationalCount,
    ssCriticalCount,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The number of managed instances found for each patch severity level defined in the request filter.
--
-- /See:/ 'mkSeveritySummary' smart constructor.
data SeveritySummary = SeveritySummary'
  { lowCount ::
      Lude.Maybe Lude.Int,
    unspecifiedCount :: Lude.Maybe Lude.Int,
    highCount :: Lude.Maybe Lude.Int,
    mediumCount :: Lude.Maybe Lude.Int,
    informationalCount :: Lude.Maybe Lude.Int,
    criticalCount :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SeveritySummary' with the minimum fields required to make a request.
--
-- * 'criticalCount' - The total number of resources or compliance items that have a severity level of critical. Critical severity is determined by the organization that published the compliance items.
-- * 'highCount' - The total number of resources or compliance items that have a severity level of high. High severity is determined by the organization that published the compliance items.
-- * 'informationalCount' - The total number of resources or compliance items that have a severity level of informational. Informational severity is determined by the organization that published the compliance items.
-- * 'lowCount' - The total number of resources or compliance items that have a severity level of low. Low severity is determined by the organization that published the compliance items.
-- * 'mediumCount' - The total number of resources or compliance items that have a severity level of medium. Medium severity is determined by the organization that published the compliance items.
-- * 'unspecifiedCount' - The total number of resources or compliance items that have a severity level of unspecified. Unspecified severity is determined by the organization that published the compliance items.
mkSeveritySummary ::
  SeveritySummary
mkSeveritySummary =
  SeveritySummary'
    { lowCount = Lude.Nothing,
      unspecifiedCount = Lude.Nothing,
      highCount = Lude.Nothing,
      mediumCount = Lude.Nothing,
      informationalCount = Lude.Nothing,
      criticalCount = Lude.Nothing
    }

-- | The total number of resources or compliance items that have a severity level of low. Low severity is determined by the organization that published the compliance items.
--
-- /Note:/ Consider using 'lowCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssLowCount :: Lens.Lens' SeveritySummary (Lude.Maybe Lude.Int)
ssLowCount = Lens.lens (lowCount :: SeveritySummary -> Lude.Maybe Lude.Int) (\s a -> s {lowCount = a} :: SeveritySummary)
{-# DEPRECATED ssLowCount "Use generic-lens or generic-optics with 'lowCount' instead." #-}

-- | The total number of resources or compliance items that have a severity level of unspecified. Unspecified severity is determined by the organization that published the compliance items.
--
-- /Note:/ Consider using 'unspecifiedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssUnspecifiedCount :: Lens.Lens' SeveritySummary (Lude.Maybe Lude.Int)
ssUnspecifiedCount = Lens.lens (unspecifiedCount :: SeveritySummary -> Lude.Maybe Lude.Int) (\s a -> s {unspecifiedCount = a} :: SeveritySummary)
{-# DEPRECATED ssUnspecifiedCount "Use generic-lens or generic-optics with 'unspecifiedCount' instead." #-}

-- | The total number of resources or compliance items that have a severity level of high. High severity is determined by the organization that published the compliance items.
--
-- /Note:/ Consider using 'highCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssHighCount :: Lens.Lens' SeveritySummary (Lude.Maybe Lude.Int)
ssHighCount = Lens.lens (highCount :: SeveritySummary -> Lude.Maybe Lude.Int) (\s a -> s {highCount = a} :: SeveritySummary)
{-# DEPRECATED ssHighCount "Use generic-lens or generic-optics with 'highCount' instead." #-}

-- | The total number of resources or compliance items that have a severity level of medium. Medium severity is determined by the organization that published the compliance items.
--
-- /Note:/ Consider using 'mediumCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssMediumCount :: Lens.Lens' SeveritySummary (Lude.Maybe Lude.Int)
ssMediumCount = Lens.lens (mediumCount :: SeveritySummary -> Lude.Maybe Lude.Int) (\s a -> s {mediumCount = a} :: SeveritySummary)
{-# DEPRECATED ssMediumCount "Use generic-lens or generic-optics with 'mediumCount' instead." #-}

-- | The total number of resources or compliance items that have a severity level of informational. Informational severity is determined by the organization that published the compliance items.
--
-- /Note:/ Consider using 'informationalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssInformationalCount :: Lens.Lens' SeveritySummary (Lude.Maybe Lude.Int)
ssInformationalCount = Lens.lens (informationalCount :: SeveritySummary -> Lude.Maybe Lude.Int) (\s a -> s {informationalCount = a} :: SeveritySummary)
{-# DEPRECATED ssInformationalCount "Use generic-lens or generic-optics with 'informationalCount' instead." #-}

-- | The total number of resources or compliance items that have a severity level of critical. Critical severity is determined by the organization that published the compliance items.
--
-- /Note:/ Consider using 'criticalCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssCriticalCount :: Lens.Lens' SeveritySummary (Lude.Maybe Lude.Int)
ssCriticalCount = Lens.lens (criticalCount :: SeveritySummary -> Lude.Maybe Lude.Int) (\s a -> s {criticalCount = a} :: SeveritySummary)
{-# DEPRECATED ssCriticalCount "Use generic-lens or generic-optics with 'criticalCount' instead." #-}

instance Lude.FromJSON SeveritySummary where
  parseJSON =
    Lude.withObject
      "SeveritySummary"
      ( \x ->
          SeveritySummary'
            Lude.<$> (x Lude..:? "LowCount")
            Lude.<*> (x Lude..:? "UnspecifiedCount")
            Lude.<*> (x Lude..:? "HighCount")
            Lude.<*> (x Lude..:? "MediumCount")
            Lude.<*> (x Lude..:? "InformationalCount")
            Lude.<*> (x Lude..:? "CriticalCount")
      )
