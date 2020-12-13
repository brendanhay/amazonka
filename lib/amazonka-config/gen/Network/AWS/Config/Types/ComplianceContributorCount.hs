{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.ComplianceContributorCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.ComplianceContributorCount
  ( ComplianceContributorCount (..),

    -- * Smart constructor
    mkComplianceContributorCount,

    -- * Lenses
    cccCappedCount,
    cccCapExceeded,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The number of AWS resources or AWS Config rules responsible for the current compliance of the item, up to a maximum number.
--
-- /See:/ 'mkComplianceContributorCount' smart constructor.
data ComplianceContributorCount = ComplianceContributorCount'
  { -- | The number of AWS resources or AWS Config rules responsible for the current compliance of the item.
    cappedCount :: Lude.Maybe Lude.Int,
    -- | Indicates whether the maximum count is reached.
    capExceeded :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ComplianceContributorCount' with the minimum fields required to make a request.
--
-- * 'cappedCount' - The number of AWS resources or AWS Config rules responsible for the current compliance of the item.
-- * 'capExceeded' - Indicates whether the maximum count is reached.
mkComplianceContributorCount ::
  ComplianceContributorCount
mkComplianceContributorCount =
  ComplianceContributorCount'
    { cappedCount = Lude.Nothing,
      capExceeded = Lude.Nothing
    }

-- | The number of AWS resources or AWS Config rules responsible for the current compliance of the item.
--
-- /Note:/ Consider using 'cappedCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccCappedCount :: Lens.Lens' ComplianceContributorCount (Lude.Maybe Lude.Int)
cccCappedCount = Lens.lens (cappedCount :: ComplianceContributorCount -> Lude.Maybe Lude.Int) (\s a -> s {cappedCount = a} :: ComplianceContributorCount)
{-# DEPRECATED cccCappedCount "Use generic-lens or generic-optics with 'cappedCount' instead." #-}

-- | Indicates whether the maximum count is reached.
--
-- /Note:/ Consider using 'capExceeded' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cccCapExceeded :: Lens.Lens' ComplianceContributorCount (Lude.Maybe Lude.Bool)
cccCapExceeded = Lens.lens (capExceeded :: ComplianceContributorCount -> Lude.Maybe Lude.Bool) (\s a -> s {capExceeded = a} :: ComplianceContributorCount)
{-# DEPRECATED cccCapExceeded "Use generic-lens or generic-optics with 'capExceeded' instead." #-}

instance Lude.FromJSON ComplianceContributorCount where
  parseJSON =
    Lude.withObject
      "ComplianceContributorCount"
      ( \x ->
          ComplianceContributorCount'
            Lude.<$> (x Lude..:? "CappedCount") Lude.<*> (x Lude..:? "CapExceeded")
      )
