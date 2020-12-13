{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Tiering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Tiering
  ( Tiering (..),

    -- * Smart constructor
    mkTiering,

    -- * Lenses
    tDays,
    tAccessTier,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.IntelligentTieringAccessTier

-- | The S3 Intelligent-Tiering storage class is designed to optimize storage costs by automatically moving data to the most cost-effective storage access tier, without additional operational overhead.
--
-- /See:/ 'mkTiering' smart constructor.
data Tiering = Tiering'
  { -- | The number of consecutive days of no access after which an object will be eligible to be transitioned to the corresponding tier. The minimum number of days specified for Archive Access tier must be at least 90 days and Deep Archive Access tier must be at least 180 days. The maximum can be up to 2 years (730 days).
    days :: Lude.Int,
    -- | S3 Intelligent-Tiering access tier. See <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> for a list of access tiers in the S3 Intelligent-Tiering storage class.
    accessTier :: IntelligentTieringAccessTier
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Tiering' with the minimum fields required to make a request.
--
-- * 'days' - The number of consecutive days of no access after which an object will be eligible to be transitioned to the corresponding tier. The minimum number of days specified for Archive Access tier must be at least 90 days and Deep Archive Access tier must be at least 180 days. The maximum can be up to 2 years (730 days).
-- * 'accessTier' - S3 Intelligent-Tiering access tier. See <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> for a list of access tiers in the S3 Intelligent-Tiering storage class.
mkTiering ::
  -- | 'days'
  Lude.Int ->
  -- | 'accessTier'
  IntelligentTieringAccessTier ->
  Tiering
mkTiering pDays_ pAccessTier_ =
  Tiering' {days = pDays_, accessTier = pAccessTier_}

-- | The number of consecutive days of no access after which an object will be eligible to be transitioned to the corresponding tier. The minimum number of days specified for Archive Access tier must be at least 90 days and Deep Archive Access tier must be at least 180 days. The maximum can be up to 2 years (730 days).
--
-- /Note:/ Consider using 'days' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDays :: Lens.Lens' Tiering Lude.Int
tDays = Lens.lens (days :: Tiering -> Lude.Int) (\s a -> s {days = a} :: Tiering)
{-# DEPRECATED tDays "Use generic-lens or generic-optics with 'days' instead." #-}

-- | S3 Intelligent-Tiering access tier. See <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> for a list of access tiers in the S3 Intelligent-Tiering storage class.
--
-- /Note:/ Consider using 'accessTier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tAccessTier :: Lens.Lens' Tiering IntelligentTieringAccessTier
tAccessTier = Lens.lens (accessTier :: Tiering -> IntelligentTieringAccessTier) (\s a -> s {accessTier = a} :: Tiering)
{-# DEPRECATED tAccessTier "Use generic-lens or generic-optics with 'accessTier' instead." #-}

instance Lude.FromXML Tiering where
  parseXML x =
    Tiering'
      Lude.<$> (x Lude..@ "Days") Lude.<*> (x Lude..@ "AccessTier")

instance Lude.ToXML Tiering where
  toXML Tiering' {..} =
    Lude.mconcat
      ["Days" Lude.@= days, "AccessTier" Lude.@= accessTier]
