{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Tiering
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.S3.Types.Tiering
  ( Tiering (..)
  -- * Smart constructor
  , mkTiering
  -- * Lenses
  , tDays
  , tAccessTier
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.IntelligentTieringAccessTier as Types

-- | The S3 Intelligent-Tiering storage class is designed to optimize storage costs by automatically moving data to the most cost-effective storage access tier, without additional operational overhead.
--
-- /See:/ 'mkTiering' smart constructor.
data Tiering = Tiering'
  { days :: Core.Int
    -- ^ The number of consecutive days of no access after which an object will be eligible to be transitioned to the corresponding tier. The minimum number of days specified for Archive Access tier must be at least 90 days and Deep Archive Access tier must be at least 180 days. The maximum can be up to 2 years (730 days).
  , accessTier :: Types.IntelligentTieringAccessTier
    -- ^ S3 Intelligent-Tiering access tier. See <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> for a list of access tiers in the S3 Intelligent-Tiering storage class.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Tiering' value with any optional fields omitted.
mkTiering
    :: Core.Int -- ^ 'days'
    -> Types.IntelligentTieringAccessTier -- ^ 'accessTier'
    -> Tiering
mkTiering days accessTier = Tiering'{days, accessTier}

-- | The number of consecutive days of no access after which an object will be eligible to be transitioned to the corresponding tier. The minimum number of days specified for Archive Access tier must be at least 90 days and Deep Archive Access tier must be at least 180 days. The maximum can be up to 2 years (730 days).
--
-- /Note:/ Consider using 'days' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDays :: Lens.Lens' Tiering Core.Int
tDays = Lens.field @"days"
{-# INLINEABLE tDays #-}
{-# DEPRECATED days "Use generic-lens or generic-optics with 'days' instead"  #-}

-- | S3 Intelligent-Tiering access tier. See <https://docs.aws.amazon.com/AmazonS3/latest/dev/storage-class-intro.html#sc-dynamic-data-access Storage class for automatically optimizing frequently and infrequently accessed objects> for a list of access tiers in the S3 Intelligent-Tiering storage class.
--
-- /Note:/ Consider using 'accessTier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tAccessTier :: Lens.Lens' Tiering Types.IntelligentTieringAccessTier
tAccessTier = Lens.field @"accessTier"
{-# INLINEABLE tAccessTier #-}
{-# DEPRECATED accessTier "Use generic-lens or generic-optics with 'accessTier' instead"  #-}

instance Core.ToXML Tiering where
        toXML Tiering{..}
          = Core.toXMLElement "Days" days Core.<>
              Core.toXMLElement "AccessTier" accessTier

instance Core.FromXML Tiering where
        parseXML x
          = Tiering' Core.<$>
              (x Core..@ "Days") Core.<*> x Core..@ "AccessTier"
