{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.BlockPublicAccess
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GuardDuty.Types.BlockPublicAccess
  ( BlockPublicAccess (..),

    -- * Smart constructor
    mkBlockPublicAccess,

    -- * Lenses
    bpaBlockPublicAcls,
    bpaBlockPublicPolicy,
    bpaIgnorePublicAcls,
    bpaRestrictPublicBuckets,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information on how the bucker owner's S3 Block Public Access settings are being applied to the S3 bucket. See <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html S3 Block Public Access> for more information.
--
-- /See:/ 'mkBlockPublicAccess' smart constructor.
data BlockPublicAccess = BlockPublicAccess'
  { -- | Indicates if S3 Block Public Access is set to @BlockPublicAcls@ .
    blockPublicAcls :: Core.Maybe Core.Bool,
    -- | Indicates if S3 Block Public Access is set to @BlockPublicPolicy@ .
    blockPublicPolicy :: Core.Maybe Core.Bool,
    -- | Indicates if S3 Block Public Access is set to @IgnorePublicAcls@ .
    ignorePublicAcls :: Core.Maybe Core.Bool,
    -- | Indicates if S3 Block Public Access is set to @RestrictPublicBuckets@ .
    restrictPublicBuckets :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BlockPublicAccess' value with any optional fields omitted.
mkBlockPublicAccess ::
  BlockPublicAccess
mkBlockPublicAccess =
  BlockPublicAccess'
    { blockPublicAcls = Core.Nothing,
      blockPublicPolicy = Core.Nothing,
      ignorePublicAcls = Core.Nothing,
      restrictPublicBuckets = Core.Nothing
    }

-- | Indicates if S3 Block Public Access is set to @BlockPublicAcls@ .
--
-- /Note:/ Consider using 'blockPublicAcls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpaBlockPublicAcls :: Lens.Lens' BlockPublicAccess (Core.Maybe Core.Bool)
bpaBlockPublicAcls = Lens.field @"blockPublicAcls"
{-# DEPRECATED bpaBlockPublicAcls "Use generic-lens or generic-optics with 'blockPublicAcls' instead." #-}

-- | Indicates if S3 Block Public Access is set to @BlockPublicPolicy@ .
--
-- /Note:/ Consider using 'blockPublicPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpaBlockPublicPolicy :: Lens.Lens' BlockPublicAccess (Core.Maybe Core.Bool)
bpaBlockPublicPolicy = Lens.field @"blockPublicPolicy"
{-# DEPRECATED bpaBlockPublicPolicy "Use generic-lens or generic-optics with 'blockPublicPolicy' instead." #-}

-- | Indicates if S3 Block Public Access is set to @IgnorePublicAcls@ .
--
-- /Note:/ Consider using 'ignorePublicAcls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpaIgnorePublicAcls :: Lens.Lens' BlockPublicAccess (Core.Maybe Core.Bool)
bpaIgnorePublicAcls = Lens.field @"ignorePublicAcls"
{-# DEPRECATED bpaIgnorePublicAcls "Use generic-lens or generic-optics with 'ignorePublicAcls' instead." #-}

-- | Indicates if S3 Block Public Access is set to @RestrictPublicBuckets@ .
--
-- /Note:/ Consider using 'restrictPublicBuckets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bpaRestrictPublicBuckets :: Lens.Lens' BlockPublicAccess (Core.Maybe Core.Bool)
bpaRestrictPublicBuckets = Lens.field @"restrictPublicBuckets"
{-# DEPRECATED bpaRestrictPublicBuckets "Use generic-lens or generic-optics with 'restrictPublicBuckets' instead." #-}

instance Core.FromJSON BlockPublicAccess where
  parseJSON =
    Core.withObject "BlockPublicAccess" Core.$
      \x ->
        BlockPublicAccess'
          Core.<$> (x Core..:? "blockPublicAcls")
          Core.<*> (x Core..:? "blockPublicPolicy")
          Core.<*> (x Core..:? "ignorePublicAcls")
          Core.<*> (x Core..:? "restrictPublicBuckets")
