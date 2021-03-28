{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.Types.DestinationProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GuardDuty.Types.DestinationProperties
  ( DestinationProperties (..)
  -- * Smart constructor
  , mkDestinationProperties
  -- * Lenses
  , dpDestinationArn
  , dpKmsKeyArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains the Amazon Resource Name (ARN) of the resource to publish to, such as an S3 bucket, and the ARN of the KMS key to use to encrypt published findings.
--
-- /See:/ 'mkDestinationProperties' smart constructor.
data DestinationProperties = DestinationProperties'
  { destinationArn :: Core.Maybe Core.Text
    -- ^ The ARN of the resource to publish to.
  , kmsKeyArn :: Core.Maybe Core.Text
    -- ^ The ARN of the KMS key to use for encryption.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DestinationProperties' value with any optional fields omitted.
mkDestinationProperties
    :: DestinationProperties
mkDestinationProperties
  = DestinationProperties'{destinationArn = Core.Nothing,
                           kmsKeyArn = Core.Nothing}

-- | The ARN of the resource to publish to.
--
-- /Note:/ Consider using 'destinationArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpDestinationArn :: Lens.Lens' DestinationProperties (Core.Maybe Core.Text)
dpDestinationArn = Lens.field @"destinationArn"
{-# INLINEABLE dpDestinationArn #-}
{-# DEPRECATED destinationArn "Use generic-lens or generic-optics with 'destinationArn' instead"  #-}

-- | The ARN of the KMS key to use for encryption.
--
-- /Note:/ Consider using 'kmsKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpKmsKeyArn :: Lens.Lens' DestinationProperties (Core.Maybe Core.Text)
dpKmsKeyArn = Lens.field @"kmsKeyArn"
{-# INLINEABLE dpKmsKeyArn #-}
{-# DEPRECATED kmsKeyArn "Use generic-lens or generic-optics with 'kmsKeyArn' instead"  #-}

instance Core.FromJSON DestinationProperties where
        toJSON DestinationProperties{..}
          = Core.object
              (Core.catMaybes
                 [("destinationArn" Core..=) Core.<$> destinationArn,
                  ("kmsKeyArn" Core..=) Core.<$> kmsKeyArn])

instance Core.FromJSON DestinationProperties where
        parseJSON
          = Core.withObject "DestinationProperties" Core.$
              \ x ->
                DestinationProperties' Core.<$>
                  (x Core..:? "destinationArn") Core.<*> x Core..:? "kmsKeyArn"
