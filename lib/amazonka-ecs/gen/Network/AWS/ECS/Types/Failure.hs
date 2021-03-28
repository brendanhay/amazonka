{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Failure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Types.Failure
  ( Failure (..)
  -- * Smart constructor
  , mkFailure
  -- * Lenses
  , fArn
  , fDetail
  , fReason
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A failed resource. For a list of common causes, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/api_failures_messages.html API failure reasons> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkFailure' smart constructor.
data Failure = Failure'
  { arn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the failed resource.
  , detail :: Core.Maybe Core.Text
    -- ^ The details of the failure.
  , reason :: Core.Maybe Core.Text
    -- ^ The reason for the failure.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Failure' value with any optional fields omitted.
mkFailure
    :: Failure
mkFailure
  = Failure'{arn = Core.Nothing, detail = Core.Nothing,
             reason = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the failed resource.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fArn :: Lens.Lens' Failure (Core.Maybe Core.Text)
fArn = Lens.field @"arn"
{-# INLINEABLE fArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

-- | The details of the failure.
--
-- /Note:/ Consider using 'detail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fDetail :: Lens.Lens' Failure (Core.Maybe Core.Text)
fDetail = Lens.field @"detail"
{-# INLINEABLE fDetail #-}
{-# DEPRECATED detail "Use generic-lens or generic-optics with 'detail' instead"  #-}

-- | The reason for the failure.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fReason :: Lens.Lens' Failure (Core.Maybe Core.Text)
fReason = Lens.field @"reason"
{-# INLINEABLE fReason #-}
{-# DEPRECATED reason "Use generic-lens or generic-optics with 'reason' instead"  #-}

instance Core.FromJSON Failure where
        parseJSON
          = Core.withObject "Failure" Core.$
              \ x ->
                Failure' Core.<$>
                  (x Core..:? "arn") Core.<*> x Core..:? "detail" Core.<*>
                    x Core..:? "reason"
