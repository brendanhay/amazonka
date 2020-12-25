{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Failure
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Failure
  ( Failure (..),

    -- * Smart constructor
    mkFailure,

    -- * Lenses
    fArn,
    fDetail,
    fReason,
  )
where

import qualified Network.AWS.ECS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A failed resource. For a list of common causes, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/api_failures_messages.html API failure reasons> in the /Amazon Elastic Container Service Developer Guide/ .
--
-- /See:/ 'mkFailure' smart constructor.
data Failure = Failure'
  { -- | The Amazon Resource Name (ARN) of the failed resource.
    arn :: Core.Maybe Types.String,
    -- | The details of the failure.
    detail :: Core.Maybe Types.String,
    -- | The reason for the failure.
    reason :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Failure' value with any optional fields omitted.
mkFailure ::
  Failure
mkFailure =
  Failure'
    { arn = Core.Nothing,
      detail = Core.Nothing,
      reason = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the failed resource.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fArn :: Lens.Lens' Failure (Core.Maybe Types.String)
fArn = Lens.field @"arn"
{-# DEPRECATED fArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The details of the failure.
--
-- /Note:/ Consider using 'detail' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fDetail :: Lens.Lens' Failure (Core.Maybe Types.String)
fDetail = Lens.field @"detail"
{-# DEPRECATED fDetail "Use generic-lens or generic-optics with 'detail' instead." #-}

-- | The reason for the failure.
--
-- /Note:/ Consider using 'reason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fReason :: Lens.Lens' Failure (Core.Maybe Types.String)
fReason = Lens.field @"reason"
{-# DEPRECATED fReason "Use generic-lens or generic-optics with 'reason' instead." #-}

instance Core.FromJSON Failure where
  parseJSON =
    Core.withObject "Failure" Core.$
      \x ->
        Failure'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "detail")
          Core.<*> (x Core..:? "reason")
