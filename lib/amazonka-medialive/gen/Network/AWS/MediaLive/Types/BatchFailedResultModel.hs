{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BatchFailedResultModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BatchFailedResultModel
  ( BatchFailedResultModel (..),

    -- * Smart constructor
    mkBatchFailedResultModel,

    -- * Lenses
    bfrmArn,
    bfrmCode,
    bfrmId,
    bfrmMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details from a failed operation
--
-- /See:/ 'mkBatchFailedResultModel' smart constructor.
data BatchFailedResultModel = BatchFailedResultModel'
  { -- | ARN of the resource
    arn :: Core.Maybe Core.Text,
    -- | Error code for the failed operation
    code :: Core.Maybe Core.Text,
    -- | ID of the resource
    id :: Core.Maybe Core.Text,
    -- | Error message for the failed operation
    message :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchFailedResultModel' value with any optional fields omitted.
mkBatchFailedResultModel ::
  BatchFailedResultModel
mkBatchFailedResultModel =
  BatchFailedResultModel'
    { arn = Core.Nothing,
      code = Core.Nothing,
      id = Core.Nothing,
      message = Core.Nothing
    }

-- | ARN of the resource
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfrmArn :: Lens.Lens' BatchFailedResultModel (Core.Maybe Core.Text)
bfrmArn = Lens.field @"arn"
{-# DEPRECATED bfrmArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | Error code for the failed operation
--
-- /Note:/ Consider using 'code' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfrmCode :: Lens.Lens' BatchFailedResultModel (Core.Maybe Core.Text)
bfrmCode = Lens.field @"code"
{-# DEPRECATED bfrmCode "Use generic-lens or generic-optics with 'code' instead." #-}

-- | ID of the resource
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfrmId :: Lens.Lens' BatchFailedResultModel (Core.Maybe Core.Text)
bfrmId = Lens.field @"id"
{-# DEPRECATED bfrmId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Error message for the failed operation
--
-- /Note:/ Consider using 'message' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bfrmMessage :: Lens.Lens' BatchFailedResultModel (Core.Maybe Core.Text)
bfrmMessage = Lens.field @"message"
{-# DEPRECATED bfrmMessage "Use generic-lens or generic-optics with 'message' instead." #-}

instance Core.FromJSON BatchFailedResultModel where
  parseJSON =
    Core.withObject "BatchFailedResultModel" Core.$
      \x ->
        BatchFailedResultModel'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "code")
          Core.<*> (x Core..:? "id")
          Core.<*> (x Core..:? "message")
