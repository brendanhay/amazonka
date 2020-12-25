{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.Types.BatchSuccessfulResultModel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaLive.Types.BatchSuccessfulResultModel
  ( BatchSuccessfulResultModel (..),

    -- * Smart constructor
    mkBatchSuccessfulResultModel,

    -- * Lenses
    bsrmArn,
    bsrmId,
    bsrmState,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Details from a successful operation
--
-- /See:/ 'mkBatchSuccessfulResultModel' smart constructor.
data BatchSuccessfulResultModel = BatchSuccessfulResultModel'
  { -- | ARN of the resource
    arn :: Core.Maybe Core.Text,
    -- | ID of the resource
    id :: Core.Maybe Core.Text,
    -- | Current state of the resource
    state :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchSuccessfulResultModel' value with any optional fields omitted.
mkBatchSuccessfulResultModel ::
  BatchSuccessfulResultModel
mkBatchSuccessfulResultModel =
  BatchSuccessfulResultModel'
    { arn = Core.Nothing,
      id = Core.Nothing,
      state = Core.Nothing
    }

-- | ARN of the resource
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrmArn :: Lens.Lens' BatchSuccessfulResultModel (Core.Maybe Core.Text)
bsrmArn = Lens.field @"arn"
{-# DEPRECATED bsrmArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | ID of the resource
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrmId :: Lens.Lens' BatchSuccessfulResultModel (Core.Maybe Core.Text)
bsrmId = Lens.field @"id"
{-# DEPRECATED bsrmId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | Current state of the resource
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bsrmState :: Lens.Lens' BatchSuccessfulResultModel (Core.Maybe Core.Text)
bsrmState = Lens.field @"state"
{-# DEPRECATED bsrmState "Use generic-lens or generic-optics with 'state' instead." #-}

instance Core.FromJSON BatchSuccessfulResultModel where
  parseJSON =
    Core.withObject "BatchSuccessfulResultModel" Core.$
      \x ->
        BatchSuccessfulResultModel'
          Core.<$> (x Core..:? "arn")
          Core.<*> (x Core..:? "id")
          Core.<*> (x Core..:? "state")
