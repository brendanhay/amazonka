{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.PromptSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.PromptSummary
  ( PromptSummary (..),

    -- * Smart constructor
    mkPromptSummary,

    -- * Lenses
    psArn,
    psId,
    psName,
  )
where

import qualified Network.AWS.Connect.Types.ARN as Types
import qualified Network.AWS.Connect.Types.Id as Types
import qualified Network.AWS.Connect.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the prompt.
--
-- /See:/ 'mkPromptSummary' smart constructor.
data PromptSummary = PromptSummary'
  { -- | The Amazon Resource Name (ARN) of the prompt.
    arn :: Core.Maybe Types.ARN,
    -- | The identifier of the prompt.
    id :: Core.Maybe Types.Id,
    -- | The name of the prompt.
    name :: Core.Maybe Types.Name
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PromptSummary' value with any optional fields omitted.
mkPromptSummary ::
  PromptSummary
mkPromptSummary =
  PromptSummary'
    { arn = Core.Nothing,
      id = Core.Nothing,
      name = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the prompt.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psArn :: Lens.Lens' PromptSummary (Core.Maybe Types.ARN)
psArn = Lens.field @"arn"
{-# DEPRECATED psArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The identifier of the prompt.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psId :: Lens.Lens' PromptSummary (Core.Maybe Types.Id)
psId = Lens.field @"id"
{-# DEPRECATED psId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The name of the prompt.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
psName :: Lens.Lens' PromptSummary (Core.Maybe Types.Name)
psName = Lens.field @"name"
{-# DEPRECATED psName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Core.FromJSON PromptSummary where
  parseJSON =
    Core.withObject "PromptSummary" Core.$
      \x ->
        PromptSummary'
          Core.<$> (x Core..:? "Arn")
          Core.<*> (x Core..:? "Id")
          Core.<*> (x Core..:? "Name")
