{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.OutputSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.OutputSource
  ( OutputSource (..),

    -- * Smart constructor
    mkOutputSource,

    -- * Lenses
    osOutputSourceId,
    osOutputSourceType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SSM.Types.OutputSourceId as Types
import qualified Network.AWS.SSM.Types.OutputSourceType as Types

-- | Information about the source where the association execution details are stored.
--
-- /See:/ 'mkOutputSource' smart constructor.
data OutputSource = OutputSource'
  { -- | The ID of the output source, for example the URL of an S3 bucket.
    outputSourceId :: Core.Maybe Types.OutputSourceId,
    -- | The type of source where the association execution details are stored, for example, Amazon S3.
    outputSourceType :: Core.Maybe Types.OutputSourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'OutputSource' value with any optional fields omitted.
mkOutputSource ::
  OutputSource
mkOutputSource =
  OutputSource'
    { outputSourceId = Core.Nothing,
      outputSourceType = Core.Nothing
    }

-- | The ID of the output source, for example the URL of an S3 bucket.
--
-- /Note:/ Consider using 'outputSourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osOutputSourceId :: Lens.Lens' OutputSource (Core.Maybe Types.OutputSourceId)
osOutputSourceId = Lens.field @"outputSourceId"
{-# DEPRECATED osOutputSourceId "Use generic-lens or generic-optics with 'outputSourceId' instead." #-}

-- | The type of source where the association execution details are stored, for example, Amazon S3.
--
-- /Note:/ Consider using 'outputSourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
osOutputSourceType :: Lens.Lens' OutputSource (Core.Maybe Types.OutputSourceType)
osOutputSourceType = Lens.field @"outputSourceType"
{-# DEPRECATED osOutputSourceType "Use generic-lens or generic-optics with 'outputSourceType' instead." #-}

instance Core.FromJSON OutputSource where
  parseJSON =
    Core.withObject "OutputSource" Core.$
      \x ->
        OutputSource'
          Core.<$> (x Core..:? "OutputSourceId")
          Core.<*> (x Core..:? "OutputSourceType")
