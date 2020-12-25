{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.Types.ProgressUpdateStreamSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MigrationHub.Types.ProgressUpdateStreamSummary
  ( ProgressUpdateStreamSummary (..),

    -- * Smart constructor
    mkProgressUpdateStreamSummary,

    -- * Lenses
    pussProgressUpdateStreamName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MigrationHub.Types.ProgressUpdateStreamName as Types
import qualified Network.AWS.Prelude as Core

-- | Summary of the AWS resource used for access control that is implicitly linked to your AWS account.
--
-- /See:/ 'mkProgressUpdateStreamSummary' smart constructor.
newtype ProgressUpdateStreamSummary = ProgressUpdateStreamSummary'
  { -- | The name of the ProgressUpdateStream. /Do not store personal data in this field./
    progressUpdateStreamName :: Core.Maybe Types.ProgressUpdateStreamName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'ProgressUpdateStreamSummary' value with any optional fields omitted.
mkProgressUpdateStreamSummary ::
  ProgressUpdateStreamSummary
mkProgressUpdateStreamSummary =
  ProgressUpdateStreamSummary'
    { progressUpdateStreamName =
        Core.Nothing
    }

-- | The name of the ProgressUpdateStream. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'progressUpdateStreamName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pussProgressUpdateStreamName :: Lens.Lens' ProgressUpdateStreamSummary (Core.Maybe Types.ProgressUpdateStreamName)
pussProgressUpdateStreamName = Lens.field @"progressUpdateStreamName"
{-# DEPRECATED pussProgressUpdateStreamName "Use generic-lens or generic-optics with 'progressUpdateStreamName' instead." #-}

instance Core.FromJSON ProgressUpdateStreamSummary where
  parseJSON =
    Core.withObject "ProgressUpdateStreamSummary" Core.$
      \x ->
        ProgressUpdateStreamSummary'
          Core.<$> (x Core..:? "ProgressUpdateStreamName")
