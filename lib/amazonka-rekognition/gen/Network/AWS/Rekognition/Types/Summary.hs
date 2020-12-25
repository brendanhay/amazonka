{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Rekognition.Types.Summary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Rekognition.Types.Summary
  ( Summary (..),

    -- * Smart constructor
    mkSummary,

    -- * Lenses
    sS3Object,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Rekognition.Types.S3Object as Types

-- | The S3 bucket that contains the training summary. The training summary includes aggregated evaluation metrics for the entire testing dataset and metrics for each individual label.
--
-- You get the training summary S3 bucket location by calling 'DescribeProjectVersions' .
--
-- /See:/ 'mkSummary' smart constructor.
newtype Summary = Summary'
  { s3Object :: Core.Maybe Types.S3Object
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'Summary' value with any optional fields omitted.
mkSummary ::
  Summary
mkSummary = Summary' {s3Object = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 's3Object' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sS3Object :: Lens.Lens' Summary (Core.Maybe Types.S3Object)
sS3Object = Lens.field @"s3Object"
{-# DEPRECATED sS3Object "Use generic-lens or generic-optics with 's3Object' instead." #-}

instance Core.FromJSON Summary where
  parseJSON =
    Core.withObject "Summary" Core.$
      \x -> Summary' Core.<$> (x Core..:? "S3Object")
