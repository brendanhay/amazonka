{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialSource
  ( TrialSource (..),

    -- * Smart constructor
    mkTrialSource,

    -- * Lenses
    tsSourceArn,
    tsSourceType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.SourceType as Types
import qualified Network.AWS.SageMaker.Types.TrialSourceArn as Types

-- | The source of the trial.
--
-- /See:/ 'mkTrialSource' smart constructor.
data TrialSource = TrialSource'
  { -- | The Amazon Resource Name (ARN) of the source.
    sourceArn :: Types.TrialSourceArn,
    -- | The source job type.
    sourceType :: Core.Maybe Types.SourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrialSource' value with any optional fields omitted.
mkTrialSource ::
  -- | 'sourceArn'
  Types.TrialSourceArn ->
  TrialSource
mkTrialSource sourceArn =
  TrialSource' {sourceArn, sourceType = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the source.
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsSourceArn :: Lens.Lens' TrialSource Types.TrialSourceArn
tsSourceArn = Lens.field @"sourceArn"
{-# DEPRECATED tsSourceArn "Use generic-lens or generic-optics with 'sourceArn' instead." #-}

-- | The source job type.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tsSourceType :: Lens.Lens' TrialSource (Core.Maybe Types.SourceType)
tsSourceType = Lens.field @"sourceType"
{-# DEPRECATED tsSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

instance Core.FromJSON TrialSource where
  parseJSON =
    Core.withObject "TrialSource" Core.$
      \x ->
        TrialSource'
          Core.<$> (x Core..: "SourceArn") Core.<*> (x Core..:? "SourceType")
