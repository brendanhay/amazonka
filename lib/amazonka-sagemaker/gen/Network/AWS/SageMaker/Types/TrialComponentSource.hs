{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.TrialComponentSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.TrialComponentSource
  ( TrialComponentSource (..),

    -- * Smart constructor
    mkTrialComponentSource,

    -- * Lenses
    tcsSourceArn,
    tcsSourceType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.SourceType as Types
import qualified Network.AWS.SageMaker.Types.TrialComponentSourceArn as Types

-- | The Amazon Resource Name (ARN) and job type of the source of a trial component.
--
-- /See:/ 'mkTrialComponentSource' smart constructor.
data TrialComponentSource = TrialComponentSource'
  { -- | The source ARN.
    sourceArn :: Types.TrialComponentSourceArn,
    -- | The source job type.
    sourceType :: Core.Maybe Types.SourceType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'TrialComponentSource' value with any optional fields omitted.
mkTrialComponentSource ::
  -- | 'sourceArn'
  Types.TrialComponentSourceArn ->
  TrialComponentSource
mkTrialComponentSource sourceArn =
  TrialComponentSource' {sourceArn, sourceType = Core.Nothing}

-- | The source ARN.
--
-- /Note:/ Consider using 'sourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsSourceArn :: Lens.Lens' TrialComponentSource Types.TrialComponentSourceArn
tcsSourceArn = Lens.field @"sourceArn"
{-# DEPRECATED tcsSourceArn "Use generic-lens or generic-optics with 'sourceArn' instead." #-}

-- | The source job type.
--
-- /Note:/ Consider using 'sourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcsSourceType :: Lens.Lens' TrialComponentSource (Core.Maybe Types.SourceType)
tcsSourceType = Lens.field @"sourceType"
{-# DEPRECATED tcsSourceType "Use generic-lens or generic-optics with 'sourceType' instead." #-}

instance Core.FromJSON TrialComponentSource where
  parseJSON =
    Core.withObject "TrialComponentSource" Core.$
      \x ->
        TrialComponentSource'
          Core.<$> (x Core..: "SourceArn") Core.<*> (x Core..:? "SourceType")
