{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.PipelineConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.PipelineConfig
  ( PipelineConfig (..)
  -- * Smart constructor
  , mkPipelineConfig
  -- * Lenses
  , pcFunctions
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The pipeline configuration for a resolver of kind @PIPELINE@ .
--
-- /See:/ 'mkPipelineConfig' smart constructor.
newtype PipelineConfig = PipelineConfig'
  { functions :: Core.Maybe [Core.Text]
    -- ^ A list of @Function@ objects.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'PipelineConfig' value with any optional fields omitted.
mkPipelineConfig
    :: PipelineConfig
mkPipelineConfig = PipelineConfig'{functions = Core.Nothing}

-- | A list of @Function@ objects.
--
-- /Note:/ Consider using 'functions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcFunctions :: Lens.Lens' PipelineConfig (Core.Maybe [Core.Text])
pcFunctions = Lens.field @"functions"
{-# INLINEABLE pcFunctions #-}
{-# DEPRECATED functions "Use generic-lens or generic-optics with 'functions' instead"  #-}

instance Core.FromJSON PipelineConfig where
        toJSON PipelineConfig{..}
          = Core.object
              (Core.catMaybes [("functions" Core..=) Core.<$> functions])

instance Core.FromJSON PipelineConfig where
        parseJSON
          = Core.withObject "PipelineConfig" Core.$
              \ x -> PipelineConfig' Core.<$> (x Core..:? "functions")
