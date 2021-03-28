{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.CaptureOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.CaptureOption
  ( CaptureOption (..)
  -- * Smart constructor
  , mkCaptureOption
  -- * Lenses
  , coCaptureMode
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.CaptureMode as Types

-- | 
--
-- /See:/ 'mkCaptureOption' smart constructor.
newtype CaptureOption = CaptureOption'
  { captureMode :: Types.CaptureMode
    -- ^ 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CaptureOption' value with any optional fields omitted.
mkCaptureOption
    :: Types.CaptureMode -- ^ 'captureMode'
    -> CaptureOption
mkCaptureOption captureMode = CaptureOption'{captureMode}

-- | 
--
-- /Note:/ Consider using 'captureMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
coCaptureMode :: Lens.Lens' CaptureOption Types.CaptureMode
coCaptureMode = Lens.field @"captureMode"
{-# INLINEABLE coCaptureMode #-}
{-# DEPRECATED captureMode "Use generic-lens or generic-optics with 'captureMode' instead"  #-}

instance Core.FromJSON CaptureOption where
        toJSON CaptureOption{..}
          = Core.object
              (Core.catMaybes [Core.Just ("CaptureMode" Core..= captureMode)])

instance Core.FromJSON CaptureOption where
        parseJSON
          = Core.withObject "CaptureOption" Core.$
              \ x -> CaptureOption' Core.<$> (x Core..: "CaptureMode")
