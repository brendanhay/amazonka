{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaConvert.Types.OutputGroupDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.MediaConvert.Types.OutputGroupDetail
  ( OutputGroupDetail (..)
  -- * Smart constructor
  , mkOutputGroupDetail
  -- * Lenses
  , ogdOutputDetails
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaConvert.Types.OutputDetail as Types
import qualified Network.AWS.Prelude as Core

-- | Contains details about the output groups specified in the job settings.
--
-- /See:/ 'mkOutputGroupDetail' smart constructor.
newtype OutputGroupDetail = OutputGroupDetail'
  { outputDetails :: Core.Maybe [Types.OutputDetail]
    -- ^ Details about the output
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'OutputGroupDetail' value with any optional fields omitted.
mkOutputGroupDetail
    :: OutputGroupDetail
mkOutputGroupDetail
  = OutputGroupDetail'{outputDetails = Core.Nothing}

-- | Details about the output
--
-- /Note:/ Consider using 'outputDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ogdOutputDetails :: Lens.Lens' OutputGroupDetail (Core.Maybe [Types.OutputDetail])
ogdOutputDetails = Lens.field @"outputDetails"
{-# INLINEABLE ogdOutputDetails #-}
{-# DEPRECATED outputDetails "Use generic-lens or generic-optics with 'outputDetails' instead"  #-}

instance Core.FromJSON OutputGroupDetail where
        parseJSON
          = Core.withObject "OutputGroupDetail" Core.$
              \ x -> OutputGroupDetail' Core.<$> (x Core..:? "outputDetails")
