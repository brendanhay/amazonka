{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UiTemplate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.UiTemplate
  ( UiTemplate (..)
  -- * Smart constructor
  , mkUiTemplate
  -- * Lenses
  , utContent
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.Content as Types

-- | The Liquid template for the worker user interface.
--
-- /See:/ 'mkUiTemplate' smart constructor.
newtype UiTemplate = UiTemplate'
  { content :: Types.Content
    -- ^ The content of the Liquid template for the worker user interface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UiTemplate' value with any optional fields omitted.
mkUiTemplate
    :: Types.Content -- ^ 'content'
    -> UiTemplate
mkUiTemplate content = UiTemplate'{content}

-- | The content of the Liquid template for the worker user interface.
--
-- /Note:/ Consider using 'content' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
utContent :: Lens.Lens' UiTemplate Types.Content
utContent = Lens.field @"content"
{-# INLINEABLE utContent #-}
{-# DEPRECATED content "Use generic-lens or generic-optics with 'content' instead"  #-}

instance Core.FromJSON UiTemplate where
        toJSON UiTemplate{..}
          = Core.object
              (Core.catMaybes [Core.Just ("Content" Core..= content)])
