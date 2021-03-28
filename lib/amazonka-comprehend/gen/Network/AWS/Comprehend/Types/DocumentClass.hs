{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentClass
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Comprehend.Types.DocumentClass
  ( DocumentClass (..)
  -- * Smart constructor
  , mkDocumentClass
  -- * Lenses
  , dcName
  , dcScore
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the class that categorizes the document being analyzed
--
-- /See:/ 'mkDocumentClass' smart constructor.
data DocumentClass = DocumentClass'
  { name :: Core.Maybe Core.Text
    -- ^ The name of the class.
  , score :: Core.Maybe Core.Double
    -- ^ The confidence score that Amazon Comprehend has this class correctly attributed.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DocumentClass' value with any optional fields omitted.
mkDocumentClass
    :: DocumentClass
mkDocumentClass
  = DocumentClass'{name = Core.Nothing, score = Core.Nothing}

-- | The name of the class.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcName :: Lens.Lens' DocumentClass (Core.Maybe Core.Text)
dcName = Lens.field @"name"
{-# INLINEABLE dcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The confidence score that Amazon Comprehend has this class correctly attributed.
--
-- /Note:/ Consider using 'score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcScore :: Lens.Lens' DocumentClass (Core.Maybe Core.Double)
dcScore = Lens.field @"score"
{-# INLINEABLE dcScore #-}
{-# DEPRECATED score "Use generic-lens or generic-optics with 'score' instead"  #-}

instance Core.FromJSON DocumentClass where
        parseJSON
          = Core.withObject "DocumentClass" Core.$
              \ x ->
                DocumentClass' Core.<$>
                  (x Core..:? "Name") Core.<*> x Core..:? "Score"
