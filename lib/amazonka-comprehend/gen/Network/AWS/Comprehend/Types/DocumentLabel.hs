{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.DocumentLabel
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.DocumentLabel
  ( DocumentLabel (..),

    -- * Smart constructor
    mkDocumentLabel,

    -- * Lenses
    dName,
    dScore,
  )
where

import qualified Network.AWS.Comprehend.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies one of the label or labels that categorize the document being analyzed.
--
-- /See:/ 'mkDocumentLabel' smart constructor.
data DocumentLabel = DocumentLabel'
  { -- | The name of the label.
    name :: Core.Maybe Types.String,
    -- | The confidence score that Amazon Comprehend has this label correctly attributed.
    score :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DocumentLabel' value with any optional fields omitted.
mkDocumentLabel ::
  DocumentLabel
mkDocumentLabel =
  DocumentLabel' {name = Core.Nothing, score = Core.Nothing}

-- | The name of the label.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' DocumentLabel (Core.Maybe Types.String)
dName = Lens.field @"name"
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The confidence score that Amazon Comprehend has this label correctly attributed.
--
-- /Note:/ Consider using 'score' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dScore :: Lens.Lens' DocumentLabel (Core.Maybe Core.Double)
dScore = Lens.field @"score"
{-# DEPRECATED dScore "Use generic-lens or generic-optics with 'score' instead." #-}

instance Core.FromJSON DocumentLabel where
  parseJSON =
    Core.withObject "DocumentLabel" Core.$
      \x ->
        DocumentLabel'
          Core.<$> (x Core..:? "Name") Core.<*> (x Core..:? "Score")
