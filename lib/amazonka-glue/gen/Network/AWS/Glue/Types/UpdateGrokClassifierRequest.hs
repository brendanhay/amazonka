{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UpdateGrokClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.UpdateGrokClassifierRequest
  ( UpdateGrokClassifierRequest (..),

    -- * Smart constructor
    mkUpdateGrokClassifierRequest,

    -- * Lenses
    ugcrName,
    ugcrClassification,
    ugcrCustomPatterns,
    ugcrGrokPattern,
  )
where

import qualified Network.AWS.Glue.Types.Classification as Types
import qualified Network.AWS.Glue.Types.CustomPatterns as Types
import qualified Network.AWS.Glue.Types.GrokPattern as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a grok classifier to update when passed to @UpdateClassifier@ .
--
-- /See:/ 'mkUpdateGrokClassifierRequest' smart constructor.
data UpdateGrokClassifierRequest = UpdateGrokClassifierRequest'
  { -- | The name of the @GrokClassifier@ .
    name :: Types.NameString,
    -- | An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
    classification :: Core.Maybe Types.Classification,
    -- | Optional custom grok patterns used by this classifier.
    customPatterns :: Core.Maybe Types.CustomPatterns,
    -- | The grok pattern used by this classifier.
    grokPattern :: Core.Maybe Types.GrokPattern
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateGrokClassifierRequest' value with any optional fields omitted.
mkUpdateGrokClassifierRequest ::
  -- | 'name'
  Types.NameString ->
  UpdateGrokClassifierRequest
mkUpdateGrokClassifierRequest name =
  UpdateGrokClassifierRequest'
    { name,
      classification = Core.Nothing,
      customPatterns = Core.Nothing,
      grokPattern = Core.Nothing
    }

-- | The name of the @GrokClassifier@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugcrName :: Lens.Lens' UpdateGrokClassifierRequest Types.NameString
ugcrName = Lens.field @"name"
{-# DEPRECATED ugcrName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
--
-- /Note:/ Consider using 'classification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugcrClassification :: Lens.Lens' UpdateGrokClassifierRequest (Core.Maybe Types.Classification)
ugcrClassification = Lens.field @"classification"
{-# DEPRECATED ugcrClassification "Use generic-lens or generic-optics with 'classification' instead." #-}

-- | Optional custom grok patterns used by this classifier.
--
-- /Note:/ Consider using 'customPatterns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugcrCustomPatterns :: Lens.Lens' UpdateGrokClassifierRequest (Core.Maybe Types.CustomPatterns)
ugcrCustomPatterns = Lens.field @"customPatterns"
{-# DEPRECATED ugcrCustomPatterns "Use generic-lens or generic-optics with 'customPatterns' instead." #-}

-- | The grok pattern used by this classifier.
--
-- /Note:/ Consider using 'grokPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ugcrGrokPattern :: Lens.Lens' UpdateGrokClassifierRequest (Core.Maybe Types.GrokPattern)
ugcrGrokPattern = Lens.field @"grokPattern"
{-# DEPRECATED ugcrGrokPattern "Use generic-lens or generic-optics with 'grokPattern' instead." #-}

instance Core.FromJSON UpdateGrokClassifierRequest where
  toJSON UpdateGrokClassifierRequest {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            ("Classification" Core..=) Core.<$> classification,
            ("CustomPatterns" Core..=) Core.<$> customPatterns,
            ("GrokPattern" Core..=) Core.<$> grokPattern
          ]
      )
