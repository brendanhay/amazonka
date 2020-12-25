{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.GrokClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.GrokClassifier
  ( GrokClassifier (..),

    -- * Smart constructor
    mkGrokClassifier,

    -- * Lenses
    gcName,
    gcClassification,
    gcGrokPattern,
    gcCreationTime,
    gcCustomPatterns,
    gcLastUpdated,
    gcVersion,
  )
where

import qualified Network.AWS.Glue.Types.Classification as Types
import qualified Network.AWS.Glue.Types.CustomPatterns as Types
import qualified Network.AWS.Glue.Types.GrokPattern as Types
import qualified Network.AWS.Glue.Types.Name as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A classifier that uses @grok@ patterns.
--
-- /See:/ 'mkGrokClassifier' smart constructor.
data GrokClassifier = GrokClassifier'
  { -- | The name of the classifier.
    name :: Types.Name,
    -- | An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, and so on.
    classification :: Types.Classification,
    -- | The grok pattern applied to a data store by this classifier. For more information, see built-in patterns in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers> .
    grokPattern :: Types.GrokPattern,
    -- | The time that this classifier was registered.
    creationTime :: Core.Maybe Core.NominalDiffTime,
    -- | Optional custom grok patterns defined by this classifier. For more information, see custom patterns in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers> .
    customPatterns :: Core.Maybe Types.CustomPatterns,
    -- | The time that this classifier was last updated.
    lastUpdated :: Core.Maybe Core.NominalDiffTime,
    -- | The version of this classifier.
    version :: Core.Maybe Core.Integer
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GrokClassifier' value with any optional fields omitted.
mkGrokClassifier ::
  -- | 'name'
  Types.Name ->
  -- | 'classification'
  Types.Classification ->
  -- | 'grokPattern'
  Types.GrokPattern ->
  GrokClassifier
mkGrokClassifier name classification grokPattern =
  GrokClassifier'
    { name,
      classification,
      grokPattern,
      creationTime = Core.Nothing,
      customPatterns = Core.Nothing,
      lastUpdated = Core.Nothing,
      version = Core.Nothing
    }

-- | The name of the classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcName :: Lens.Lens' GrokClassifier Types.Name
gcName = Lens.field @"name"
{-# DEPRECATED gcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, and so on.
--
-- /Note:/ Consider using 'classification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcClassification :: Lens.Lens' GrokClassifier Types.Classification
gcClassification = Lens.field @"classification"
{-# DEPRECATED gcClassification "Use generic-lens or generic-optics with 'classification' instead." #-}

-- | The grok pattern applied to a data store by this classifier. For more information, see built-in patterns in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers> .
--
-- /Note:/ Consider using 'grokPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcGrokPattern :: Lens.Lens' GrokClassifier Types.GrokPattern
gcGrokPattern = Lens.field @"grokPattern"
{-# DEPRECATED gcGrokPattern "Use generic-lens or generic-optics with 'grokPattern' instead." #-}

-- | The time that this classifier was registered.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCreationTime :: Lens.Lens' GrokClassifier (Core.Maybe Core.NominalDiffTime)
gcCreationTime = Lens.field @"creationTime"
{-# DEPRECATED gcCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | Optional custom grok patterns defined by this classifier. For more information, see custom patterns in <https://docs.aws.amazon.com/glue/latest/dg/custom-classifier.html Writing Custom Classifiers> .
--
-- /Note:/ Consider using 'customPatterns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcCustomPatterns :: Lens.Lens' GrokClassifier (Core.Maybe Types.CustomPatterns)
gcCustomPatterns = Lens.field @"customPatterns"
{-# DEPRECATED gcCustomPatterns "Use generic-lens or generic-optics with 'customPatterns' instead." #-}

-- | The time that this classifier was last updated.
--
-- /Note:/ Consider using 'lastUpdated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcLastUpdated :: Lens.Lens' GrokClassifier (Core.Maybe Core.NominalDiffTime)
gcLastUpdated = Lens.field @"lastUpdated"
{-# DEPRECATED gcLastUpdated "Use generic-lens or generic-optics with 'lastUpdated' instead." #-}

-- | The version of this classifier.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcVersion :: Lens.Lens' GrokClassifier (Core.Maybe Core.Integer)
gcVersion = Lens.field @"version"
{-# DEPRECATED gcVersion "Use generic-lens or generic-optics with 'version' instead." #-}

instance Core.FromJSON GrokClassifier where
  parseJSON =
    Core.withObject "GrokClassifier" Core.$
      \x ->
        GrokClassifier'
          Core.<$> (x Core..: "Name")
          Core.<*> (x Core..: "Classification")
          Core.<*> (x Core..: "GrokPattern")
          Core.<*> (x Core..:? "CreationTime")
          Core.<*> (x Core..:? "CustomPatterns")
          Core.<*> (x Core..:? "LastUpdated")
          Core.<*> (x Core..:? "Version")
