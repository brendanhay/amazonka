{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CreateGrokClassifierRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.CreateGrokClassifierRequest
  ( CreateGrokClassifierRequest (..)
  -- * Smart constructor
  , mkCreateGrokClassifierRequest
  -- * Lenses
  , cgcrClassification
  , cgcrName
  , cgcrGrokPattern
  , cgcrCustomPatterns
  ) where

import qualified Network.AWS.Glue.Types.Classification as Types
import qualified Network.AWS.Glue.Types.CustomPatterns as Types
import qualified Network.AWS.Glue.Types.GrokPattern as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies a @grok@ classifier for @CreateClassifier@ to create.
--
-- /See:/ 'mkCreateGrokClassifierRequest' smart constructor.
data CreateGrokClassifierRequest = CreateGrokClassifierRequest'
  { classification :: Types.Classification
    -- ^ An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
  , name :: Types.NameString
    -- ^ The name of the new classifier.
  , grokPattern :: Types.GrokPattern
    -- ^ The grok pattern used by this classifier.
  , customPatterns :: Core.Maybe Types.CustomPatterns
    -- ^ Optional custom grok patterns used by this classifier.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateGrokClassifierRequest' value with any optional fields omitted.
mkCreateGrokClassifierRequest
    :: Types.Classification -- ^ 'classification'
    -> Types.NameString -- ^ 'name'
    -> Types.GrokPattern -- ^ 'grokPattern'
    -> CreateGrokClassifierRequest
mkCreateGrokClassifierRequest classification name grokPattern
  = CreateGrokClassifierRequest'{classification, name, grokPattern,
                                 customPatterns = Core.Nothing}

-- | An identifier of the data format that the classifier matches, such as Twitter, JSON, Omniture logs, Amazon CloudWatch Logs, and so on.
--
-- /Note:/ Consider using 'classification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcrClassification :: Lens.Lens' CreateGrokClassifierRequest Types.Classification
cgcrClassification = Lens.field @"classification"
{-# INLINEABLE cgcrClassification #-}
{-# DEPRECATED classification "Use generic-lens or generic-optics with 'classification' instead"  #-}

-- | The name of the new classifier.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcrName :: Lens.Lens' CreateGrokClassifierRequest Types.NameString
cgcrName = Lens.field @"name"
{-# INLINEABLE cgcrName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The grok pattern used by this classifier.
--
-- /Note:/ Consider using 'grokPattern' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcrGrokPattern :: Lens.Lens' CreateGrokClassifierRequest Types.GrokPattern
cgcrGrokPattern = Lens.field @"grokPattern"
{-# INLINEABLE cgcrGrokPattern #-}
{-# DEPRECATED grokPattern "Use generic-lens or generic-optics with 'grokPattern' instead"  #-}

-- | Optional custom grok patterns used by this classifier.
--
-- /Note:/ Consider using 'customPatterns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgcrCustomPatterns :: Lens.Lens' CreateGrokClassifierRequest (Core.Maybe Types.CustomPatterns)
cgcrCustomPatterns = Lens.field @"customPatterns"
{-# INLINEABLE cgcrCustomPatterns #-}
{-# DEPRECATED customPatterns "Use generic-lens or generic-optics with 'customPatterns' instead"  #-}

instance Core.FromJSON CreateGrokClassifierRequest where
        toJSON CreateGrokClassifierRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Classification" Core..= classification),
                  Core.Just ("Name" Core..= name),
                  Core.Just ("GrokPattern" Core..= grokPattern),
                  ("CustomPatterns" Core..=) Core.<$> customPatterns])
