{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ResolvedAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Types.ResolvedAttributes
  ( ResolvedAttributes (..)
  -- * Smart constructor
  , mkResolvedAttributes
  -- * Lenses
  , raAutoMLJobObjective
  , raCompletionCriteria
  , raProblemType
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SageMaker.Types.AutoMLJobCompletionCriteria as Types
import qualified Network.AWS.SageMaker.Types.AutoMLJobObjective as Types
import qualified Network.AWS.SageMaker.Types.ProblemType as Types

-- | The resolved attributes.
--
-- /See:/ 'mkResolvedAttributes' smart constructor.
data ResolvedAttributes = ResolvedAttributes'
  { autoMLJobObjective :: Core.Maybe Types.AutoMLJobObjective
  , completionCriteria :: Core.Maybe Types.AutoMLJobCompletionCriteria
  , problemType :: Core.Maybe Types.ProblemType
    -- ^ The problem type.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ResolvedAttributes' value with any optional fields omitted.
mkResolvedAttributes
    :: ResolvedAttributes
mkResolvedAttributes
  = ResolvedAttributes'{autoMLJobObjective = Core.Nothing,
                        completionCriteria = Core.Nothing, problemType = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'autoMLJobObjective' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raAutoMLJobObjective :: Lens.Lens' ResolvedAttributes (Core.Maybe Types.AutoMLJobObjective)
raAutoMLJobObjective = Lens.field @"autoMLJobObjective"
{-# INLINEABLE raAutoMLJobObjective #-}
{-# DEPRECATED autoMLJobObjective "Use generic-lens or generic-optics with 'autoMLJobObjective' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'completionCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raCompletionCriteria :: Lens.Lens' ResolvedAttributes (Core.Maybe Types.AutoMLJobCompletionCriteria)
raCompletionCriteria = Lens.field @"completionCriteria"
{-# INLINEABLE raCompletionCriteria #-}
{-# DEPRECATED completionCriteria "Use generic-lens or generic-optics with 'completionCriteria' instead"  #-}

-- | The problem type.
--
-- /Note:/ Consider using 'problemType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raProblemType :: Lens.Lens' ResolvedAttributes (Core.Maybe Types.ProblemType)
raProblemType = Lens.field @"problemType"
{-# INLINEABLE raProblemType #-}
{-# DEPRECATED problemType "Use generic-lens or generic-optics with 'problemType' instead"  #-}

instance Core.FromJSON ResolvedAttributes where
        parseJSON
          = Core.withObject "ResolvedAttributes" Core.$
              \ x ->
                ResolvedAttributes' Core.<$>
                  (x Core..:? "AutoMLJobObjective") Core.<*>
                    x Core..:? "CompletionCriteria"
                    Core.<*> x Core..:? "ProblemType"
