-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.ResolvedAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ResolvedAttributes
  ( ResolvedAttributes (..),

    -- * Smart constructor
    mkResolvedAttributes,

    -- * Lenses
    raProblemType,
    raAutoMLJobObjective,
    raCompletionCriteria,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SageMaker.Types.AutoMLJobCompletionCriteria
import Network.AWS.SageMaker.Types.AutoMLJobObjective
import Network.AWS.SageMaker.Types.ProblemType

-- | The resolved attributes.
--
-- /See:/ 'mkResolvedAttributes' smart constructor.
data ResolvedAttributes = ResolvedAttributes'
  { problemType ::
      Lude.Maybe ProblemType,
    autoMLJobObjective :: Lude.Maybe AutoMLJobObjective,
    completionCriteria ::
      Lude.Maybe AutoMLJobCompletionCriteria
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResolvedAttributes' with the minimum fields required to make a request.
--
-- * 'autoMLJobObjective' - Undocumented field.
-- * 'completionCriteria' - Undocumented field.
-- * 'problemType' - The problem type.
mkResolvedAttributes ::
  ResolvedAttributes
mkResolvedAttributes =
  ResolvedAttributes'
    { problemType = Lude.Nothing,
      autoMLJobObjective = Lude.Nothing,
      completionCriteria = Lude.Nothing
    }

-- | The problem type.
--
-- /Note:/ Consider using 'problemType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raProblemType :: Lens.Lens' ResolvedAttributes (Lude.Maybe ProblemType)
raProblemType = Lens.lens (problemType :: ResolvedAttributes -> Lude.Maybe ProblemType) (\s a -> s {problemType = a} :: ResolvedAttributes)
{-# DEPRECATED raProblemType "Use generic-lens or generic-optics with 'problemType' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'autoMLJobObjective' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raAutoMLJobObjective :: Lens.Lens' ResolvedAttributes (Lude.Maybe AutoMLJobObjective)
raAutoMLJobObjective = Lens.lens (autoMLJobObjective :: ResolvedAttributes -> Lude.Maybe AutoMLJobObjective) (\s a -> s {autoMLJobObjective = a} :: ResolvedAttributes)
{-# DEPRECATED raAutoMLJobObjective "Use generic-lens or generic-optics with 'autoMLJobObjective' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'completionCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raCompletionCriteria :: Lens.Lens' ResolvedAttributes (Lude.Maybe AutoMLJobCompletionCriteria)
raCompletionCriteria = Lens.lens (completionCriteria :: ResolvedAttributes -> Lude.Maybe AutoMLJobCompletionCriteria) (\s a -> s {completionCriteria = a} :: ResolvedAttributes)
{-# DEPRECATED raCompletionCriteria "Use generic-lens or generic-optics with 'completionCriteria' instead." #-}

instance Lude.FromJSON ResolvedAttributes where
  parseJSON =
    Lude.withObject
      "ResolvedAttributes"
      ( \x ->
          ResolvedAttributes'
            Lude.<$> (x Lude..:? "ProblemType")
            Lude.<*> (x Lude..:? "AutoMLJobObjective")
            Lude.<*> (x Lude..:? "CompletionCriteria")
      )
