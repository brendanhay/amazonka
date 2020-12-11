-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CodeGenEdge
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CodeGenEdge
  ( CodeGenEdge (..),

    -- * Smart constructor
    mkCodeGenEdge,

    -- * Lenses
    cgeTargetParameter,
    cgeSource,
    cgeTarget,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a directional edge in a directed acyclic graph (DAG).
--
-- /See:/ 'mkCodeGenEdge' smart constructor.
data CodeGenEdge = CodeGenEdge'
  { targetParameter ::
      Lude.Maybe Lude.Text,
    source :: Lude.Text,
    target :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CodeGenEdge' with the minimum fields required to make a request.
--
-- * 'source' - The ID of the node at which the edge starts.
-- * 'target' - The ID of the node at which the edge ends.
-- * 'targetParameter' - The target of the edge.
mkCodeGenEdge ::
  -- | 'source'
  Lude.Text ->
  -- | 'target'
  Lude.Text ->
  CodeGenEdge
mkCodeGenEdge pSource_ pTarget_ =
  CodeGenEdge'
    { targetParameter = Lude.Nothing,
      source = pSource_,
      target = pTarget_
    }

-- | The target of the edge.
--
-- /Note:/ Consider using 'targetParameter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgeTargetParameter :: Lens.Lens' CodeGenEdge (Lude.Maybe Lude.Text)
cgeTargetParameter = Lens.lens (targetParameter :: CodeGenEdge -> Lude.Maybe Lude.Text) (\s a -> s {targetParameter = a} :: CodeGenEdge)
{-# DEPRECATED cgeTargetParameter "Use generic-lens or generic-optics with 'targetParameter' instead." #-}

-- | The ID of the node at which the edge starts.
--
-- /Note:/ Consider using 'source' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgeSource :: Lens.Lens' CodeGenEdge Lude.Text
cgeSource = Lens.lens (source :: CodeGenEdge -> Lude.Text) (\s a -> s {source = a} :: CodeGenEdge)
{-# DEPRECATED cgeSource "Use generic-lens or generic-optics with 'source' instead." #-}

-- | The ID of the node at which the edge ends.
--
-- /Note:/ Consider using 'target' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgeTarget :: Lens.Lens' CodeGenEdge Lude.Text
cgeTarget = Lens.lens (target :: CodeGenEdge -> Lude.Text) (\s a -> s {target = a} :: CodeGenEdge)
{-# DEPRECATED cgeTarget "Use generic-lens or generic-optics with 'target' instead." #-}

instance Lude.FromJSON CodeGenEdge where
  parseJSON =
    Lude.withObject
      "CodeGenEdge"
      ( \x ->
          CodeGenEdge'
            Lude.<$> (x Lude..:? "TargetParameter")
            Lude.<*> (x Lude..: "Source")
            Lude.<*> (x Lude..: "Target")
      )

instance Lude.ToJSON CodeGenEdge where
  toJSON CodeGenEdge' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TargetParameter" Lude..=) Lude.<$> targetParameter,
            Lude.Just ("Source" Lude..= source),
            Lude.Just ("Target" Lude..= target)
          ]
      )
