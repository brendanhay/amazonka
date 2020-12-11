-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.Rule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.Rule
  ( Rule (..),

    -- * Smart constructor
    mkRule,

    -- * Lenses
    rParameters,
    rType,
  )
where

import Network.AWS.CloudDirectory.Types.RuleType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains an Amazon Resource Name (ARN) and parameters that are associated with the rule.
--
-- /See:/ 'mkRule' smart constructor.
data Rule = Rule'
  { parameters ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    type' :: Lude.Maybe RuleType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Rule' with the minimum fields required to make a request.
--
-- * 'parameters' - The minimum and maximum parameters that are associated with the rule.
-- * 'type'' - The type of attribute validation rule.
mkRule ::
  Rule
mkRule = Rule' {parameters = Lude.Nothing, type' = Lude.Nothing}

-- | The minimum and maximum parameters that are associated with the rule.
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rParameters :: Lens.Lens' Rule (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
rParameters = Lens.lens (parameters :: Rule -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: Rule)
{-# DEPRECATED rParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The type of attribute validation rule.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rType :: Lens.Lens' Rule (Lude.Maybe RuleType)
rType = Lens.lens (type' :: Rule -> Lude.Maybe RuleType) (\s a -> s {type' = a} :: Rule)
{-# DEPRECATED rType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON Rule where
  parseJSON =
    Lude.withObject
      "Rule"
      ( \x ->
          Rule'
            Lude.<$> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Type")
      )

instance Lude.ToJSON Rule where
  toJSON Rule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Parameters" Lude..=) Lude.<$> parameters,
            ("Type" Lude..=) Lude.<$> type'
          ]
      )
