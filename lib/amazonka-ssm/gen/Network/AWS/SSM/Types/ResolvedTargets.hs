{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.ResolvedTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.ResolvedTargets
  ( ResolvedTargets (..),

    -- * Smart constructor
    mkResolvedTargets,

    -- * Lenses
    rtTruncated,
    rtParameterValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about targets that resolved during the Automation execution.
--
-- /See:/ 'mkResolvedTargets' smart constructor.
data ResolvedTargets = ResolvedTargets'
  { -- | A boolean value indicating whether the resolved target list is truncated.
    truncated :: Lude.Maybe Lude.Bool,
    -- | A list of parameter values sent to targets that resolved during the Automation execution.
    parameterValues :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ResolvedTargets' with the minimum fields required to make a request.
--
-- * 'truncated' - A boolean value indicating whether the resolved target list is truncated.
-- * 'parameterValues' - A list of parameter values sent to targets that resolved during the Automation execution.
mkResolvedTargets ::
  ResolvedTargets
mkResolvedTargets =
  ResolvedTargets'
    { truncated = Lude.Nothing,
      parameterValues = Lude.Nothing
    }

-- | A boolean value indicating whether the resolved target list is truncated.
--
-- /Note:/ Consider using 'truncated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtTruncated :: Lens.Lens' ResolvedTargets (Lude.Maybe Lude.Bool)
rtTruncated = Lens.lens (truncated :: ResolvedTargets -> Lude.Maybe Lude.Bool) (\s a -> s {truncated = a} :: ResolvedTargets)
{-# DEPRECATED rtTruncated "Use generic-lens or generic-optics with 'truncated' instead." #-}

-- | A list of parameter values sent to targets that resolved during the Automation execution.
--
-- /Note:/ Consider using 'parameterValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtParameterValues :: Lens.Lens' ResolvedTargets (Lude.Maybe [Lude.Text])
rtParameterValues = Lens.lens (parameterValues :: ResolvedTargets -> Lude.Maybe [Lude.Text]) (\s a -> s {parameterValues = a} :: ResolvedTargets)
{-# DEPRECATED rtParameterValues "Use generic-lens or generic-optics with 'parameterValues' instead." #-}

instance Lude.FromJSON ResolvedTargets where
  parseJSON =
    Lude.withObject
      "ResolvedTargets"
      ( \x ->
          ResolvedTargets'
            Lude.<$> (x Lude..:? "Truncated")
            Lude.<*> (x Lude..:? "ParameterValues" Lude..!= Lude.mempty)
      )
