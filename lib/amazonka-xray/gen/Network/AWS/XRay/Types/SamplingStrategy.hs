{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.SamplingStrategy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.SamplingStrategy
  ( SamplingStrategy (..),

    -- * Smart constructor
    mkSamplingStrategy,

    -- * Lenses
    ssValue,
    ssName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.XRay.Types.SamplingStrategyName

-- | The name and value of a sampling rule to apply to a trace summary.
--
-- /See:/ 'mkSamplingStrategy' smart constructor.
data SamplingStrategy = SamplingStrategy'
  { value ::
      Lude.Maybe Lude.Double,
    name :: Lude.Maybe SamplingStrategyName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SamplingStrategy' with the minimum fields required to make a request.
--
-- * 'name' - The name of a sampling rule.
-- * 'value' - The value of a sampling rule.
mkSamplingStrategy ::
  SamplingStrategy
mkSamplingStrategy =
  SamplingStrategy' {value = Lude.Nothing, name = Lude.Nothing}

-- | The value of a sampling rule.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssValue :: Lens.Lens' SamplingStrategy (Lude.Maybe Lude.Double)
ssValue = Lens.lens (value :: SamplingStrategy -> Lude.Maybe Lude.Double) (\s a -> s {value = a} :: SamplingStrategy)
{-# DEPRECATED ssValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of a sampling rule.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssName :: Lens.Lens' SamplingStrategy (Lude.Maybe SamplingStrategyName)
ssName = Lens.lens (name :: SamplingStrategy -> Lude.Maybe SamplingStrategyName) (\s a -> s {name = a} :: SamplingStrategy)
{-# DEPRECATED ssName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToJSON SamplingStrategy where
  toJSON SamplingStrategy' {..} =
    Lude.object
      ( Lude.catMaybes
          [("Value" Lude..=) Lude.<$> value, ("Name" Lude..=) Lude.<$> name]
      )
