{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.StaticValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.StaticValue
  ( StaticValue (..),

    -- * Smart constructor
    mkStaticValue,

    -- * Lenses
    svValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The static value of the resource.
--
-- /See:/ 'mkStaticValue' smart constructor.
newtype StaticValue = StaticValue'
  { -- | A list of values. For example, the ARN of the assumed role.
    values :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StaticValue' with the minimum fields required to make a request.
--
-- * 'values' - A list of values. For example, the ARN of the assumed role.
mkStaticValue ::
  StaticValue
mkStaticValue = StaticValue' {values = Lude.mempty}

-- | A list of values. For example, the ARN of the assumed role.
--
-- /Note:/ Consider using 'values' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
svValues :: Lens.Lens' StaticValue [Lude.Text]
svValues = Lens.lens (values :: StaticValue -> [Lude.Text]) (\s a -> s {values = a} :: StaticValue)
{-# DEPRECATED svValues "Use generic-lens or generic-optics with 'values' instead." #-}

instance Lude.FromJSON StaticValue where
  parseJSON =
    Lude.withObject
      "StaticValue"
      ( \x ->
          StaticValue' Lude.<$> (x Lude..:? "Values" Lude..!= Lude.mempty)
      )

instance Lude.ToJSON StaticValue where
  toJSON StaticValue' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("Values" Lude..= values)])
