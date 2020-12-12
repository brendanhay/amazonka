{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServiceCatalog.Types.ParameterConstraints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ParameterConstraints
  ( ParameterConstraints (..),

    -- * Smart constructor
    mkParameterConstraints,

    -- * Lenses
    pcAllowedValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The constraints that the administrator has put on the parameter.
--
-- /See:/ 'mkParameterConstraints' smart constructor.
newtype ParameterConstraints = ParameterConstraints'
  { allowedValues ::
      Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParameterConstraints' with the minimum fields required to make a request.
--
-- * 'allowedValues' - The values that the administrator has allowed for the parameter.
mkParameterConstraints ::
  ParameterConstraints
mkParameterConstraints =
  ParameterConstraints' {allowedValues = Lude.Nothing}

-- | The values that the administrator has allowed for the parameter.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcAllowedValues :: Lens.Lens' ParameterConstraints (Lude.Maybe [Lude.Text])
pcAllowedValues = Lens.lens (allowedValues :: ParameterConstraints -> Lude.Maybe [Lude.Text]) (\s a -> s {allowedValues = a} :: ParameterConstraints)
{-# DEPRECATED pcAllowedValues "Use generic-lens or generic-optics with 'allowedValues' instead." #-}

instance Lude.FromJSON ParameterConstraints where
  parseJSON =
    Lude.withObject
      "ParameterConstraints"
      ( \x ->
          ParameterConstraints'
            Lude.<$> (x Lude..:? "AllowedValues" Lude..!= Lude.mempty)
      )
