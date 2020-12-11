-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ParameterNameValue
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ParameterNameValue
  ( ParameterNameValue (..),

    -- * Smart constructor
    mkParameterNameValue,

    -- * Lenses
    pnvParameterValue,
    pnvParameterName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a name-value pair that is used to update the value of a parameter.
--
-- /See:/ 'mkParameterNameValue' smart constructor.
data ParameterNameValue = ParameterNameValue'
  { parameterValue ::
      Lude.Maybe Lude.Text,
    parameterName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ParameterNameValue' with the minimum fields required to make a request.
--
-- * 'parameterName' - The name of the parameter.
-- * 'parameterValue' - The value of the parameter.
mkParameterNameValue ::
  ParameterNameValue
mkParameterNameValue =
  ParameterNameValue'
    { parameterValue = Lude.Nothing,
      parameterName = Lude.Nothing
    }

-- | The value of the parameter.
--
-- /Note:/ Consider using 'parameterValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnvParameterValue :: Lens.Lens' ParameterNameValue (Lude.Maybe Lude.Text)
pnvParameterValue = Lens.lens (parameterValue :: ParameterNameValue -> Lude.Maybe Lude.Text) (\s a -> s {parameterValue = a} :: ParameterNameValue)
{-# DEPRECATED pnvParameterValue "Use generic-lens or generic-optics with 'parameterValue' instead." #-}

-- | The name of the parameter.
--
-- /Note:/ Consider using 'parameterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pnvParameterName :: Lens.Lens' ParameterNameValue (Lude.Maybe Lude.Text)
pnvParameterName = Lens.lens (parameterName :: ParameterNameValue -> Lude.Maybe Lude.Text) (\s a -> s {parameterName = a} :: ParameterNameValue)
{-# DEPRECATED pnvParameterName "Use generic-lens or generic-optics with 'parameterName' instead." #-}

instance Lude.ToQuery ParameterNameValue where
  toQuery ParameterNameValue' {..} =
    Lude.mconcat
      [ "ParameterValue" Lude.=: parameterValue,
        "ParameterName" Lude.=: parameterName
      ]
