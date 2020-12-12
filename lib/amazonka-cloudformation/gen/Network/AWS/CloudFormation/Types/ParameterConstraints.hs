{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.ParameterConstraints
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.ParameterConstraints
  ( ParameterConstraints (..),

    -- * Smart constructor
    mkParameterConstraints,

    -- * Lenses
    pcAllowedValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A set of criteria that AWS CloudFormation uses to validate parameter values. Although other constraints might be defined in the stack template, AWS CloudFormation returns only the @AllowedValues@ property.
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
-- * 'allowedValues' - A list of values that are permitted for a parameter.
mkParameterConstraints ::
  ParameterConstraints
mkParameterConstraints =
  ParameterConstraints' {allowedValues = Lude.Nothing}

-- | A list of values that are permitted for a parameter.
--
-- /Note:/ Consider using 'allowedValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pcAllowedValues :: Lens.Lens' ParameterConstraints (Lude.Maybe [Lude.Text])
pcAllowedValues = Lens.lens (allowedValues :: ParameterConstraints -> Lude.Maybe [Lude.Text]) (\s a -> s {allowedValues = a} :: ParameterConstraints)
{-# DEPRECATED pcAllowedValues "Use generic-lens or generic-optics with 'allowedValues' instead." #-}

instance Lude.FromXML ParameterConstraints where
  parseXML x =
    ParameterConstraints'
      Lude.<$> ( x Lude..@? "AllowedValues" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
