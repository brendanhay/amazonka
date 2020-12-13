{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.Dimension
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.Dimension
  ( Dimension (..),

    -- * Smart constructor
    mkDimension,

    -- * Lenses
    dValue,
    dName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal

-- | For the metric that the CloudWatch alarm is associated with, a complex type that contains information about one dimension.
--
-- /See:/ 'mkDimension' smart constructor.
data Dimension = Dimension'
  { -- | For the metric that the CloudWatch alarm is associated with, the value of one dimension.
    value :: Lude.Text,
    -- | For the metric that the CloudWatch alarm is associated with, the name of one dimension.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Dimension' with the minimum fields required to make a request.
--
-- * 'value' - For the metric that the CloudWatch alarm is associated with, the value of one dimension.
-- * 'name' - For the metric that the CloudWatch alarm is associated with, the name of one dimension.
mkDimension ::
  -- | 'value'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  Dimension
mkDimension pValue_ pName_ =
  Dimension' {value = pValue_, name = pName_}

-- | For the metric that the CloudWatch alarm is associated with, the value of one dimension.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dValue :: Lens.Lens' Dimension Lude.Text
dValue = Lens.lens (value :: Dimension -> Lude.Text) (\s a -> s {value = a} :: Dimension)
{-# DEPRECATED dValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | For the metric that the CloudWatch alarm is associated with, the name of one dimension.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dName :: Lens.Lens' Dimension Lude.Text
dName = Lens.lens (name :: Dimension -> Lude.Text) (\s a -> s {name = a} :: Dimension)
{-# DEPRECATED dName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML Dimension where
  parseXML x =
    Dimension'
      Lude.<$> (x Lude..@ "Value") Lude.<*> (x Lude..@ "Name")
