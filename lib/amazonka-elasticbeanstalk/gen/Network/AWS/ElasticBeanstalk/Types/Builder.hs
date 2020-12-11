-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Types.Builder
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Types.Builder
  ( Builder (..),

    -- * Smart constructor
    mkBuilder,

    -- * Lenses
    bARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The builder used to build the custom platform.
--
-- /See:/ 'mkBuilder' smart constructor.
newtype Builder = Builder' {arn :: Lude.Maybe Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Builder' with the minimum fields required to make a request.
--
-- * 'arn' - The ARN of the builder.
mkBuilder ::
  Builder
mkBuilder = Builder' {arn = Lude.Nothing}

-- | The ARN of the builder.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bARN :: Lens.Lens' Builder (Lude.Maybe Lude.Text)
bARN = Lens.lens (arn :: Builder -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Builder)
{-# DEPRECATED bARN "Use generic-lens or generic-optics with 'arn' instead." #-}

instance Lude.FromXML Builder where
  parseXML x = Builder' Lude.<$> (x Lude..@? "ARN")
