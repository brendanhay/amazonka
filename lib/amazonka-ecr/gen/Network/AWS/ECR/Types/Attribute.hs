{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECR.Types.Attribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECR.Types.Attribute
  ( Attribute (..),

    -- * Smart constructor
    mkAttribute,

    -- * Lenses
    aValue,
    aKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This data type is used in the 'ImageScanFinding' data type.
--
-- /See:/ 'mkAttribute' smart constructor.
data Attribute = Attribute'
  { -- | The value assigned to the attribute key.
    value :: Lude.Maybe Lude.Text,
    -- | The attribute key.
    key :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Attribute' with the minimum fields required to make a request.
--
-- * 'value' - The value assigned to the attribute key.
-- * 'key' - The attribute key.
mkAttribute ::
  -- | 'key'
  Lude.Text ->
  Attribute
mkAttribute pKey_ = Attribute' {value = Lude.Nothing, key = pKey_}

-- | The value assigned to the attribute key.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aValue :: Lens.Lens' Attribute (Lude.Maybe Lude.Text)
aValue = Lens.lens (value :: Attribute -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Attribute)
{-# DEPRECATED aValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The attribute key.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aKey :: Lens.Lens' Attribute Lude.Text
aKey = Lens.lens (key :: Attribute -> Lude.Text) (\s a -> s {key = a} :: Attribute)
{-# DEPRECATED aKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON Attribute where
  parseJSON =
    Lude.withObject
      "Attribute"
      ( \x ->
          Attribute'
            Lude.<$> (x Lude..:? "value") Lude.<*> (x Lude..: "key")
      )
