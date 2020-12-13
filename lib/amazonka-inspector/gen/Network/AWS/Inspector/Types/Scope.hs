{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Inspector.Types.Scope
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Inspector.Types.Scope
  ( Scope (..),

    -- * Smart constructor
    mkScope,

    -- * Lenses
    sValue,
    sKey,
  )
where

import Network.AWS.Inspector.Types.ScopeType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | This data type contains key-value pairs that identify various Amazon resources.
--
-- /See:/ 'mkScope' smart constructor.
data Scope = Scope'
  { -- | The resource identifier for the specified scope type.
    value :: Lude.Maybe Lude.Text,
    -- | The type of the scope.
    key :: Lude.Maybe ScopeType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Scope' with the minimum fields required to make a request.
--
-- * 'value' - The resource identifier for the specified scope type.
-- * 'key' - The type of the scope.
mkScope ::
  Scope
mkScope = Scope' {value = Lude.Nothing, key = Lude.Nothing}

-- | The resource identifier for the specified scope type.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sValue :: Lens.Lens' Scope (Lude.Maybe Lude.Text)
sValue = Lens.lens (value :: Scope -> Lude.Maybe Lude.Text) (\s a -> s {value = a} :: Scope)
{-# DEPRECATED sValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The type of the scope.
--
-- /Note:/ Consider using 'key' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sKey :: Lens.Lens' Scope (Lude.Maybe ScopeType)
sKey = Lens.lens (key :: Scope -> Lude.Maybe ScopeType) (\s a -> s {key = a} :: Scope)
{-# DEPRECATED sKey "Use generic-lens or generic-optics with 'key' instead." #-}

instance Lude.FromJSON Scope where
  parseJSON =
    Lude.withObject
      "Scope"
      ( \x ->
          Scope' Lude.<$> (x Lude..:? "value") Lude.<*> (x Lude..:? "key")
      )
