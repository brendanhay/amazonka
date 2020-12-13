{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.Types.ReplaceableAttribute
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SDB.Types.ReplaceableAttribute
  ( ReplaceableAttribute (..),

    -- * Smart constructor
    mkReplaceableAttribute,

    -- * Lenses
    raReplace,
    raValue,
    raName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- |
--
-- /See:/ 'mkReplaceableAttribute' smart constructor.
data ReplaceableAttribute = ReplaceableAttribute'
  { -- | @false@
    replace :: Lude.Maybe Lude.Bool,
    -- | The value of the replaceable attribute.
    value :: Lude.Text,
    -- | The name of the replaceable attribute.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReplaceableAttribute' with the minimum fields required to make a request.
--
-- * 'replace' - @false@
-- * 'value' - The value of the replaceable attribute.
-- * 'name' - The name of the replaceable attribute.
mkReplaceableAttribute ::
  -- | 'value'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  ReplaceableAttribute
mkReplaceableAttribute pValue_ pName_ =
  ReplaceableAttribute'
    { replace = Lude.Nothing,
      value = pValue_,
      name = pName_
    }

-- | @false@
--
-- /Note:/ Consider using 'replace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raReplace :: Lens.Lens' ReplaceableAttribute (Lude.Maybe Lude.Bool)
raReplace = Lens.lens (replace :: ReplaceableAttribute -> Lude.Maybe Lude.Bool) (\s a -> s {replace = a} :: ReplaceableAttribute)
{-# DEPRECATED raReplace "Use generic-lens or generic-optics with 'replace' instead." #-}

-- | The value of the replaceable attribute.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raValue :: Lens.Lens' ReplaceableAttribute Lude.Text
raValue = Lens.lens (value :: ReplaceableAttribute -> Lude.Text) (\s a -> s {value = a} :: ReplaceableAttribute)
{-# DEPRECATED raValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The name of the replaceable attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
raName :: Lens.Lens' ReplaceableAttribute Lude.Text
raName = Lens.lens (name :: ReplaceableAttribute -> Lude.Text) (\s a -> s {name = a} :: ReplaceableAttribute)
{-# DEPRECATED raName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.ToQuery ReplaceableAttribute where
  toQuery ReplaceableAttribute' {..} =
    Lude.mconcat
      [ "Replace" Lude.=: replace,
        "Value" Lude.=: value,
        "Name" Lude.=: name
      ]
