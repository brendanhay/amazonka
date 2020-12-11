-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.CodeGenNodeArg
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.CodeGenNodeArg
  ( CodeGenNodeArg (..),

    -- * Smart constructor
    mkCodeGenNodeArg,

    -- * Lenses
    cgnaParam,
    cgnaName,
    cgnaValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An argument or property of a node.
--
-- /See:/ 'mkCodeGenNodeArg' smart constructor.
data CodeGenNodeArg = CodeGenNodeArg'
  { param ::
      Lude.Maybe Lude.Bool,
    name :: Lude.Text,
    value :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CodeGenNodeArg' with the minimum fields required to make a request.
--
-- * 'name' - The name of the argument or property.
-- * 'param' - True if the value is used as a parameter.
-- * 'value' - The value of the argument or property.
mkCodeGenNodeArg ::
  -- | 'name'
  Lude.Text ->
  -- | 'value'
  Lude.Text ->
  CodeGenNodeArg
mkCodeGenNodeArg pName_ pValue_ =
  CodeGenNodeArg'
    { param = Lude.Nothing,
      name = pName_,
      value = pValue_
    }

-- | True if the value is used as a parameter.
--
-- /Note:/ Consider using 'param' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgnaParam :: Lens.Lens' CodeGenNodeArg (Lude.Maybe Lude.Bool)
cgnaParam = Lens.lens (param :: CodeGenNodeArg -> Lude.Maybe Lude.Bool) (\s a -> s {param = a} :: CodeGenNodeArg)
{-# DEPRECATED cgnaParam "Use generic-lens or generic-optics with 'param' instead." #-}

-- | The name of the argument or property.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgnaName :: Lens.Lens' CodeGenNodeArg Lude.Text
cgnaName = Lens.lens (name :: CodeGenNodeArg -> Lude.Text) (\s a -> s {name = a} :: CodeGenNodeArg)
{-# DEPRECATED cgnaName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The value of the argument or property.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cgnaValue :: Lens.Lens' CodeGenNodeArg Lude.Text
cgnaValue = Lens.lens (value :: CodeGenNodeArg -> Lude.Text) (\s a -> s {value = a} :: CodeGenNodeArg)
{-# DEPRECATED cgnaValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromJSON CodeGenNodeArg where
  parseJSON =
    Lude.withObject
      "CodeGenNodeArg"
      ( \x ->
          CodeGenNodeArg'
            Lude.<$> (x Lude..:? "Param")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "Value")
      )

instance Lude.ToJSON CodeGenNodeArg where
  toJSON CodeGenNodeArg' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Param" Lude..=) Lude.<$> param,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Value" Lude..= value)
          ]
      )
