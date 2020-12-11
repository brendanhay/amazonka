-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.Type
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.Type
  ( Type (..),

    -- * Smart constructor
    mkType,

    -- * Lenses
    tArn,
    tDefinition,
    tFormat,
    tName,
    tDescription,
  )
where

import Network.AWS.AppSync.Types.TypeDefinitionFormat
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a type.
--
-- /See:/ 'mkType' smart constructor.
data Type = Type'
  { arn :: Lude.Maybe Lude.Text,
    definition :: Lude.Maybe Lude.Text,
    format :: Lude.Maybe TypeDefinitionFormat,
    name :: Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Type' with the minimum fields required to make a request.
--
-- * 'arn' - The type ARN.
-- * 'definition' - The type definition.
-- * 'description' - The type description.
-- * 'format' - The type format: SDL or JSON.
-- * 'name' - The type name.
mkType ::
  Type
mkType =
  Type'
    { arn = Lude.Nothing,
      definition = Lude.Nothing,
      format = Lude.Nothing,
      name = Lude.Nothing,
      description = Lude.Nothing
    }

-- | The type ARN.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tArn :: Lens.Lens' Type (Lude.Maybe Lude.Text)
tArn = Lens.lens (arn :: Type -> Lude.Maybe Lude.Text) (\s a -> s {arn = a} :: Type)
{-# DEPRECATED tArn "Use generic-lens or generic-optics with 'arn' instead." #-}

-- | The type definition.
--
-- /Note:/ Consider using 'definition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDefinition :: Lens.Lens' Type (Lude.Maybe Lude.Text)
tDefinition = Lens.lens (definition :: Type -> Lude.Maybe Lude.Text) (\s a -> s {definition = a} :: Type)
{-# DEPRECATED tDefinition "Use generic-lens or generic-optics with 'definition' instead." #-}

-- | The type format: SDL or JSON.
--
-- /Note:/ Consider using 'format' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tFormat :: Lens.Lens' Type (Lude.Maybe TypeDefinitionFormat)
tFormat = Lens.lens (format :: Type -> Lude.Maybe TypeDefinitionFormat) (\s a -> s {format = a} :: Type)
{-# DEPRECATED tFormat "Use generic-lens or generic-optics with 'format' instead." #-}

-- | The type name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tName :: Lens.Lens' Type (Lude.Maybe Lude.Text)
tName = Lens.lens (name :: Type -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: Type)
{-# DEPRECATED tName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tDescription :: Lens.Lens' Type (Lude.Maybe Lude.Text)
tDescription = Lens.lens (description :: Type -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: Type)
{-# DEPRECATED tDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.FromJSON Type where
  parseJSON =
    Lude.withObject
      "Type"
      ( \x ->
          Type'
            Lude.<$> (x Lude..:? "arn")
            Lude.<*> (x Lude..:? "definition")
            Lude.<*> (x Lude..:? "format")
            Lude.<*> (x Lude..:? "name")
            Lude.<*> (x Lude..:? "description")
      )
