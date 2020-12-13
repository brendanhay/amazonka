{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.TypedLinkAttributeDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.TypedLinkAttributeDefinition
  ( TypedLinkAttributeDefinition (..),

    -- * Smart constructor
    mkTypedLinkAttributeDefinition,

    -- * Lenses
    tladRules,
    tladName,
    tladRequiredBehavior,
    tladDefaultValue,
    tladIsImmutable,
    tladType,
  )
where

import Network.AWS.CloudDirectory.Types.FacetAttributeType
import Network.AWS.CloudDirectory.Types.RequiredAttributeBehavior
import Network.AWS.CloudDirectory.Types.Rule
import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A typed link attribute definition.
--
-- /See:/ 'mkTypedLinkAttributeDefinition' smart constructor.
data TypedLinkAttributeDefinition = TypedLinkAttributeDefinition'
  { -- | Validation rules that are attached to the attribute definition.
    rules :: Lude.Maybe (Lude.HashMap Lude.Text (Rule)),
    -- | The unique name of the typed link attribute.
    name :: Lude.Text,
    -- | The required behavior of the @TypedLinkAttributeDefinition@ .
    requiredBehavior :: RequiredAttributeBehavior,
    -- | The default value of the attribute (if configured).
    defaultValue :: Lude.Maybe TypedAttributeValue,
    -- | Whether the attribute is mutable or not.
    isImmutable :: Lude.Maybe Lude.Bool,
    -- | The type of the attribute.
    type' :: FacetAttributeType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TypedLinkAttributeDefinition' with the minimum fields required to make a request.
--
-- * 'rules' - Validation rules that are attached to the attribute definition.
-- * 'name' - The unique name of the typed link attribute.
-- * 'requiredBehavior' - The required behavior of the @TypedLinkAttributeDefinition@ .
-- * 'defaultValue' - The default value of the attribute (if configured).
-- * 'isImmutable' - Whether the attribute is mutable or not.
-- * 'type'' - The type of the attribute.
mkTypedLinkAttributeDefinition ::
  -- | 'name'
  Lude.Text ->
  -- | 'requiredBehavior'
  RequiredAttributeBehavior ->
  -- | 'type''
  FacetAttributeType ->
  TypedLinkAttributeDefinition
mkTypedLinkAttributeDefinition pName_ pRequiredBehavior_ pType_ =
  TypedLinkAttributeDefinition'
    { rules = Lude.Nothing,
      name = pName_,
      requiredBehavior = pRequiredBehavior_,
      defaultValue = Lude.Nothing,
      isImmutable = Lude.Nothing,
      type' = pType_
    }

-- | Validation rules that are attached to the attribute definition.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladRules :: Lens.Lens' TypedLinkAttributeDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Rule)))
tladRules = Lens.lens (rules :: TypedLinkAttributeDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Rule))) (\s a -> s {rules = a} :: TypedLinkAttributeDefinition)
{-# DEPRECATED tladRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The unique name of the typed link attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladName :: Lens.Lens' TypedLinkAttributeDefinition Lude.Text
tladName = Lens.lens (name :: TypedLinkAttributeDefinition -> Lude.Text) (\s a -> s {name = a} :: TypedLinkAttributeDefinition)
{-# DEPRECATED tladName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The required behavior of the @TypedLinkAttributeDefinition@ .
--
-- /Note:/ Consider using 'requiredBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladRequiredBehavior :: Lens.Lens' TypedLinkAttributeDefinition RequiredAttributeBehavior
tladRequiredBehavior = Lens.lens (requiredBehavior :: TypedLinkAttributeDefinition -> RequiredAttributeBehavior) (\s a -> s {requiredBehavior = a} :: TypedLinkAttributeDefinition)
{-# DEPRECATED tladRequiredBehavior "Use generic-lens or generic-optics with 'requiredBehavior' instead." #-}

-- | The default value of the attribute (if configured).
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladDefaultValue :: Lens.Lens' TypedLinkAttributeDefinition (Lude.Maybe TypedAttributeValue)
tladDefaultValue = Lens.lens (defaultValue :: TypedLinkAttributeDefinition -> Lude.Maybe TypedAttributeValue) (\s a -> s {defaultValue = a} :: TypedLinkAttributeDefinition)
{-# DEPRECATED tladDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | Whether the attribute is mutable or not.
--
-- /Note:/ Consider using 'isImmutable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladIsImmutable :: Lens.Lens' TypedLinkAttributeDefinition (Lude.Maybe Lude.Bool)
tladIsImmutable = Lens.lens (isImmutable :: TypedLinkAttributeDefinition -> Lude.Maybe Lude.Bool) (\s a -> s {isImmutable = a} :: TypedLinkAttributeDefinition)
{-# DEPRECATED tladIsImmutable "Use generic-lens or generic-optics with 'isImmutable' instead." #-}

-- | The type of the attribute.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladType :: Lens.Lens' TypedLinkAttributeDefinition FacetAttributeType
tladType = Lens.lens (type' :: TypedLinkAttributeDefinition -> FacetAttributeType) (\s a -> s {type' = a} :: TypedLinkAttributeDefinition)
{-# DEPRECATED tladType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON TypedLinkAttributeDefinition where
  parseJSON =
    Lude.withObject
      "TypedLinkAttributeDefinition"
      ( \x ->
          TypedLinkAttributeDefinition'
            Lude.<$> (x Lude..:? "Rules" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "RequiredBehavior")
            Lude.<*> (x Lude..:? "DefaultValue")
            Lude.<*> (x Lude..:? "IsImmutable")
            Lude.<*> (x Lude..: "Type")
      )

instance Lude.ToJSON TypedLinkAttributeDefinition where
  toJSON TypedLinkAttributeDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Rules" Lude..=) Lude.<$> rules,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("RequiredBehavior" Lude..= requiredBehavior),
            ("DefaultValue" Lude..=) Lude.<$> defaultValue,
            ("IsImmutable" Lude..=) Lude.<$> isImmutable,
            Lude.Just ("Type" Lude..= type')
          ]
      )
