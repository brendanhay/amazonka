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
    tladDefaultValue,
    tladIsImmutable,
    tladName,
    tladType,
    tladRequiredBehavior,
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
  { rules ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Rule)),
    defaultValue ::
      Lude.Maybe TypedAttributeValue,
    isImmutable ::
      Lude.Maybe Lude.Bool,
    name :: Lude.Text,
    type' :: FacetAttributeType,
    requiredBehavior ::
      RequiredAttributeBehavior
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TypedLinkAttributeDefinition' with the minimum fields required to make a request.
--
-- * 'defaultValue' - The default value of the attribute (if configured).
-- * 'isImmutable' - Whether the attribute is mutable or not.
-- * 'name' - The unique name of the typed link attribute.
-- * 'requiredBehavior' - The required behavior of the @TypedLinkAttributeDefinition@ .
-- * 'rules' - Validation rules that are attached to the attribute definition.
-- * 'type'' - The type of the attribute.
mkTypedLinkAttributeDefinition ::
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  FacetAttributeType ->
  -- | 'requiredBehavior'
  RequiredAttributeBehavior ->
  TypedLinkAttributeDefinition
mkTypedLinkAttributeDefinition pName_ pType_ pRequiredBehavior_ =
  TypedLinkAttributeDefinition'
    { rules = Lude.Nothing,
      defaultValue = Lude.Nothing,
      isImmutable = Lude.Nothing,
      name = pName_,
      type' = pType_,
      requiredBehavior = pRequiredBehavior_
    }

-- | Validation rules that are attached to the attribute definition.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladRules :: Lens.Lens' TypedLinkAttributeDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Rule)))
tladRules = Lens.lens (rules :: TypedLinkAttributeDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Rule))) (\s a -> s {rules = a} :: TypedLinkAttributeDefinition)
{-# DEPRECATED tladRules "Use generic-lens or generic-optics with 'rules' instead." #-}

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

-- | The unique name of the typed link attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladName :: Lens.Lens' TypedLinkAttributeDefinition Lude.Text
tladName = Lens.lens (name :: TypedLinkAttributeDefinition -> Lude.Text) (\s a -> s {name = a} :: TypedLinkAttributeDefinition)
{-# DEPRECATED tladName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of the attribute.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladType :: Lens.Lens' TypedLinkAttributeDefinition FacetAttributeType
tladType = Lens.lens (type' :: TypedLinkAttributeDefinition -> FacetAttributeType) (\s a -> s {type' = a} :: TypedLinkAttributeDefinition)
{-# DEPRECATED tladType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The required behavior of the @TypedLinkAttributeDefinition@ .
--
-- /Note:/ Consider using 'requiredBehavior' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tladRequiredBehavior :: Lens.Lens' TypedLinkAttributeDefinition RequiredAttributeBehavior
tladRequiredBehavior = Lens.lens (requiredBehavior :: TypedLinkAttributeDefinition -> RequiredAttributeBehavior) (\s a -> s {requiredBehavior = a} :: TypedLinkAttributeDefinition)
{-# DEPRECATED tladRequiredBehavior "Use generic-lens or generic-optics with 'requiredBehavior' instead." #-}

instance Lude.FromJSON TypedLinkAttributeDefinition where
  parseJSON =
    Lude.withObject
      "TypedLinkAttributeDefinition"
      ( \x ->
          TypedLinkAttributeDefinition'
            Lude.<$> (x Lude..:? "Rules" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DefaultValue")
            Lude.<*> (x Lude..:? "IsImmutable")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "Type")
            Lude.<*> (x Lude..: "RequiredBehavior")
      )

instance Lude.ToJSON TypedLinkAttributeDefinition where
  toJSON TypedLinkAttributeDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Rules" Lude..=) Lude.<$> rules,
            ("DefaultValue" Lude..=) Lude.<$> defaultValue,
            ("IsImmutable" Lude..=) Lude.<$> isImmutable,
            Lude.Just ("Name" Lude..= name),
            Lude.Just ("Type" Lude..= type'),
            Lude.Just ("RequiredBehavior" Lude..= requiredBehavior)
          ]
      )
