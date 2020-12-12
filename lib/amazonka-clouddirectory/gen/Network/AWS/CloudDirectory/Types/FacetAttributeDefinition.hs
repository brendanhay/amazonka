{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.FacetAttributeDefinition
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.FacetAttributeDefinition
  ( FacetAttributeDefinition (..),

    -- * Smart constructor
    mkFacetAttributeDefinition,

    -- * Lenses
    fadRules,
    fadDefaultValue,
    fadIsImmutable,
    fadType,
  )
where

import Network.AWS.CloudDirectory.Types.FacetAttributeType
import Network.AWS.CloudDirectory.Types.Rule
import Network.AWS.CloudDirectory.Types.TypedAttributeValue
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A facet attribute definition. See <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_attributereferences.html Attribute References> for more information.
--
-- /See:/ 'mkFacetAttributeDefinition' smart constructor.
data FacetAttributeDefinition = FacetAttributeDefinition'
  { rules ::
      Lude.Maybe
        (Lude.HashMap Lude.Text (Rule)),
    defaultValue ::
      Lude.Maybe TypedAttributeValue,
    isImmutable :: Lude.Maybe Lude.Bool,
    type' :: FacetAttributeType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'FacetAttributeDefinition' with the minimum fields required to make a request.
--
-- * 'defaultValue' - The default value of the attribute (if configured).
-- * 'isImmutable' - Whether the attribute is mutable or not.
-- * 'rules' - Validation rules attached to the attribute definition.
-- * 'type'' - The type of the attribute.
mkFacetAttributeDefinition ::
  -- | 'type''
  FacetAttributeType ->
  FacetAttributeDefinition
mkFacetAttributeDefinition pType_ =
  FacetAttributeDefinition'
    { rules = Lude.Nothing,
      defaultValue = Lude.Nothing,
      isImmutable = Lude.Nothing,
      type' = pType_
    }

-- | Validation rules attached to the attribute definition.
--
-- /Note:/ Consider using 'rules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fadRules :: Lens.Lens' FacetAttributeDefinition (Lude.Maybe (Lude.HashMap Lude.Text (Rule)))
fadRules = Lens.lens (rules :: FacetAttributeDefinition -> Lude.Maybe (Lude.HashMap Lude.Text (Rule))) (\s a -> s {rules = a} :: FacetAttributeDefinition)
{-# DEPRECATED fadRules "Use generic-lens or generic-optics with 'rules' instead." #-}

-- | The default value of the attribute (if configured).
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fadDefaultValue :: Lens.Lens' FacetAttributeDefinition (Lude.Maybe TypedAttributeValue)
fadDefaultValue = Lens.lens (defaultValue :: FacetAttributeDefinition -> Lude.Maybe TypedAttributeValue) (\s a -> s {defaultValue = a} :: FacetAttributeDefinition)
{-# DEPRECATED fadDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

-- | Whether the attribute is mutable or not.
--
-- /Note:/ Consider using 'isImmutable' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fadIsImmutable :: Lens.Lens' FacetAttributeDefinition (Lude.Maybe Lude.Bool)
fadIsImmutable = Lens.lens (isImmutable :: FacetAttributeDefinition -> Lude.Maybe Lude.Bool) (\s a -> s {isImmutable = a} :: FacetAttributeDefinition)
{-# DEPRECATED fadIsImmutable "Use generic-lens or generic-optics with 'isImmutable' instead." #-}

-- | The type of the attribute.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
fadType :: Lens.Lens' FacetAttributeDefinition FacetAttributeType
fadType = Lens.lens (type' :: FacetAttributeDefinition -> FacetAttributeType) (\s a -> s {type' = a} :: FacetAttributeDefinition)
{-# DEPRECATED fadType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON FacetAttributeDefinition where
  parseJSON =
    Lude.withObject
      "FacetAttributeDefinition"
      ( \x ->
          FacetAttributeDefinition'
            Lude.<$> (x Lude..:? "Rules" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "DefaultValue")
            Lude.<*> (x Lude..:? "IsImmutable")
            Lude.<*> (x Lude..: "Type")
      )

instance Lude.ToJSON FacetAttributeDefinition where
  toJSON FacetAttributeDefinition' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Rules" Lude..=) Lude.<$> rules,
            ("DefaultValue" Lude..=) Lude.<$> defaultValue,
            ("IsImmutable" Lude..=) Lude.<$> isImmutable,
            Lude.Just ("Type" Lude..= type')
          ]
      )
