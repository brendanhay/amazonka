{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.AttributeKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.AttributeKey
  ( AttributeKey (..),

    -- * Smart constructor
    mkAttributeKey,

    -- * Lenses
    akFacetName,
    akSchemaARN,
    akName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A unique identifier for an attribute.
--
-- /See:/ 'mkAttributeKey' smart constructor.
data AttributeKey = AttributeKey'
  { -- | The name of the facet that the attribute exists within.
    facetName :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the schema that contains the facet and attribute.
    schemaARN :: Lude.Text,
    -- | The name of the attribute.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttributeKey' with the minimum fields required to make a request.
--
-- * 'facetName' - The name of the facet that the attribute exists within.
-- * 'schemaARN' - The Amazon Resource Name (ARN) of the schema that contains the facet and attribute.
-- * 'name' - The name of the attribute.
mkAttributeKey ::
  -- | 'facetName'
  Lude.Text ->
  -- | 'schemaARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  AttributeKey
mkAttributeKey pFacetName_ pSchemaARN_ pName_ =
  AttributeKey'
    { facetName = pFacetName_,
      schemaARN = pSchemaARN_,
      name = pName_
    }

-- | The name of the facet that the attribute exists within.
--
-- /Note:/ Consider using 'facetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akFacetName :: Lens.Lens' AttributeKey Lude.Text
akFacetName = Lens.lens (facetName :: AttributeKey -> Lude.Text) (\s a -> s {facetName = a} :: AttributeKey)
{-# DEPRECATED akFacetName "Use generic-lens or generic-optics with 'facetName' instead." #-}

-- | The Amazon Resource Name (ARN) of the schema that contains the facet and attribute.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akSchemaARN :: Lens.Lens' AttributeKey Lude.Text
akSchemaARN = Lens.lens (schemaARN :: AttributeKey -> Lude.Text) (\s a -> s {schemaARN = a} :: AttributeKey)
{-# DEPRECATED akSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The name of the attribute.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
akName :: Lens.Lens' AttributeKey Lude.Text
akName = Lens.lens (name :: AttributeKey -> Lude.Text) (\s a -> s {name = a} :: AttributeKey)
{-# DEPRECATED akName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON AttributeKey where
  parseJSON =
    Lude.withObject
      "AttributeKey"
      ( \x ->
          AttributeKey'
            Lude.<$> (x Lude..: "FacetName")
            Lude.<*> (x Lude..: "SchemaArn")
            Lude.<*> (x Lude..: "Name")
      )

instance Lude.ToJSON AttributeKey where
  toJSON AttributeKey' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("FacetName" Lude..= facetName),
            Lude.Just ("SchemaArn" Lude..= schemaARN),
            Lude.Just ("Name" Lude..= name)
          ]
      )
