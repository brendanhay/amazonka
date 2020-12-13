{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.SchemaFacet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.SchemaFacet
  ( SchemaFacet (..),

    -- * Smart constructor
    mkSchemaFacet,

    -- * Lenses
    sfFacetName,
    sfSchemaARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A facet.
--
-- /See:/ 'mkSchemaFacet' smart constructor.
data SchemaFacet = SchemaFacet'
  { -- | The name of the facet.
    facetName :: Lude.Maybe Lude.Text,
    -- | The ARN of the schema that contains the facet with no minor component. See 'arns' and <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_inplaceschemaupgrade.html In-Place Schema Upgrade> for a description of when to provide minor versions.
    schemaARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SchemaFacet' with the minimum fields required to make a request.
--
-- * 'facetName' - The name of the facet.
-- * 'schemaARN' - The ARN of the schema that contains the facet with no minor component. See 'arns' and <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_inplaceschemaupgrade.html In-Place Schema Upgrade> for a description of when to provide minor versions.
mkSchemaFacet ::
  SchemaFacet
mkSchemaFacet =
  SchemaFacet' {facetName = Lude.Nothing, schemaARN = Lude.Nothing}

-- | The name of the facet.
--
-- /Note:/ Consider using 'facetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfFacetName :: Lens.Lens' SchemaFacet (Lude.Maybe Lude.Text)
sfFacetName = Lens.lens (facetName :: SchemaFacet -> Lude.Maybe Lude.Text) (\s a -> s {facetName = a} :: SchemaFacet)
{-# DEPRECATED sfFacetName "Use generic-lens or generic-optics with 'facetName' instead." #-}

-- | The ARN of the schema that contains the facet with no minor component. See 'arns' and <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/schemas_inplaceschemaupgrade.html In-Place Schema Upgrade> for a description of when to provide minor versions.
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sfSchemaARN :: Lens.Lens' SchemaFacet (Lude.Maybe Lude.Text)
sfSchemaARN = Lens.lens (schemaARN :: SchemaFacet -> Lude.Maybe Lude.Text) (\s a -> s {schemaARN = a} :: SchemaFacet)
{-# DEPRECATED sfSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

instance Lude.FromJSON SchemaFacet where
  parseJSON =
    Lude.withObject
      "SchemaFacet"
      ( \x ->
          SchemaFacet'
            Lude.<$> (x Lude..:? "FacetName") Lude.<*> (x Lude..:? "SchemaArn")
      )

instance Lude.ToJSON SchemaFacet where
  toJSON SchemaFacet' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("FacetName" Lude..=) Lude.<$> facetName,
            ("SchemaArn" Lude..=) Lude.<$> schemaARN
          ]
      )
