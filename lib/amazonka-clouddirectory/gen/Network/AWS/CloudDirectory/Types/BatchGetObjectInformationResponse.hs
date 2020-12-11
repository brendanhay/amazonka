-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.Types.BatchGetObjectInformationResponse
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudDirectory.Types.BatchGetObjectInformationResponse
  ( BatchGetObjectInformationResponse (..),

    -- * Smart constructor
    mkBatchGetObjectInformationResponse,

    -- * Lenses
    bgoiObjectIdentifier,
    bgoiSchemaFacets,
  )
where

import Network.AWS.CloudDirectory.Types.SchemaFacet
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the output of a 'GetObjectInformation' response operation.
--
-- /See:/ 'mkBatchGetObjectInformationResponse' smart constructor.
data BatchGetObjectInformationResponse = BatchGetObjectInformationResponse'
  { objectIdentifier ::
      Lude.Maybe Lude.Text,
    schemaFacets ::
      Lude.Maybe
        [SchemaFacet]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BatchGetObjectInformationResponse' with the minimum fields required to make a request.
--
-- * 'objectIdentifier' - The @ObjectIdentifier@ of the specified object.
-- * 'schemaFacets' - The facets attached to the specified object.
mkBatchGetObjectInformationResponse ::
  BatchGetObjectInformationResponse
mkBatchGetObjectInformationResponse =
  BatchGetObjectInformationResponse'
    { objectIdentifier =
        Lude.Nothing,
      schemaFacets = Lude.Nothing
    }

-- | The @ObjectIdentifier@ of the specified object.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgoiObjectIdentifier :: Lens.Lens' BatchGetObjectInformationResponse (Lude.Maybe Lude.Text)
bgoiObjectIdentifier = Lens.lens (objectIdentifier :: BatchGetObjectInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {objectIdentifier = a} :: BatchGetObjectInformationResponse)
{-# DEPRECATED bgoiObjectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead." #-}

-- | The facets attached to the specified object.
--
-- /Note:/ Consider using 'schemaFacets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bgoiSchemaFacets :: Lens.Lens' BatchGetObjectInformationResponse (Lude.Maybe [SchemaFacet])
bgoiSchemaFacets = Lens.lens (schemaFacets :: BatchGetObjectInformationResponse -> Lude.Maybe [SchemaFacet]) (\s a -> s {schemaFacets = a} :: BatchGetObjectInformationResponse)
{-# DEPRECATED bgoiSchemaFacets "Use generic-lens or generic-optics with 'schemaFacets' instead." #-}

instance Lude.FromJSON BatchGetObjectInformationResponse where
  parseJSON =
    Lude.withObject
      "BatchGetObjectInformationResponse"
      ( \x ->
          BatchGetObjectInformationResponse'
            Lude.<$> (x Lude..:? "ObjectIdentifier")
            Lude.<*> (x Lude..:? "SchemaFacets" Lude..!= Lude.mempty)
      )
