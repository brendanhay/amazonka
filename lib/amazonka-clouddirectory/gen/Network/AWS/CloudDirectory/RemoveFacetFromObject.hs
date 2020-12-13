{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.RemoveFacetFromObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the specified facet from the specified object.
module Network.AWS.CloudDirectory.RemoveFacetFromObject
  ( -- * Creating a request
    RemoveFacetFromObject (..),
    mkRemoveFacetFromObject,

    -- ** Request lenses
    rffoDirectoryARN,
    rffoSchemaFacet,
    rffoObjectReference,

    -- * Destructuring the response
    RemoveFacetFromObjectResponse (..),
    mkRemoveFacetFromObjectResponse,

    -- ** Response lenses
    rfforsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveFacetFromObject' smart constructor.
data RemoveFacetFromObject = RemoveFacetFromObject'
  { -- | The ARN of the directory in which the object resides.
    directoryARN :: Lude.Text,
    -- | The facet to remove. See 'SchemaFacet' for details.
    schemaFacet :: SchemaFacet,
    -- | A reference to the object to remove the facet from.
    objectReference :: ObjectReference
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveFacetFromObject' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The ARN of the directory in which the object resides.
-- * 'schemaFacet' - The facet to remove. See 'SchemaFacet' for details.
-- * 'objectReference' - A reference to the object to remove the facet from.
mkRemoveFacetFromObject ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'schemaFacet'
  SchemaFacet ->
  -- | 'objectReference'
  ObjectReference ->
  RemoveFacetFromObject
mkRemoveFacetFromObject
  pDirectoryARN_
  pSchemaFacet_
  pObjectReference_ =
    RemoveFacetFromObject'
      { directoryARN = pDirectoryARN_,
        schemaFacet = pSchemaFacet_,
        objectReference = pObjectReference_
      }

-- | The ARN of the directory in which the object resides.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rffoDirectoryARN :: Lens.Lens' RemoveFacetFromObject Lude.Text
rffoDirectoryARN = Lens.lens (directoryARN :: RemoveFacetFromObject -> Lude.Text) (\s a -> s {directoryARN = a} :: RemoveFacetFromObject)
{-# DEPRECATED rffoDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | The facet to remove. See 'SchemaFacet' for details.
--
-- /Note:/ Consider using 'schemaFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rffoSchemaFacet :: Lens.Lens' RemoveFacetFromObject SchemaFacet
rffoSchemaFacet = Lens.lens (schemaFacet :: RemoveFacetFromObject -> SchemaFacet) (\s a -> s {schemaFacet = a} :: RemoveFacetFromObject)
{-# DEPRECATED rffoSchemaFacet "Use generic-lens or generic-optics with 'schemaFacet' instead." #-}

-- | A reference to the object to remove the facet from.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rffoObjectReference :: Lens.Lens' RemoveFacetFromObject ObjectReference
rffoObjectReference = Lens.lens (objectReference :: RemoveFacetFromObject -> ObjectReference) (\s a -> s {objectReference = a} :: RemoveFacetFromObject)
{-# DEPRECATED rffoObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.AWSRequest RemoveFacetFromObject where
  type Rs RemoveFacetFromObject = RemoveFacetFromObjectResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveEmpty
      ( \s h x ->
          RemoveFacetFromObjectResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RemoveFacetFromObject where
  toHeaders RemoveFacetFromObject' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON RemoveFacetFromObject where
  toJSON RemoveFacetFromObject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SchemaFacet" Lude..= schemaFacet),
            Lude.Just ("ObjectReference" Lude..= objectReference)
          ]
      )

instance Lude.ToPath RemoveFacetFromObject where
  toPath =
    Lude.const
      "/amazonclouddirectory/2017-01-11/object/facets/delete"

instance Lude.ToQuery RemoveFacetFromObject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkRemoveFacetFromObjectResponse' smart constructor.
newtype RemoveFacetFromObjectResponse = RemoveFacetFromObjectResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveFacetFromObjectResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkRemoveFacetFromObjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  RemoveFacetFromObjectResponse
mkRemoveFacetFromObjectResponse pResponseStatus_ =
  RemoveFacetFromObjectResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rfforsResponseStatus :: Lens.Lens' RemoveFacetFromObjectResponse Lude.Int
rfforsResponseStatus = Lens.lens (responseStatus :: RemoveFacetFromObjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RemoveFacetFromObjectResponse)
{-# DEPRECATED rfforsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
