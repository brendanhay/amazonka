{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.GetObjectInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata about an object.
module Network.AWS.CloudDirectory.GetObjectInformation
  ( -- * Creating a request
    GetObjectInformation (..),
    mkGetObjectInformation,

    -- ** Request lenses
    goiConsistencyLevel,
    goiDirectoryARN,
    goiObjectReference,

    -- * Destructuring the response
    GetObjectInformationResponse (..),
    mkGetObjectInformationResponse,

    -- ** Response lenses
    goirsObjectIdentifier,
    goirsSchemaFacets,
    goirsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetObjectInformation' smart constructor.
data GetObjectInformation = GetObjectInformation'
  { consistencyLevel ::
      Lude.Maybe ConsistencyLevel,
    directoryARN :: Lude.Text,
    objectReference :: ObjectReference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetObjectInformation' with the minimum fields required to make a request.
--
-- * 'consistencyLevel' - The consistency level at which to retrieve the object information.
-- * 'directoryARN' - The ARN of the directory being retrieved.
-- * 'objectReference' - A reference to the object.
mkGetObjectInformation ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'objectReference'
  ObjectReference ->
  GetObjectInformation
mkGetObjectInformation pDirectoryARN_ pObjectReference_ =
  GetObjectInformation'
    { consistencyLevel = Lude.Nothing,
      directoryARN = pDirectoryARN_,
      objectReference = pObjectReference_
    }

-- | The consistency level at which to retrieve the object information.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goiConsistencyLevel :: Lens.Lens' GetObjectInformation (Lude.Maybe ConsistencyLevel)
goiConsistencyLevel = Lens.lens (consistencyLevel :: GetObjectInformation -> Lude.Maybe ConsistencyLevel) (\s a -> s {consistencyLevel = a} :: GetObjectInformation)
{-# DEPRECATED goiConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | The ARN of the directory being retrieved.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goiDirectoryARN :: Lens.Lens' GetObjectInformation Lude.Text
goiDirectoryARN = Lens.lens (directoryARN :: GetObjectInformation -> Lude.Text) (\s a -> s {directoryARN = a} :: GetObjectInformation)
{-# DEPRECATED goiDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | A reference to the object.
--
-- /Note:/ Consider using 'objectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goiObjectReference :: Lens.Lens' GetObjectInformation ObjectReference
goiObjectReference = Lens.lens (objectReference :: GetObjectInformation -> ObjectReference) (\s a -> s {objectReference = a} :: GetObjectInformation)
{-# DEPRECATED goiObjectReference "Use generic-lens or generic-optics with 'objectReference' instead." #-}

instance Lude.AWSRequest GetObjectInformation where
  type Rs GetObjectInformation = GetObjectInformationResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetObjectInformationResponse'
            Lude.<$> (x Lude..?> "ObjectIdentifier")
            Lude.<*> (x Lude..?> "SchemaFacets" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetObjectInformation where
  toHeaders GetObjectInformation' {..} =
    Lude.mconcat
      [ "x-amz-consistency-level" Lude.=# consistencyLevel,
        "x-amz-data-partition" Lude.=# directoryARN
      ]

instance Lude.ToJSON GetObjectInformation where
  toJSON GetObjectInformation' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("ObjectReference" Lude..= objectReference)]
      )

instance Lude.ToPath GetObjectInformation where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/object/information"

instance Lude.ToQuery GetObjectInformation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetObjectInformationResponse' smart constructor.
data GetObjectInformationResponse = GetObjectInformationResponse'
  { objectIdentifier ::
      Lude.Maybe Lude.Text,
    schemaFacets ::
      Lude.Maybe [SchemaFacet],
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetObjectInformationResponse' with the minimum fields required to make a request.
--
-- * 'objectIdentifier' - The @ObjectIdentifier@ of the specified object.
-- * 'responseStatus' - The response status code.
-- * 'schemaFacets' - The facets attached to the specified object. Although the response does not include minor version information, the most recently applied minor version of each Facet is in effect. See 'GetAppliedSchemaVersion' for details.
mkGetObjectInformationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetObjectInformationResponse
mkGetObjectInformationResponse pResponseStatus_ =
  GetObjectInformationResponse'
    { objectIdentifier = Lude.Nothing,
      schemaFacets = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ObjectIdentifier@ of the specified object.
--
-- /Note:/ Consider using 'objectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goirsObjectIdentifier :: Lens.Lens' GetObjectInformationResponse (Lude.Maybe Lude.Text)
goirsObjectIdentifier = Lens.lens (objectIdentifier :: GetObjectInformationResponse -> Lude.Maybe Lude.Text) (\s a -> s {objectIdentifier = a} :: GetObjectInformationResponse)
{-# DEPRECATED goirsObjectIdentifier "Use generic-lens or generic-optics with 'objectIdentifier' instead." #-}

-- | The facets attached to the specified object. Although the response does not include minor version information, the most recently applied minor version of each Facet is in effect. See 'GetAppliedSchemaVersion' for details.
--
-- /Note:/ Consider using 'schemaFacets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goirsSchemaFacets :: Lens.Lens' GetObjectInformationResponse (Lude.Maybe [SchemaFacet])
goirsSchemaFacets = Lens.lens (schemaFacets :: GetObjectInformationResponse -> Lude.Maybe [SchemaFacet]) (\s a -> s {schemaFacets = a} :: GetObjectInformationResponse)
{-# DEPRECATED goirsSchemaFacets "Use generic-lens or generic-optics with 'schemaFacets' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
goirsResponseStatus :: Lens.Lens' GetObjectInformationResponse Lude.Int
goirsResponseStatus = Lens.lens (responseStatus :: GetObjectInformationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetObjectInformationResponse)
{-# DEPRECATED goirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
