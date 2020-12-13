{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.GetFacet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details of the 'Facet' , such as facet name, attributes, 'Rule' s, or @ObjectType@ . You can call this on all kinds of schema facets -- published, development, or applied.
module Network.AWS.CloudDirectory.GetFacet
  ( -- * Creating a request
    GetFacet (..),
    mkGetFacet,

    -- ** Request lenses
    gfSchemaARN,
    gfName,

    -- * Destructuring the response
    GetFacetResponse (..),
    mkGetFacetResponse,

    -- ** Response lenses
    gfrsFacet,
    gfrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFacet' smart constructor.
data GetFacet = GetFacet'
  { -- | The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
    schemaARN :: Lude.Text,
    -- | The name of the facet to retrieve.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFacet' with the minimum fields required to make a request.
--
-- * 'schemaARN' - The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
-- * 'name' - The name of the facet to retrieve.
mkGetFacet ::
  -- | 'schemaARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  GetFacet
mkGetFacet pSchemaARN_ pName_ =
  GetFacet' {schemaARN = pSchemaARN_, name = pName_}

-- | The Amazon Resource Name (ARN) that is associated with the 'Facet' . For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfSchemaARN :: Lens.Lens' GetFacet Lude.Text
gfSchemaARN = Lens.lens (schemaARN :: GetFacet -> Lude.Text) (\s a -> s {schemaARN = a} :: GetFacet)
{-# DEPRECATED gfSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The name of the facet to retrieve.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfName :: Lens.Lens' GetFacet Lude.Text
gfName = Lens.lens (name :: GetFacet -> Lude.Text) (\s a -> s {name = a} :: GetFacet)
{-# DEPRECATED gfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetFacet where
  type Rs GetFacet = GetFacetResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFacetResponse'
            Lude.<$> (x Lude..?> "Facet") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFacet where
  toHeaders GetFacet' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# schemaARN]

instance Lude.ToJSON GetFacet where
  toJSON GetFacet' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath GetFacet where
  toPath = Lude.const "/amazonclouddirectory/2017-01-11/facet"

instance Lude.ToQuery GetFacet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFacetResponse' smart constructor.
data GetFacetResponse = GetFacetResponse'
  { -- | The 'Facet' structure that is associated with the facet.
    facet :: Lude.Maybe Facet,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFacetResponse' with the minimum fields required to make a request.
--
-- * 'facet' - The 'Facet' structure that is associated with the facet.
-- * 'responseStatus' - The response status code.
mkGetFacetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetFacetResponse
mkGetFacetResponse pResponseStatus_ =
  GetFacetResponse'
    { facet = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The 'Facet' structure that is associated with the facet.
--
-- /Note:/ Consider using 'facet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsFacet :: Lens.Lens' GetFacetResponse (Lude.Maybe Facet)
gfrsFacet = Lens.lens (facet :: GetFacetResponse -> Lude.Maybe Facet) (\s a -> s {facet = a} :: GetFacetResponse)
{-# DEPRECATED gfrsFacet "Use generic-lens or generic-optics with 'facet' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfrsResponseStatus :: Lens.Lens' GetFacetResponse Lude.Int
gfrsResponseStatus = Lens.lens (responseStatus :: GetFacetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFacetResponse)
{-# DEPRECATED gfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
