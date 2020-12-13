{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.GetTypedLinkFacetInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the identity attribute order for a specific 'TypedLinkFacet' . For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
module Network.AWS.CloudDirectory.GetTypedLinkFacetInformation
  ( -- * Creating a request
    GetTypedLinkFacetInformation (..),
    mkGetTypedLinkFacetInformation,

    -- ** Request lenses
    gtlfiSchemaARN,
    gtlfiName,

    -- * Destructuring the response
    GetTypedLinkFacetInformationResponse (..),
    mkGetTypedLinkFacetInformationResponse,

    -- ** Response lenses
    gtlfirsIdentityAttributeOrder,
    gtlfirsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetTypedLinkFacetInformation' smart constructor.
data GetTypedLinkFacetInformation = GetTypedLinkFacetInformation'
  { -- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
    schemaARN :: Lude.Text,
    -- | The unique name of the typed link facet.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTypedLinkFacetInformation' with the minimum fields required to make a request.
--
-- * 'schemaARN' - The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
-- * 'name' - The unique name of the typed link facet.
mkGetTypedLinkFacetInformation ::
  -- | 'schemaARN'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  GetTypedLinkFacetInformation
mkGetTypedLinkFacetInformation pSchemaARN_ pName_ =
  GetTypedLinkFacetInformation'
    { schemaARN = pSchemaARN_,
      name = pName_
    }

-- | The Amazon Resource Name (ARN) that is associated with the schema. For more information, see 'arns' .
--
-- /Note:/ Consider using 'schemaARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtlfiSchemaARN :: Lens.Lens' GetTypedLinkFacetInformation Lude.Text
gtlfiSchemaARN = Lens.lens (schemaARN :: GetTypedLinkFacetInformation -> Lude.Text) (\s a -> s {schemaARN = a} :: GetTypedLinkFacetInformation)
{-# DEPRECATED gtlfiSchemaARN "Use generic-lens or generic-optics with 'schemaARN' instead." #-}

-- | The unique name of the typed link facet.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtlfiName :: Lens.Lens' GetTypedLinkFacetInformation Lude.Text
gtlfiName = Lens.lens (name :: GetTypedLinkFacetInformation -> Lude.Text) (\s a -> s {name = a} :: GetTypedLinkFacetInformation)
{-# DEPRECATED gtlfiName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest GetTypedLinkFacetInformation where
  type
    Rs GetTypedLinkFacetInformation =
      GetTypedLinkFacetInformationResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTypedLinkFacetInformationResponse'
            Lude.<$> (x Lude..?> "IdentityAttributeOrder" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTypedLinkFacetInformation where
  toHeaders GetTypedLinkFacetInformation' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# schemaARN]

instance Lude.ToJSON GetTypedLinkFacetInformation where
  toJSON GetTypedLinkFacetInformation' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath GetTypedLinkFacetInformation where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/typedlink/facet/get"

instance Lude.ToQuery GetTypedLinkFacetInformation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTypedLinkFacetInformationResponse' smart constructor.
data GetTypedLinkFacetInformationResponse = GetTypedLinkFacetInformationResponse'
  { -- | The order of identity attributes for the facet, from most significant to least significant. The ability to filter typed links considers the order that the attributes are defined on the typed link facet. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range. Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls. For more information about identity attributes, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
    identityAttributeOrder :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTypedLinkFacetInformationResponse' with the minimum fields required to make a request.
--
-- * 'identityAttributeOrder' - The order of identity attributes for the facet, from most significant to least significant. The ability to filter typed links considers the order that the attributes are defined on the typed link facet. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range. Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls. For more information about identity attributes, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
-- * 'responseStatus' - The response status code.
mkGetTypedLinkFacetInformationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTypedLinkFacetInformationResponse
mkGetTypedLinkFacetInformationResponse pResponseStatus_ =
  GetTypedLinkFacetInformationResponse'
    { identityAttributeOrder =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The order of identity attributes for the facet, from most significant to least significant. The ability to filter typed links considers the order that the attributes are defined on the typed link facet. When providing ranges to typed link selection, any inexact ranges must be specified at the end. Any attributes that do not have a range specified are presumed to match the entire range. Filters are interpreted in the order of the attributes on the typed link facet, not the order in which they are supplied to any API calls. For more information about identity attributes, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'identityAttributeOrder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtlfirsIdentityAttributeOrder :: Lens.Lens' GetTypedLinkFacetInformationResponse (Lude.Maybe [Lude.Text])
gtlfirsIdentityAttributeOrder = Lens.lens (identityAttributeOrder :: GetTypedLinkFacetInformationResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {identityAttributeOrder = a} :: GetTypedLinkFacetInformationResponse)
{-# DEPRECATED gtlfirsIdentityAttributeOrder "Use generic-lens or generic-optics with 'identityAttributeOrder' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtlfirsResponseStatus :: Lens.Lens' GetTypedLinkFacetInformationResponse Lude.Int
gtlfirsResponseStatus = Lens.lens (responseStatus :: GetTypedLinkFacetInformationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTypedLinkFacetInformationResponse)
{-# DEPRECATED gtlfirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
