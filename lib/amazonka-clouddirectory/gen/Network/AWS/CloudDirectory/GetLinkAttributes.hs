{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.GetLinkAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves attributes that are associated with a typed link.
module Network.AWS.CloudDirectory.GetLinkAttributes
  ( -- * Creating a request
    GetLinkAttributes (..),
    mkGetLinkAttributes,

    -- ** Request lenses
    glaConsistencyLevel,
    glaDirectoryARN,
    glaTypedLinkSpecifier,
    glaAttributeNames,

    -- * Destructuring the response
    GetLinkAttributesResponse (..),
    mkGetLinkAttributesResponse,

    -- ** Response lenses
    glarsAttributes,
    glarsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetLinkAttributes' smart constructor.
data GetLinkAttributes = GetLinkAttributes'
  { consistencyLevel ::
      Lude.Maybe ConsistencyLevel,
    directoryARN :: Lude.Text,
    typedLinkSpecifier :: TypedLinkSpecifier,
    attributeNames :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetLinkAttributes' with the minimum fields required to make a request.
--
-- * 'attributeNames' - A list of attribute names whose values will be retrieved.
-- * 'consistencyLevel' - The consistency level at which to retrieve the attributes on a typed link.
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the Directory where the typed link resides. For more information, see 'arns' or <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
-- * 'typedLinkSpecifier' - Allows a typed link specifier to be accepted as input.
mkGetLinkAttributes ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'typedLinkSpecifier'
  TypedLinkSpecifier ->
  GetLinkAttributes
mkGetLinkAttributes pDirectoryARN_ pTypedLinkSpecifier_ =
  GetLinkAttributes'
    { consistencyLevel = Lude.Nothing,
      directoryARN = pDirectoryARN_,
      typedLinkSpecifier = pTypedLinkSpecifier_,
      attributeNames = Lude.mempty
    }

-- | The consistency level at which to retrieve the attributes on a typed link.
--
-- /Note:/ Consider using 'consistencyLevel' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glaConsistencyLevel :: Lens.Lens' GetLinkAttributes (Lude.Maybe ConsistencyLevel)
glaConsistencyLevel = Lens.lens (consistencyLevel :: GetLinkAttributes -> Lude.Maybe ConsistencyLevel) (\s a -> s {consistencyLevel = a} :: GetLinkAttributes)
{-# DEPRECATED glaConsistencyLevel "Use generic-lens or generic-optics with 'consistencyLevel' instead." #-}

-- | The Amazon Resource Name (ARN) that is associated with the Directory where the typed link resides. For more information, see 'arns' or <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glaDirectoryARN :: Lens.Lens' GetLinkAttributes Lude.Text
glaDirectoryARN = Lens.lens (directoryARN :: GetLinkAttributes -> Lude.Text) (\s a -> s {directoryARN = a} :: GetLinkAttributes)
{-# DEPRECATED glaDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | Allows a typed link specifier to be accepted as input.
--
-- /Note:/ Consider using 'typedLinkSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glaTypedLinkSpecifier :: Lens.Lens' GetLinkAttributes TypedLinkSpecifier
glaTypedLinkSpecifier = Lens.lens (typedLinkSpecifier :: GetLinkAttributes -> TypedLinkSpecifier) (\s a -> s {typedLinkSpecifier = a} :: GetLinkAttributes)
{-# DEPRECATED glaTypedLinkSpecifier "Use generic-lens or generic-optics with 'typedLinkSpecifier' instead." #-}

-- | A list of attribute names whose values will be retrieved.
--
-- /Note:/ Consider using 'attributeNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glaAttributeNames :: Lens.Lens' GetLinkAttributes [Lude.Text]
glaAttributeNames = Lens.lens (attributeNames :: GetLinkAttributes -> [Lude.Text]) (\s a -> s {attributeNames = a} :: GetLinkAttributes)
{-# DEPRECATED glaAttributeNames "Use generic-lens or generic-optics with 'attributeNames' instead." #-}

instance Lude.AWSRequest GetLinkAttributes where
  type Rs GetLinkAttributes = GetLinkAttributesResponse
  request = Req.postJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetLinkAttributesResponse'
            Lude.<$> (x Lude..?> "Attributes" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetLinkAttributes where
  toHeaders GetLinkAttributes' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON GetLinkAttributes where
  toJSON GetLinkAttributes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ConsistencyLevel" Lude..=) Lude.<$> consistencyLevel,
            Lude.Just ("TypedLinkSpecifier" Lude..= typedLinkSpecifier),
            Lude.Just ("AttributeNames" Lude..= attributeNames)
          ]
      )

instance Lude.ToPath GetLinkAttributes where
  toPath =
    Lude.const
      "/amazonclouddirectory/2017-01-11/typedlink/attributes/get"

instance Lude.ToQuery GetLinkAttributes where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetLinkAttributesResponse' smart constructor.
data GetLinkAttributesResponse = GetLinkAttributesResponse'
  { attributes ::
      Lude.Maybe [AttributeKeyAndValue],
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

-- | Creates a value of 'GetLinkAttributesResponse' with the minimum fields required to make a request.
--
-- * 'attributes' - The attributes that are associated with the typed link.
-- * 'responseStatus' - The response status code.
mkGetLinkAttributesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetLinkAttributesResponse
mkGetLinkAttributesResponse pResponseStatus_ =
  GetLinkAttributesResponse'
    { attributes = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The attributes that are associated with the typed link.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glarsAttributes :: Lens.Lens' GetLinkAttributesResponse (Lude.Maybe [AttributeKeyAndValue])
glarsAttributes = Lens.lens (attributes :: GetLinkAttributesResponse -> Lude.Maybe [AttributeKeyAndValue]) (\s a -> s {attributes = a} :: GetLinkAttributesResponse)
{-# DEPRECATED glarsAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
glarsResponseStatus :: Lens.Lens' GetLinkAttributesResponse Lude.Int
glarsResponseStatus = Lens.lens (responseStatus :: GetLinkAttributesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetLinkAttributesResponse)
{-# DEPRECATED glarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
