{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.AttachTypedLink
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches a typed link to a specified source and target object. For more information, see <https://docs.aws.amazon.com/clouddirectory/latest/developerguide/directory_objects_links.html#directory_objects_links_typedlink Typed Links> .
module Network.AWS.CloudDirectory.AttachTypedLink
  ( -- * Creating a request
    AttachTypedLink (..),
    mkAttachTypedLink,

    -- ** Request lenses
    atlDirectoryARN,
    atlSourceObjectReference,
    atlTargetObjectReference,
    atlTypedLinkFacet,
    atlAttributes,

    -- * Destructuring the response
    AttachTypedLinkResponse (..),
    mkAttachTypedLinkResponse,

    -- ** Response lenses
    atlrsTypedLinkSpecifier,
    atlrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachTypedLink' smart constructor.
data AttachTypedLink = AttachTypedLink'
  { directoryARN :: Lude.Text,
    sourceObjectReference :: ObjectReference,
    targetObjectReference :: ObjectReference,
    typedLinkFacet :: TypedLinkSchemaAndFacetName,
    attributes :: [AttributeNameAndValue]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachTypedLink' with the minimum fields required to make a request.
--
-- * 'attributes' - A set of attributes that are associated with the typed link.
-- * 'directoryARN' - The Amazon Resource Name (ARN) of the directory where you want to attach the typed link.
-- * 'sourceObjectReference' - Identifies the source object that the typed link will attach to.
-- * 'targetObjectReference' - Identifies the target object that the typed link will attach to.
-- * 'typedLinkFacet' - Identifies the typed link facet that is associated with the typed link.
mkAttachTypedLink ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'sourceObjectReference'
  ObjectReference ->
  -- | 'targetObjectReference'
  ObjectReference ->
  -- | 'typedLinkFacet'
  TypedLinkSchemaAndFacetName ->
  AttachTypedLink
mkAttachTypedLink
  pDirectoryARN_
  pSourceObjectReference_
  pTargetObjectReference_
  pTypedLinkFacet_ =
    AttachTypedLink'
      { directoryARN = pDirectoryARN_,
        sourceObjectReference = pSourceObjectReference_,
        targetObjectReference = pTargetObjectReference_,
        typedLinkFacet = pTypedLinkFacet_,
        attributes = Lude.mempty
      }

-- | The Amazon Resource Name (ARN) of the directory where you want to attach the typed link.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atlDirectoryARN :: Lens.Lens' AttachTypedLink Lude.Text
atlDirectoryARN = Lens.lens (directoryARN :: AttachTypedLink -> Lude.Text) (\s a -> s {directoryARN = a} :: AttachTypedLink)
{-# DEPRECATED atlDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | Identifies the source object that the typed link will attach to.
--
-- /Note:/ Consider using 'sourceObjectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atlSourceObjectReference :: Lens.Lens' AttachTypedLink ObjectReference
atlSourceObjectReference = Lens.lens (sourceObjectReference :: AttachTypedLink -> ObjectReference) (\s a -> s {sourceObjectReference = a} :: AttachTypedLink)
{-# DEPRECATED atlSourceObjectReference "Use generic-lens or generic-optics with 'sourceObjectReference' instead." #-}

-- | Identifies the target object that the typed link will attach to.
--
-- /Note:/ Consider using 'targetObjectReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atlTargetObjectReference :: Lens.Lens' AttachTypedLink ObjectReference
atlTargetObjectReference = Lens.lens (targetObjectReference :: AttachTypedLink -> ObjectReference) (\s a -> s {targetObjectReference = a} :: AttachTypedLink)
{-# DEPRECATED atlTargetObjectReference "Use generic-lens or generic-optics with 'targetObjectReference' instead." #-}

-- | Identifies the typed link facet that is associated with the typed link.
--
-- /Note:/ Consider using 'typedLinkFacet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atlTypedLinkFacet :: Lens.Lens' AttachTypedLink TypedLinkSchemaAndFacetName
atlTypedLinkFacet = Lens.lens (typedLinkFacet :: AttachTypedLink -> TypedLinkSchemaAndFacetName) (\s a -> s {typedLinkFacet = a} :: AttachTypedLink)
{-# DEPRECATED atlTypedLinkFacet "Use generic-lens or generic-optics with 'typedLinkFacet' instead." #-}

-- | A set of attributes that are associated with the typed link.
--
-- /Note:/ Consider using 'attributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atlAttributes :: Lens.Lens' AttachTypedLink [AttributeNameAndValue]
atlAttributes = Lens.lens (attributes :: AttachTypedLink -> [AttributeNameAndValue]) (\s a -> s {attributes = a} :: AttachTypedLink)
{-# DEPRECATED atlAttributes "Use generic-lens or generic-optics with 'attributes' instead." #-}

instance Lude.AWSRequest AttachTypedLink where
  type Rs AttachTypedLink = AttachTypedLinkResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          AttachTypedLinkResponse'
            Lude.<$> (x Lude..?> "TypedLinkSpecifier")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AttachTypedLink where
  toHeaders AttachTypedLink' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON AttachTypedLink where
  toJSON AttachTypedLink' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("SourceObjectReference" Lude..= sourceObjectReference),
            Lude.Just ("TargetObjectReference" Lude..= targetObjectReference),
            Lude.Just ("TypedLinkFacet" Lude..= typedLinkFacet),
            Lude.Just ("Attributes" Lude..= attributes)
          ]
      )

instance Lude.ToPath AttachTypedLink where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/typedlink/attach"

instance Lude.ToQuery AttachTypedLink where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAttachTypedLinkResponse' smart constructor.
data AttachTypedLinkResponse = AttachTypedLinkResponse'
  { typedLinkSpecifier ::
      Lude.Maybe TypedLinkSpecifier,
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

-- | Creates a value of 'AttachTypedLinkResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'typedLinkSpecifier' - Returns a typed link specifier as output.
mkAttachTypedLinkResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AttachTypedLinkResponse
mkAttachTypedLinkResponse pResponseStatus_ =
  AttachTypedLinkResponse'
    { typedLinkSpecifier = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns a typed link specifier as output.
--
-- /Note:/ Consider using 'typedLinkSpecifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atlrsTypedLinkSpecifier :: Lens.Lens' AttachTypedLinkResponse (Lude.Maybe TypedLinkSpecifier)
atlrsTypedLinkSpecifier = Lens.lens (typedLinkSpecifier :: AttachTypedLinkResponse -> Lude.Maybe TypedLinkSpecifier) (\s a -> s {typedLinkSpecifier = a} :: AttachTypedLinkResponse)
{-# DEPRECATED atlrsTypedLinkSpecifier "Use generic-lens or generic-optics with 'typedLinkSpecifier' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atlrsResponseStatus :: Lens.Lens' AttachTypedLinkResponse Lude.Int
atlrsResponseStatus = Lens.lens (responseStatus :: AttachTypedLinkResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AttachTypedLinkResponse)
{-# DEPRECATED atlrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
