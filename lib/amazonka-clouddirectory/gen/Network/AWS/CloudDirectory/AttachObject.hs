{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.AttachObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches an existing object to another object. An object can be accessed in two ways:
--
--
--     * Using the path
--
--
--     * Using @ObjectIdentifier@
module Network.AWS.CloudDirectory.AttachObject
  ( -- * Creating a request
    AttachObject (..),
    mkAttachObject,

    -- ** Request lenses
    aoDirectoryARN,
    aoParentReference,
    aoChildReference,
    aoLinkName,

    -- * Destructuring the response
    AttachObjectResponse (..),
    mkAttachObjectResponse,

    -- ** Response lenses
    aorsAttachedObjectIdentifier,
    aorsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachObject' smart constructor.
data AttachObject = AttachObject'
  { -- | Amazon Resource Name (ARN) that is associated with the 'Directory' where both objects reside. For more information, see 'arns' .
    directoryARN :: Lude.Text,
    -- | The parent object reference.
    parentReference :: ObjectReference,
    -- | The child object reference to be attached to the object.
    childReference :: ObjectReference,
    -- | The link name with which the child object is attached to the parent.
    linkName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachObject' with the minimum fields required to make a request.
--
-- * 'directoryARN' - Amazon Resource Name (ARN) that is associated with the 'Directory' where both objects reside. For more information, see 'arns' .
-- * 'parentReference' - The parent object reference.
-- * 'childReference' - The child object reference to be attached to the object.
-- * 'linkName' - The link name with which the child object is attached to the parent.
mkAttachObject ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'parentReference'
  ObjectReference ->
  -- | 'childReference'
  ObjectReference ->
  -- | 'linkName'
  Lude.Text ->
  AttachObject
mkAttachObject
  pDirectoryARN_
  pParentReference_
  pChildReference_
  pLinkName_ =
    AttachObject'
      { directoryARN = pDirectoryARN_,
        parentReference = pParentReference_,
        childReference = pChildReference_,
        linkName = pLinkName_
      }

-- | Amazon Resource Name (ARN) that is associated with the 'Directory' where both objects reside. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoDirectoryARN :: Lens.Lens' AttachObject Lude.Text
aoDirectoryARN = Lens.lens (directoryARN :: AttachObject -> Lude.Text) (\s a -> s {directoryARN = a} :: AttachObject)
{-# DEPRECATED aoDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | The parent object reference.
--
-- /Note:/ Consider using 'parentReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoParentReference :: Lens.Lens' AttachObject ObjectReference
aoParentReference = Lens.lens (parentReference :: AttachObject -> ObjectReference) (\s a -> s {parentReference = a} :: AttachObject)
{-# DEPRECATED aoParentReference "Use generic-lens or generic-optics with 'parentReference' instead." #-}

-- | The child object reference to be attached to the object.
--
-- /Note:/ Consider using 'childReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoChildReference :: Lens.Lens' AttachObject ObjectReference
aoChildReference = Lens.lens (childReference :: AttachObject -> ObjectReference) (\s a -> s {childReference = a} :: AttachObject)
{-# DEPRECATED aoChildReference "Use generic-lens or generic-optics with 'childReference' instead." #-}

-- | The link name with which the child object is attached to the parent.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aoLinkName :: Lens.Lens' AttachObject Lude.Text
aoLinkName = Lens.lens (linkName :: AttachObject -> Lude.Text) (\s a -> s {linkName = a} :: AttachObject)
{-# DEPRECATED aoLinkName "Use generic-lens or generic-optics with 'linkName' instead." #-}

instance Lude.AWSRequest AttachObject where
  type Rs AttachObject = AttachObjectResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          AttachObjectResponse'
            Lude.<$> (x Lude..?> "AttachedObjectIdentifier")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AttachObject where
  toHeaders AttachObject' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON AttachObject where
  toJSON AttachObject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ParentReference" Lude..= parentReference),
            Lude.Just ("ChildReference" Lude..= childReference),
            Lude.Just ("LinkName" Lude..= linkName)
          ]
      )

instance Lude.ToPath AttachObject where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/object/attach"

instance Lude.ToQuery AttachObject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAttachObjectResponse' smart constructor.
data AttachObjectResponse = AttachObjectResponse'
  { -- | The attached @ObjectIdentifier@ , which is the child @ObjectIdentifier@ .
    attachedObjectIdentifier :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachObjectResponse' with the minimum fields required to make a request.
--
-- * 'attachedObjectIdentifier' - The attached @ObjectIdentifier@ , which is the child @ObjectIdentifier@ .
-- * 'responseStatus' - The response status code.
mkAttachObjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AttachObjectResponse
mkAttachObjectResponse pResponseStatus_ =
  AttachObjectResponse'
    { attachedObjectIdentifier = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The attached @ObjectIdentifier@ , which is the child @ObjectIdentifier@ .
--
-- /Note:/ Consider using 'attachedObjectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aorsAttachedObjectIdentifier :: Lens.Lens' AttachObjectResponse (Lude.Maybe Lude.Text)
aorsAttachedObjectIdentifier = Lens.lens (attachedObjectIdentifier :: AttachObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {attachedObjectIdentifier = a} :: AttachObjectResponse)
{-# DEPRECATED aorsAttachedObjectIdentifier "Use generic-lens or generic-optics with 'attachedObjectIdentifier' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aorsResponseStatus :: Lens.Lens' AttachObjectResponse Lude.Int
aorsResponseStatus = Lens.lens (responseStatus :: AttachObjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AttachObjectResponse)
{-# DEPRECATED aorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
