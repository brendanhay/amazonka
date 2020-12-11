{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DetachObject
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches a given object from the parent object. The object that is to be detached from the parent is specified by the link name.
module Network.AWS.CloudDirectory.DetachObject
  ( -- * Creating a request
    DetachObject (..),
    mkDetachObject,

    -- ** Request lenses
    detDirectoryARN,
    detParentReference,
    detLinkName,

    -- * Destructuring the response
    DetachObjectResponse (..),
    mkDetachObjectResponse,

    -- ** Response lenses
    detrsDetachedObjectIdentifier,
    detrsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachObject' smart constructor.
data DetachObject = DetachObject'
  { directoryARN :: Lude.Text,
    parentReference :: ObjectReference,
    linkName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachObject' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
-- * 'linkName' - The link name associated with the object that needs to be detached.
-- * 'parentReference' - The parent reference from which the object with the specified link name is detached.
mkDetachObject ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'parentReference'
  ObjectReference ->
  -- | 'linkName'
  Lude.Text ->
  DetachObject
mkDetachObject pDirectoryARN_ pParentReference_ pLinkName_ =
  DetachObject'
    { directoryARN = pDirectoryARN_,
      parentReference = pParentReference_,
      linkName = pLinkName_
    }

-- | The Amazon Resource Name (ARN) that is associated with the 'Directory' where objects reside. For more information, see 'arns' .
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detDirectoryARN :: Lens.Lens' DetachObject Lude.Text
detDirectoryARN = Lens.lens (directoryARN :: DetachObject -> Lude.Text) (\s a -> s {directoryARN = a} :: DetachObject)
{-# DEPRECATED detDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | The parent reference from which the object with the specified link name is detached.
--
-- /Note:/ Consider using 'parentReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detParentReference :: Lens.Lens' DetachObject ObjectReference
detParentReference = Lens.lens (parentReference :: DetachObject -> ObjectReference) (\s a -> s {parentReference = a} :: DetachObject)
{-# DEPRECATED detParentReference "Use generic-lens or generic-optics with 'parentReference' instead." #-}

-- | The link name associated with the object that needs to be detached.
--
-- /Note:/ Consider using 'linkName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detLinkName :: Lens.Lens' DetachObject Lude.Text
detLinkName = Lens.lens (linkName :: DetachObject -> Lude.Text) (\s a -> s {linkName = a} :: DetachObject)
{-# DEPRECATED detLinkName "Use generic-lens or generic-optics with 'linkName' instead." #-}

instance Lude.AWSRequest DetachObject where
  type Rs DetachObject = DetachObjectResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetachObjectResponse'
            Lude.<$> (x Lude..?> "DetachedObjectIdentifier")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetachObject where
  toHeaders DetachObject' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON DetachObject where
  toJSON DetachObject' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ParentReference" Lude..= parentReference),
            Lude.Just ("LinkName" Lude..= linkName)
          ]
      )

instance Lude.ToPath DetachObject where
  toPath =
    Lude.const "/amazonclouddirectory/2017-01-11/object/detach"

instance Lude.ToQuery DetachObject where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetachObjectResponse' smart constructor.
data DetachObjectResponse = DetachObjectResponse'
  { detachedObjectIdentifier ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DetachObjectResponse' with the minimum fields required to make a request.
--
-- * 'detachedObjectIdentifier' - The @ObjectIdentifier@ that was detached from the object.
-- * 'responseStatus' - The response status code.
mkDetachObjectResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetachObjectResponse
mkDetachObjectResponse pResponseStatus_ =
  DetachObjectResponse'
    { detachedObjectIdentifier = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ObjectIdentifier@ that was detached from the object.
--
-- /Note:/ Consider using 'detachedObjectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsDetachedObjectIdentifier :: Lens.Lens' DetachObjectResponse (Lude.Maybe Lude.Text)
detrsDetachedObjectIdentifier = Lens.lens (detachedObjectIdentifier :: DetachObjectResponse -> Lude.Maybe Lude.Text) (\s a -> s {detachedObjectIdentifier = a} :: DetachObjectResponse)
{-# DEPRECATED detrsDetachedObjectIdentifier "Use generic-lens or generic-optics with 'detachedObjectIdentifier' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsResponseStatus :: Lens.Lens' DetachObjectResponse Lude.Int
detrsResponseStatus = Lens.lens (responseStatus :: DetachObjectResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetachObjectResponse)
{-# DEPRECATED detrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
