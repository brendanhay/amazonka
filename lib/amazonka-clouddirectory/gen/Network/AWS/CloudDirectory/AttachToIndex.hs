{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.AttachToIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches the specified object to the specified index.
module Network.AWS.CloudDirectory.AttachToIndex
  ( -- * Creating a request
    AttachToIndex (..),
    mkAttachToIndex,

    -- ** Request lenses
    atiDirectoryARN,
    atiTargetReference,
    atiIndexReference,

    -- * Destructuring the response
    AttachToIndexResponse (..),
    mkAttachToIndexResponse,

    -- ** Response lenses
    atirsAttachedObjectIdentifier,
    atirsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAttachToIndex' smart constructor.
data AttachToIndex = AttachToIndex'
  { -- | The Amazon Resource Name (ARN) of the directory where the object and index exist.
    directoryARN :: Lude.Text,
    -- | A reference to the object that you are attaching to the index.
    targetReference :: ObjectReference,
    -- | A reference to the index that you are attaching the object to.
    indexReference :: ObjectReference
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachToIndex' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) of the directory where the object and index exist.
-- * 'targetReference' - A reference to the object that you are attaching to the index.
-- * 'indexReference' - A reference to the index that you are attaching the object to.
mkAttachToIndex ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'targetReference'
  ObjectReference ->
  -- | 'indexReference'
  ObjectReference ->
  AttachToIndex
mkAttachToIndex pDirectoryARN_ pTargetReference_ pIndexReference_ =
  AttachToIndex'
    { directoryARN = pDirectoryARN_,
      targetReference = pTargetReference_,
      indexReference = pIndexReference_
    }

-- | The Amazon Resource Name (ARN) of the directory where the object and index exist.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiDirectoryARN :: Lens.Lens' AttachToIndex Lude.Text
atiDirectoryARN = Lens.lens (directoryARN :: AttachToIndex -> Lude.Text) (\s a -> s {directoryARN = a} :: AttachToIndex)
{-# DEPRECATED atiDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | A reference to the object that you are attaching to the index.
--
-- /Note:/ Consider using 'targetReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiTargetReference :: Lens.Lens' AttachToIndex ObjectReference
atiTargetReference = Lens.lens (targetReference :: AttachToIndex -> ObjectReference) (\s a -> s {targetReference = a} :: AttachToIndex)
{-# DEPRECATED atiTargetReference "Use generic-lens or generic-optics with 'targetReference' instead." #-}

-- | A reference to the index that you are attaching the object to.
--
-- /Note:/ Consider using 'indexReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atiIndexReference :: Lens.Lens' AttachToIndex ObjectReference
atiIndexReference = Lens.lens (indexReference :: AttachToIndex -> ObjectReference) (\s a -> s {indexReference = a} :: AttachToIndex)
{-# DEPRECATED atiIndexReference "Use generic-lens or generic-optics with 'indexReference' instead." #-}

instance Lude.AWSRequest AttachToIndex where
  type Rs AttachToIndex = AttachToIndexResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          AttachToIndexResponse'
            Lude.<$> (x Lude..?> "AttachedObjectIdentifier")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AttachToIndex where
  toHeaders AttachToIndex' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON AttachToIndex where
  toJSON AttachToIndex' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TargetReference" Lude..= targetReference),
            Lude.Just ("IndexReference" Lude..= indexReference)
          ]
      )

instance Lude.ToPath AttachToIndex where
  toPath = Lude.const "/amazonclouddirectory/2017-01-11/index/attach"

instance Lude.ToQuery AttachToIndex where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAttachToIndexResponse' smart constructor.
data AttachToIndexResponse = AttachToIndexResponse'
  { -- | The @ObjectIdentifier@ of the object that was attached to the index.
    attachedObjectIdentifier :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AttachToIndexResponse' with the minimum fields required to make a request.
--
-- * 'attachedObjectIdentifier' - The @ObjectIdentifier@ of the object that was attached to the index.
-- * 'responseStatus' - The response status code.
mkAttachToIndexResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AttachToIndexResponse
mkAttachToIndexResponse pResponseStatus_ =
  AttachToIndexResponse'
    { attachedObjectIdentifier = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ObjectIdentifier@ of the object that was attached to the index.
--
-- /Note:/ Consider using 'attachedObjectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atirsAttachedObjectIdentifier :: Lens.Lens' AttachToIndexResponse (Lude.Maybe Lude.Text)
atirsAttachedObjectIdentifier = Lens.lens (attachedObjectIdentifier :: AttachToIndexResponse -> Lude.Maybe Lude.Text) (\s a -> s {attachedObjectIdentifier = a} :: AttachToIndexResponse)
{-# DEPRECATED atirsAttachedObjectIdentifier "Use generic-lens or generic-optics with 'attachedObjectIdentifier' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atirsResponseStatus :: Lens.Lens' AttachToIndexResponse Lude.Int
atirsResponseStatus = Lens.lens (responseStatus :: AttachToIndexResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AttachToIndexResponse)
{-# DEPRECATED atirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
