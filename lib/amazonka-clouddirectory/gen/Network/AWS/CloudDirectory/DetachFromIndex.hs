{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudDirectory.DetachFromIndex
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Detaches the specified object from the specified index.
module Network.AWS.CloudDirectory.DetachFromIndex
  ( -- * Creating a request
    DetachFromIndex (..),
    mkDetachFromIndex,

    -- ** Request lenses
    dfiDirectoryARN,
    dfiIndexReference,
    dfiTargetReference,

    -- * Destructuring the response
    DetachFromIndexResponse (..),
    mkDetachFromIndexResponse,

    -- ** Response lenses
    dfirsDetachedObjectIdentifier,
    dfirsResponseStatus,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDetachFromIndex' smart constructor.
data DetachFromIndex = DetachFromIndex'
  { directoryARN :: Lude.Text,
    indexReference :: ObjectReference,
    targetReference :: ObjectReference
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DetachFromIndex' with the minimum fields required to make a request.
--
-- * 'directoryARN' - The Amazon Resource Name (ARN) of the directory the index and object exist in.
-- * 'indexReference' - A reference to the index object.
-- * 'targetReference' - A reference to the object being detached from the index.
mkDetachFromIndex ::
  -- | 'directoryARN'
  Lude.Text ->
  -- | 'indexReference'
  ObjectReference ->
  -- | 'targetReference'
  ObjectReference ->
  DetachFromIndex
mkDetachFromIndex pDirectoryARN_ pIndexReference_ pTargetReference_ =
  DetachFromIndex'
    { directoryARN = pDirectoryARN_,
      indexReference = pIndexReference_,
      targetReference = pTargetReference_
    }

-- | The Amazon Resource Name (ARN) of the directory the index and object exist in.
--
-- /Note:/ Consider using 'directoryARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiDirectoryARN :: Lens.Lens' DetachFromIndex Lude.Text
dfiDirectoryARN = Lens.lens (directoryARN :: DetachFromIndex -> Lude.Text) (\s a -> s {directoryARN = a} :: DetachFromIndex)
{-# DEPRECATED dfiDirectoryARN "Use generic-lens or generic-optics with 'directoryARN' instead." #-}

-- | A reference to the index object.
--
-- /Note:/ Consider using 'indexReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiIndexReference :: Lens.Lens' DetachFromIndex ObjectReference
dfiIndexReference = Lens.lens (indexReference :: DetachFromIndex -> ObjectReference) (\s a -> s {indexReference = a} :: DetachFromIndex)
{-# DEPRECATED dfiIndexReference "Use generic-lens or generic-optics with 'indexReference' instead." #-}

-- | A reference to the object being detached from the index.
--
-- /Note:/ Consider using 'targetReference' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfiTargetReference :: Lens.Lens' DetachFromIndex ObjectReference
dfiTargetReference = Lens.lens (targetReference :: DetachFromIndex -> ObjectReference) (\s a -> s {targetReference = a} :: DetachFromIndex)
{-# DEPRECATED dfiTargetReference "Use generic-lens or generic-optics with 'targetReference' instead." #-}

instance Lude.AWSRequest DetachFromIndex where
  type Rs DetachFromIndex = DetachFromIndexResponse
  request = Req.putJSON cloudDirectoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          DetachFromIndexResponse'
            Lude.<$> (x Lude..?> "DetachedObjectIdentifier")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DetachFromIndex where
  toHeaders DetachFromIndex' {..} =
    Lude.mconcat ["x-amz-data-partition" Lude.=# directoryARN]

instance Lude.ToJSON DetachFromIndex where
  toJSON DetachFromIndex' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("IndexReference" Lude..= indexReference),
            Lude.Just ("TargetReference" Lude..= targetReference)
          ]
      )

instance Lude.ToPath DetachFromIndex where
  toPath = Lude.const "/amazonclouddirectory/2017-01-11/index/detach"

instance Lude.ToQuery DetachFromIndex where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDetachFromIndexResponse' smart constructor.
data DetachFromIndexResponse = DetachFromIndexResponse'
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

-- | Creates a value of 'DetachFromIndexResponse' with the minimum fields required to make a request.
--
-- * 'detachedObjectIdentifier' - The @ObjectIdentifier@ of the object that was detached from the index.
-- * 'responseStatus' - The response status code.
mkDetachFromIndexResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DetachFromIndexResponse
mkDetachFromIndexResponse pResponseStatus_ =
  DetachFromIndexResponse'
    { detachedObjectIdentifier = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The @ObjectIdentifier@ of the object that was detached from the index.
--
-- /Note:/ Consider using 'detachedObjectIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfirsDetachedObjectIdentifier :: Lens.Lens' DetachFromIndexResponse (Lude.Maybe Lude.Text)
dfirsDetachedObjectIdentifier = Lens.lens (detachedObjectIdentifier :: DetachFromIndexResponse -> Lude.Maybe Lude.Text) (\s a -> s {detachedObjectIdentifier = a} :: DetachFromIndexResponse)
{-# DEPRECATED dfirsDetachedObjectIdentifier "Use generic-lens or generic-optics with 'detachedObjectIdentifier' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfirsResponseStatus :: Lens.Lens' DetachFromIndexResponse Lude.Int
dfirsResponseStatus = Lens.lens (responseStatus :: DetachFromIndexResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DetachFromIndexResponse)
{-# DEPRECATED dfirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
