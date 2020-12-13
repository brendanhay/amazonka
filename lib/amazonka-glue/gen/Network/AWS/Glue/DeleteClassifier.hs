{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.DeleteClassifier
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a classifier from the Data Catalog.
module Network.AWS.Glue.DeleteClassifier
  ( -- * Creating a request
    DeleteClassifier (..),
    mkDeleteClassifier,

    -- ** Request lenses
    dcfName,

    -- * Destructuring the response
    DeleteClassifierResponse (..),
    mkDeleteClassifierResponse,

    -- ** Response lenses
    drsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteClassifier' smart constructor.
newtype DeleteClassifier = DeleteClassifier'
  { -- | Name of the classifier to remove.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteClassifier' with the minimum fields required to make a request.
--
-- * 'name' - Name of the classifier to remove.
mkDeleteClassifier ::
  -- | 'name'
  Lude.Text ->
  DeleteClassifier
mkDeleteClassifier pName_ = DeleteClassifier' {name = pName_}

-- | Name of the classifier to remove.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcfName :: Lens.Lens' DeleteClassifier Lude.Text
dcfName = Lens.lens (name :: DeleteClassifier -> Lude.Text) (\s a -> s {name = a} :: DeleteClassifier)
{-# DEPRECATED dcfName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteClassifier where
  type Rs DeleteClassifier = DeleteClassifierResponse
  request = Req.postJSON glueService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteClassifierResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteClassifier where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.DeleteClassifier" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteClassifier where
  toJSON DeleteClassifier' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteClassifier where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteClassifier where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteClassifierResponse' smart constructor.
newtype DeleteClassifierResponse = DeleteClassifierResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteClassifierResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteClassifierResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteClassifierResponse
mkDeleteClassifierResponse pResponseStatus_ =
  DeleteClassifierResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteClassifierResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DeleteClassifierResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteClassifierResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
