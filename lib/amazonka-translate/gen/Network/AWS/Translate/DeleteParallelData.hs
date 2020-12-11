{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.DeleteParallelData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a parallel data resource in Amazon Translate.
module Network.AWS.Translate.DeleteParallelData
  ( -- * Creating a request
    DeleteParallelData (..),
    mkDeleteParallelData,

    -- ** Request lenses
    dpdName,

    -- * Destructuring the response
    DeleteParallelDataResponse (..),
    mkDeleteParallelDataResponse,

    -- ** Response lenses
    dpdrsStatus,
    dpdrsName,
    dpdrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Translate.Types

-- | /See:/ 'mkDeleteParallelData' smart constructor.
newtype DeleteParallelData = DeleteParallelData' {name :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteParallelData' with the minimum fields required to make a request.
--
-- * 'name' - The name of the parallel data resource that is being deleted.
mkDeleteParallelData ::
  -- | 'name'
  Lude.Text ->
  DeleteParallelData
mkDeleteParallelData pName_ = DeleteParallelData' {name = pName_}

-- | The name of the parallel data resource that is being deleted.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdName :: Lens.Lens' DeleteParallelData Lude.Text
dpdName = Lens.lens (name :: DeleteParallelData -> Lude.Text) (\s a -> s {name = a} :: DeleteParallelData)
{-# DEPRECATED dpdName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.AWSRequest DeleteParallelData where
  type Rs DeleteParallelData = DeleteParallelDataResponse
  request = Req.postJSON translateService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteParallelDataResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "Name")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteParallelData where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSShineFrontendService_20170701.DeleteParallelData" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteParallelData where
  toJSON DeleteParallelData' {..} =
    Lude.object (Lude.catMaybes [Lude.Just ("Name" Lude..= name)])

instance Lude.ToPath DeleteParallelData where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteParallelData where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteParallelDataResponse' smart constructor.
data DeleteParallelDataResponse = DeleteParallelDataResponse'
  { status ::
      Lude.Maybe ParallelDataStatus,
    name :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DeleteParallelDataResponse' with the minimum fields required to make a request.
--
-- * 'name' - The name of the parallel data resource that is being deleted.
-- * 'responseStatus' - The response status code.
-- * 'status' - The status of the parallel data deletion.
mkDeleteParallelDataResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteParallelDataResponse
mkDeleteParallelDataResponse pResponseStatus_ =
  DeleteParallelDataResponse'
    { status = Lude.Nothing,
      name = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The status of the parallel data deletion.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdrsStatus :: Lens.Lens' DeleteParallelDataResponse (Lude.Maybe ParallelDataStatus)
dpdrsStatus = Lens.lens (status :: DeleteParallelDataResponse -> Lude.Maybe ParallelDataStatus) (\s a -> s {status = a} :: DeleteParallelDataResponse)
{-# DEPRECATED dpdrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the parallel data resource that is being deleted.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdrsName :: Lens.Lens' DeleteParallelDataResponse (Lude.Maybe Lude.Text)
dpdrsName = Lens.lens (name :: DeleteParallelDataResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: DeleteParallelDataResponse)
{-# DEPRECATED dpdrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpdrsResponseStatus :: Lens.Lens' DeleteParallelDataResponse Lude.Int
dpdrsResponseStatus = Lens.lens (responseStatus :: DeleteParallelDataResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteParallelDataResponse)
{-# DEPRECATED dpdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
