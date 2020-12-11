{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.DeleteDatastore
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified data store.
module Network.AWS.IoTAnalytics.DeleteDatastore
  ( -- * Creating a request
    DeleteDatastore (..),
    mkDeleteDatastore,

    -- ** Request lenses
    ddDatastoreName,

    -- * Destructuring the response
    DeleteDatastoreResponse (..),
    mkDeleteDatastoreResponse,
  )
where

import Network.AWS.IoTAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDatastore' smart constructor.
newtype DeleteDatastore = DeleteDatastore'
  { datastoreName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDatastore' with the minimum fields required to make a request.
--
-- * 'datastoreName' - The name of the data store to delete.
mkDeleteDatastore ::
  -- | 'datastoreName'
  Lude.Text ->
  DeleteDatastore
mkDeleteDatastore pDatastoreName_ =
  DeleteDatastore' {datastoreName = pDatastoreName_}

-- | The name of the data store to delete.
--
-- /Note:/ Consider using 'datastoreName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDatastoreName :: Lens.Lens' DeleteDatastore Lude.Text
ddDatastoreName = Lens.lens (datastoreName :: DeleteDatastore -> Lude.Text) (\s a -> s {datastoreName = a} :: DeleteDatastore)
{-# DEPRECATED ddDatastoreName "Use generic-lens or generic-optics with 'datastoreName' instead." #-}

instance Lude.AWSRequest DeleteDatastore where
  type Rs DeleteDatastore = DeleteDatastoreResponse
  request = Req.delete ioTAnalyticsService
  response = Res.receiveNull DeleteDatastoreResponse'

instance Lude.ToHeaders DeleteDatastore where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteDatastore where
  toPath DeleteDatastore' {..} =
    Lude.mconcat ["/datastores/", Lude.toBS datastoreName]

instance Lude.ToQuery DeleteDatastore where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDatastoreResponse' smart constructor.
data DeleteDatastoreResponse = DeleteDatastoreResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDatastoreResponse' with the minimum fields required to make a request.
mkDeleteDatastoreResponse ::
  DeleteDatastoreResponse
mkDeleteDatastoreResponse = DeleteDatastoreResponse'
