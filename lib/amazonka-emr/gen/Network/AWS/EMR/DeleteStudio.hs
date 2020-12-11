{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.DeleteStudio
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes an Amazon EMR Studio from the Studio metadata store.
module Network.AWS.EMR.DeleteStudio
  ( -- * Creating a request
    DeleteStudio (..),
    mkDeleteStudio,

    -- ** Request lenses
    dStudioId,

    -- * Destructuring the response
    DeleteStudioResponse (..),
    mkDeleteStudioResponse,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteStudio' smart constructor.
newtype DeleteStudio = DeleteStudio' {studioId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStudio' with the minimum fields required to make a request.
--
-- * 'studioId' - The ID of the Amazon EMR Studio.
mkDeleteStudio ::
  -- | 'studioId'
  Lude.Text ->
  DeleteStudio
mkDeleteStudio pStudioId_ = DeleteStudio' {studioId = pStudioId_}

-- | The ID of the Amazon EMR Studio.
--
-- /Note:/ Consider using 'studioId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStudioId :: Lens.Lens' DeleteStudio Lude.Text
dStudioId = Lens.lens (studioId :: DeleteStudio -> Lude.Text) (\s a -> s {studioId = a} :: DeleteStudio)
{-# DEPRECATED dStudioId "Use generic-lens or generic-optics with 'studioId' instead." #-}

instance Lude.AWSRequest DeleteStudio where
  type Rs DeleteStudio = DeleteStudioResponse
  request = Req.postJSON emrService
  response = Res.receiveNull DeleteStudioResponse'

instance Lude.ToHeaders DeleteStudio where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.DeleteStudio" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteStudio where
  toJSON DeleteStudio' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("StudioId" Lude..= studioId)])

instance Lude.ToPath DeleteStudio where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteStudio where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteStudioResponse' smart constructor.
data DeleteStudioResponse = DeleteStudioResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteStudioResponse' with the minimum fields required to make a request.
mkDeleteStudioResponse ::
  DeleteStudioResponse
mkDeleteStudioResponse = DeleteStudioResponse'
