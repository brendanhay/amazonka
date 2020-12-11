{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.DeleteTrust
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an existing trust relationship between your AWS Managed Microsoft AD directory and an external domain.
module Network.AWS.DirectoryService.DeleteTrust
  ( -- * Creating a request
    DeleteTrust (..),
    mkDeleteTrust,

    -- ** Request lenses
    dtDeleteAssociatedConditionalForwarder,
    dtTrustId,

    -- * Destructuring the response
    DeleteTrustResponse (..),
    mkDeleteTrustResponse,

    -- ** Response lenses
    dttrsTrustId,
    dttrsResponseStatus,
  )
where

import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Deletes the local side of an existing trust relationship between the AWS Managed Microsoft AD directory and the external domain.
--
-- /See:/ 'mkDeleteTrust' smart constructor.
data DeleteTrust = DeleteTrust'
  { deleteAssociatedConditionalForwarder ::
      Lude.Maybe Lude.Bool,
    trustId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTrust' with the minimum fields required to make a request.
--
-- * 'deleteAssociatedConditionalForwarder' - Delete a conditional forwarder as part of a DeleteTrustRequest.
-- * 'trustId' - The Trust ID of the trust relationship to be deleted.
mkDeleteTrust ::
  -- | 'trustId'
  Lude.Text ->
  DeleteTrust
mkDeleteTrust pTrustId_ =
  DeleteTrust'
    { deleteAssociatedConditionalForwarder = Lude.Nothing,
      trustId = pTrustId_
    }

-- | Delete a conditional forwarder as part of a DeleteTrustRequest.
--
-- /Note:/ Consider using 'deleteAssociatedConditionalForwarder' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtDeleteAssociatedConditionalForwarder :: Lens.Lens' DeleteTrust (Lude.Maybe Lude.Bool)
dtDeleteAssociatedConditionalForwarder = Lens.lens (deleteAssociatedConditionalForwarder :: DeleteTrust -> Lude.Maybe Lude.Bool) (\s a -> s {deleteAssociatedConditionalForwarder = a} :: DeleteTrust)
{-# DEPRECATED dtDeleteAssociatedConditionalForwarder "Use generic-lens or generic-optics with 'deleteAssociatedConditionalForwarder' instead." #-}

-- | The Trust ID of the trust relationship to be deleted.
--
-- /Note:/ Consider using 'trustId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTrustId :: Lens.Lens' DeleteTrust Lude.Text
dtTrustId = Lens.lens (trustId :: DeleteTrust -> Lude.Text) (\s a -> s {trustId = a} :: DeleteTrust)
{-# DEPRECATED dtTrustId "Use generic-lens or generic-optics with 'trustId' instead." #-}

instance Lude.AWSRequest DeleteTrust where
  type Rs DeleteTrust = DeleteTrustResponse
  request = Req.postJSON directoryServiceService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteTrustResponse'
            Lude.<$> (x Lude..?> "TrustId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTrust where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DirectoryService_20150416.DeleteTrust" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteTrust where
  toJSON DeleteTrust' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("DeleteAssociatedConditionalForwarder" Lude..=)
              Lude.<$> deleteAssociatedConditionalForwarder,
            Lude.Just ("TrustId" Lude..= trustId)
          ]
      )

instance Lude.ToPath DeleteTrust where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTrust where
  toQuery = Lude.const Lude.mempty

-- | The result of a DeleteTrust request.
--
-- /See:/ 'mkDeleteTrustResponse' smart constructor.
data DeleteTrustResponse = DeleteTrustResponse'
  { trustId ::
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

-- | Creates a value of 'DeleteTrustResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'trustId' - The Trust ID of the trust relationship that was deleted.
mkDeleteTrustResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTrustResponse
mkDeleteTrustResponse pResponseStatus_ =
  DeleteTrustResponse'
    { trustId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Trust ID of the trust relationship that was deleted.
--
-- /Note:/ Consider using 'trustId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttrsTrustId :: Lens.Lens' DeleteTrustResponse (Lude.Maybe Lude.Text)
dttrsTrustId = Lens.lens (trustId :: DeleteTrustResponse -> Lude.Maybe Lude.Text) (\s a -> s {trustId = a} :: DeleteTrustResponse)
{-# DEPRECATED dttrsTrustId "Use generic-lens or generic-optics with 'trustId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dttrsResponseStatus :: Lens.Lens' DeleteTrustResponse Lude.Int
dttrsResponseStatus = Lens.lens (responseStatus :: DeleteTrustResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTrustResponse)
{-# DEPRECATED dttrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
