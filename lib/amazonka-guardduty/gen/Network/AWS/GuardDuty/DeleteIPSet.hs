{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DeleteIPSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the IPSet specified by the @ipSetId@ . IPSets are called trusted IP lists in the console user interface.
module Network.AWS.GuardDuty.DeleteIPSet
  ( -- * Creating a request
    DeleteIPSet (..),
    mkDeleteIPSet,

    -- ** Request lenses
    disDetectorId,
    disIPSetId,

    -- * Destructuring the response
    DeleteIPSetResponse (..),
    mkDeleteIPSetResponse,

    -- ** Response lenses
    dipsrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteIPSet' smart constructor.
data DeleteIPSet = DeleteIPSet'
  { -- | The unique ID of the detector associated with the IPSet.
    detectorId :: Lude.Text,
    -- | The unique ID of the IPSet to delete.
    ipSetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIPSet' with the minimum fields required to make a request.
--
-- * 'detectorId' - The unique ID of the detector associated with the IPSet.
-- * 'ipSetId' - The unique ID of the IPSet to delete.
mkDeleteIPSet ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'ipSetId'
  Lude.Text ->
  DeleteIPSet
mkDeleteIPSet pDetectorId_ pIPSetId_ =
  DeleteIPSet' {detectorId = pDetectorId_, ipSetId = pIPSetId_}

-- | The unique ID of the detector associated with the IPSet.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disDetectorId :: Lens.Lens' DeleteIPSet Lude.Text
disDetectorId = Lens.lens (detectorId :: DeleteIPSet -> Lude.Text) (\s a -> s {detectorId = a} :: DeleteIPSet)
{-# DEPRECATED disDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The unique ID of the IPSet to delete.
--
-- /Note:/ Consider using 'ipSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
disIPSetId :: Lens.Lens' DeleteIPSet Lude.Text
disIPSetId = Lens.lens (ipSetId :: DeleteIPSet -> Lude.Text) (\s a -> s {ipSetId = a} :: DeleteIPSet)
{-# DEPRECATED disIPSetId "Use generic-lens or generic-optics with 'ipSetId' instead." #-}

instance Lude.AWSRequest DeleteIPSet where
  type Rs DeleteIPSet = DeleteIPSetResponse
  request = Req.delete guardDutyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteIPSetResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteIPSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteIPSet where
  toPath DeleteIPSet' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/ipset/", Lude.toBS ipSetId]

instance Lude.ToQuery DeleteIPSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteIPSetResponse' smart constructor.
newtype DeleteIPSetResponse = DeleteIPSetResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteIPSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteIPSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteIPSetResponse
mkDeleteIPSetResponse pResponseStatus_ =
  DeleteIPSetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dipsrsResponseStatus :: Lens.Lens' DeleteIPSetResponse Lude.Int
dipsrsResponseStatus = Lens.lens (responseStatus :: DeleteIPSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteIPSetResponse)
{-# DEPRECATED dipsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
