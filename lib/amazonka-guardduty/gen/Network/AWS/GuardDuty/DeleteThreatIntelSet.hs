{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DeleteThreatIntelSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the ThreatIntelSet specified by the ThreatIntelSet ID.
module Network.AWS.GuardDuty.DeleteThreatIntelSet
  ( -- * Creating a request
    DeleteThreatIntelSet (..),
    mkDeleteThreatIntelSet,

    -- ** Request lenses
    dtisDetectorId,
    dtisThreatIntelSetId,

    -- * Destructuring the response
    DeleteThreatIntelSetResponse (..),
    mkDeleteThreatIntelSetResponse,

    -- ** Response lenses
    dtisrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteThreatIntelSet' smart constructor.
data DeleteThreatIntelSet = DeleteThreatIntelSet'
  { detectorId ::
      Lude.Text,
    threatIntelSetId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteThreatIntelSet' with the minimum fields required to make a request.
--
-- * 'detectorId' - The unique ID of the detector that the threatIntelSet is associated with.
-- * 'threatIntelSetId' - The unique ID of the threatIntelSet that you want to delete.
mkDeleteThreatIntelSet ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'threatIntelSetId'
  Lude.Text ->
  DeleteThreatIntelSet
mkDeleteThreatIntelSet pDetectorId_ pThreatIntelSetId_ =
  DeleteThreatIntelSet'
    { detectorId = pDetectorId_,
      threatIntelSetId = pThreatIntelSetId_
    }

-- | The unique ID of the detector that the threatIntelSet is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtisDetectorId :: Lens.Lens' DeleteThreatIntelSet Lude.Text
dtisDetectorId = Lens.lens (detectorId :: DeleteThreatIntelSet -> Lude.Text) (\s a -> s {detectorId = a} :: DeleteThreatIntelSet)
{-# DEPRECATED dtisDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The unique ID of the threatIntelSet that you want to delete.
--
-- /Note:/ Consider using 'threatIntelSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtisThreatIntelSetId :: Lens.Lens' DeleteThreatIntelSet Lude.Text
dtisThreatIntelSetId = Lens.lens (threatIntelSetId :: DeleteThreatIntelSet -> Lude.Text) (\s a -> s {threatIntelSetId = a} :: DeleteThreatIntelSet)
{-# DEPRECATED dtisThreatIntelSetId "Use generic-lens or generic-optics with 'threatIntelSetId' instead." #-}

instance Lude.AWSRequest DeleteThreatIntelSet where
  type Rs DeleteThreatIntelSet = DeleteThreatIntelSetResponse
  request = Req.delete guardDutyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteThreatIntelSetResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteThreatIntelSet where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteThreatIntelSet where
  toPath DeleteThreatIntelSet' {..} =
    Lude.mconcat
      [ "/detector/",
        Lude.toBS detectorId,
        "/threatintelset/",
        Lude.toBS threatIntelSetId
      ]

instance Lude.ToQuery DeleteThreatIntelSet where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteThreatIntelSetResponse' smart constructor.
newtype DeleteThreatIntelSetResponse = DeleteThreatIntelSetResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteThreatIntelSetResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteThreatIntelSetResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteThreatIntelSetResponse
mkDeleteThreatIntelSetResponse pResponseStatus_ =
  DeleteThreatIntelSetResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtisrsResponseStatus :: Lens.Lens' DeleteThreatIntelSetResponse Lude.Int
dtisrsResponseStatus = Lens.lens (responseStatus :: DeleteThreatIntelSetResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteThreatIntelSetResponse)
{-# DEPRECATED dtisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
