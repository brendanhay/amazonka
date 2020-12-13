{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.DeleteFilter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the filter specified by the filter name.
module Network.AWS.GuardDuty.DeleteFilter
  ( -- * Creating a request
    DeleteFilter (..),
    mkDeleteFilter,

    -- ** Request lenses
    dfFilterName,
    dfDetectorId,

    -- * Destructuring the response
    DeleteFilterResponse (..),
    mkDeleteFilterResponse,

    -- ** Response lenses
    dfrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteFilter' smart constructor.
data DeleteFilter = DeleteFilter'
  { -- | The name of the filter that you want to delete.
    filterName :: Lude.Text,
    -- | The unique ID of the detector that the filter is associated with.
    detectorId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFilter' with the minimum fields required to make a request.
--
-- * 'filterName' - The name of the filter that you want to delete.
-- * 'detectorId' - The unique ID of the detector that the filter is associated with.
mkDeleteFilter ::
  -- | 'filterName'
  Lude.Text ->
  -- | 'detectorId'
  Lude.Text ->
  DeleteFilter
mkDeleteFilter pFilterName_ pDetectorId_ =
  DeleteFilter'
    { filterName = pFilterName_,
      detectorId = pDetectorId_
    }

-- | The name of the filter that you want to delete.
--
-- /Note:/ Consider using 'filterName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfFilterName :: Lens.Lens' DeleteFilter Lude.Text
dfFilterName = Lens.lens (filterName :: DeleteFilter -> Lude.Text) (\s a -> s {filterName = a} :: DeleteFilter)
{-# DEPRECATED dfFilterName "Use generic-lens or generic-optics with 'filterName' instead." #-}

-- | The unique ID of the detector that the filter is associated with.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfDetectorId :: Lens.Lens' DeleteFilter Lude.Text
dfDetectorId = Lens.lens (detectorId :: DeleteFilter -> Lude.Text) (\s a -> s {detectorId = a} :: DeleteFilter)
{-# DEPRECATED dfDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Lude.AWSRequest DeleteFilter where
  type Rs DeleteFilter = DeleteFilterResponse
  request = Req.delete guardDutyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteFilterResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteFilter where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath DeleteFilter where
  toPath DeleteFilter' {..} =
    Lude.mconcat
      [ "/detector/",
        Lude.toBS detectorId,
        "/filter/",
        Lude.toBS filterName
      ]

instance Lude.ToQuery DeleteFilter where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteFilterResponse' smart constructor.
newtype DeleteFilterResponse = DeleteFilterResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteFilterResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteFilterResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteFilterResponse
mkDeleteFilterResponse pResponseStatus_ =
  DeleteFilterResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfrsResponseStatus :: Lens.Lens' DeleteFilterResponse Lude.Int
dfrsResponseStatus = Lens.lens (responseStatus :: DeleteFilterResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteFilterResponse)
{-# DEPRECATED dfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
