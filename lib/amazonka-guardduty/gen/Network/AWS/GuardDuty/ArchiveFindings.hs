{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.ArchiveFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Archives GuardDuty findings that are specified by the list of finding IDs.
module Network.AWS.GuardDuty.ArchiveFindings
  ( -- * Creating a request
    ArchiveFindings (..),
    mkArchiveFindings,

    -- ** Request lenses
    afDetectorId,
    afFindingIds,

    -- * Destructuring the response
    ArchiveFindingsResponse (..),
    mkArchiveFindingsResponse,

    -- ** Response lenses
    afrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkArchiveFindings' smart constructor.
data ArchiveFindings = ArchiveFindings'
  { detectorId :: Lude.Text,
    findingIds :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ArchiveFindings' with the minimum fields required to make a request.
--
-- * 'detectorId' - The ID of the detector that specifies the GuardDuty service whose findings you want to archive.
-- * 'findingIds' - The IDs of the findings that you want to archive.
mkArchiveFindings ::
  -- | 'detectorId'
  Lude.Text ->
  ArchiveFindings
mkArchiveFindings pDetectorId_ =
  ArchiveFindings'
    { detectorId = pDetectorId_,
      findingIds = Lude.mempty
    }

-- | The ID of the detector that specifies the GuardDuty service whose findings you want to archive.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afDetectorId :: Lens.Lens' ArchiveFindings Lude.Text
afDetectorId = Lens.lens (detectorId :: ArchiveFindings -> Lude.Text) (\s a -> s {detectorId = a} :: ArchiveFindings)
{-# DEPRECATED afDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The IDs of the findings that you want to archive.
--
-- /Note:/ Consider using 'findingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afFindingIds :: Lens.Lens' ArchiveFindings [Lude.Text]
afFindingIds = Lens.lens (findingIds :: ArchiveFindings -> [Lude.Text]) (\s a -> s {findingIds = a} :: ArchiveFindings)
{-# DEPRECATED afFindingIds "Use generic-lens or generic-optics with 'findingIds' instead." #-}

instance Lude.AWSRequest ArchiveFindings where
  type Rs ArchiveFindings = ArchiveFindingsResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          ArchiveFindingsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ArchiveFindings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ArchiveFindings where
  toJSON ArchiveFindings' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("findingIds" Lude..= findingIds)])

instance Lude.ToPath ArchiveFindings where
  toPath ArchiveFindings' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/findings/archive"]

instance Lude.ToQuery ArchiveFindings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkArchiveFindingsResponse' smart constructor.
newtype ArchiveFindingsResponse = ArchiveFindingsResponse'
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

-- | Creates a value of 'ArchiveFindingsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkArchiveFindingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ArchiveFindingsResponse
mkArchiveFindingsResponse pResponseStatus_ =
  ArchiveFindingsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
afrsResponseStatus :: Lens.Lens' ArchiveFindingsResponse Lude.Int
afrsResponseStatus = Lens.lens (responseStatus :: ArchiveFindingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ArchiveFindingsResponse)
{-# DEPRECATED afrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
