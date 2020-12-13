{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UnarchiveFindings
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unarchives GuardDuty findings specified by the @findingIds@ .
module Network.AWS.GuardDuty.UnarchiveFindings
  ( -- * Creating a request
    UnarchiveFindings (..),
    mkUnarchiveFindings,

    -- ** Request lenses
    uFindingIds,
    uDetectorId,

    -- * Destructuring the response
    UnarchiveFindingsResponse (..),
    mkUnarchiveFindingsResponse,

    -- ** Response lenses
    ursResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUnarchiveFindings' smart constructor.
data UnarchiveFindings = UnarchiveFindings'
  { -- | The IDs of the findings to unarchive.
    findingIds :: [Lude.Text],
    -- | The ID of the detector associated with the findings to unarchive.
    detectorId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnarchiveFindings' with the minimum fields required to make a request.
--
-- * 'findingIds' - The IDs of the findings to unarchive.
-- * 'detectorId' - The ID of the detector associated with the findings to unarchive.
mkUnarchiveFindings ::
  -- | 'detectorId'
  Lude.Text ->
  UnarchiveFindings
mkUnarchiveFindings pDetectorId_ =
  UnarchiveFindings'
    { findingIds = Lude.mempty,
      detectorId = pDetectorId_
    }

-- | The IDs of the findings to unarchive.
--
-- /Note:/ Consider using 'findingIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uFindingIds :: Lens.Lens' UnarchiveFindings [Lude.Text]
uFindingIds = Lens.lens (findingIds :: UnarchiveFindings -> [Lude.Text]) (\s a -> s {findingIds = a} :: UnarchiveFindings)
{-# DEPRECATED uFindingIds "Use generic-lens or generic-optics with 'findingIds' instead." #-}

-- | The ID of the detector associated with the findings to unarchive.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDetectorId :: Lens.Lens' UnarchiveFindings Lude.Text
uDetectorId = Lens.lens (detectorId :: UnarchiveFindings -> Lude.Text) (\s a -> s {detectorId = a} :: UnarchiveFindings)
{-# DEPRECATED uDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Lude.AWSRequest UnarchiveFindings where
  type Rs UnarchiveFindings = UnarchiveFindingsResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UnarchiveFindingsResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UnarchiveFindings where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UnarchiveFindings where
  toJSON UnarchiveFindings' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("findingIds" Lude..= findingIds)])

instance Lude.ToPath UnarchiveFindings where
  toPath UnarchiveFindings' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/findings/unarchive"]

instance Lude.ToQuery UnarchiveFindings where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUnarchiveFindingsResponse' smart constructor.
newtype UnarchiveFindingsResponse = UnarchiveFindingsResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UnarchiveFindingsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUnarchiveFindingsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UnarchiveFindingsResponse
mkUnarchiveFindingsResponse pResponseStatus_ =
  UnarchiveFindingsResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ursResponseStatus :: Lens.Lens' UnarchiveFindingsResponse Lude.Int
ursResponseStatus = Lens.lens (responseStatus :: UnarchiveFindingsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UnarchiveFindingsResponse)
{-# DEPRECATED ursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
