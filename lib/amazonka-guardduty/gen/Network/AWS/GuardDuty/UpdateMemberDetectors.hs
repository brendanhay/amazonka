{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.UpdateMemberDetectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Contains information on member accounts to be updated.
module Network.AWS.GuardDuty.UpdateMemberDetectors
  ( -- * Creating a request
    UpdateMemberDetectors (..),
    mkUpdateMemberDetectors,

    -- ** Request lenses
    umdDataSources,
    umdDetectorId,
    umdAccountIds,

    -- * Destructuring the response
    UpdateMemberDetectorsResponse (..),
    mkUpdateMemberDetectorsResponse,

    -- ** Response lenses
    umdrsResponseStatus,
    umdrsUnprocessedAccounts,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateMemberDetectors' smart constructor.
data UpdateMemberDetectors = UpdateMemberDetectors'
  { dataSources ::
      Lude.Maybe DataSourceConfigurations,
    detectorId :: Lude.Text,
    accountIds :: Lude.NonEmpty Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMemberDetectors' with the minimum fields required to make a request.
--
-- * 'accountIds' - A list of member account IDs to be updated.
-- * 'dataSources' - An object describes which data sources will be updated.
-- * 'detectorId' - The detector ID of the master account.
mkUpdateMemberDetectors ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'accountIds'
  Lude.NonEmpty Lude.Text ->
  UpdateMemberDetectors
mkUpdateMemberDetectors pDetectorId_ pAccountIds_ =
  UpdateMemberDetectors'
    { dataSources = Lude.Nothing,
      detectorId = pDetectorId_,
      accountIds = pAccountIds_
    }

-- | An object describes which data sources will be updated.
--
-- /Note:/ Consider using 'dataSources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umdDataSources :: Lens.Lens' UpdateMemberDetectors (Lude.Maybe DataSourceConfigurations)
umdDataSources = Lens.lens (dataSources :: UpdateMemberDetectors -> Lude.Maybe DataSourceConfigurations) (\s a -> s {dataSources = a} :: UpdateMemberDetectors)
{-# DEPRECATED umdDataSources "Use generic-lens or generic-optics with 'dataSources' instead." #-}

-- | The detector ID of the master account.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umdDetectorId :: Lens.Lens' UpdateMemberDetectors Lude.Text
umdDetectorId = Lens.lens (detectorId :: UpdateMemberDetectors -> Lude.Text) (\s a -> s {detectorId = a} :: UpdateMemberDetectors)
{-# DEPRECATED umdDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | A list of member account IDs to be updated.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umdAccountIds :: Lens.Lens' UpdateMemberDetectors (Lude.NonEmpty Lude.Text)
umdAccountIds = Lens.lens (accountIds :: UpdateMemberDetectors -> Lude.NonEmpty Lude.Text) (\s a -> s {accountIds = a} :: UpdateMemberDetectors)
{-# DEPRECATED umdAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

instance Lude.AWSRequest UpdateMemberDetectors where
  type Rs UpdateMemberDetectors = UpdateMemberDetectorsResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateMemberDetectorsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "unprocessedAccounts" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders UpdateMemberDetectors where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateMemberDetectors where
  toJSON UpdateMemberDetectors' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("dataSources" Lude..=) Lude.<$> dataSources,
            Lude.Just ("accountIds" Lude..= accountIds)
          ]
      )

instance Lude.ToPath UpdateMemberDetectors where
  toPath UpdateMemberDetectors' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/member/detector/update"]

instance Lude.ToQuery UpdateMemberDetectors where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateMemberDetectorsResponse' smart constructor.
data UpdateMemberDetectorsResponse = UpdateMemberDetectorsResponse'
  { responseStatus ::
      Lude.Int,
    unprocessedAccounts ::
      [UnprocessedAccount]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateMemberDetectorsResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'unprocessedAccounts' - A list of member account IDs that were unable to be processed along with an explanation for why they were not processed.
mkUpdateMemberDetectorsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateMemberDetectorsResponse
mkUpdateMemberDetectorsResponse pResponseStatus_ =
  UpdateMemberDetectorsResponse'
    { responseStatus = pResponseStatus_,
      unprocessedAccounts = Lude.mempty
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umdrsResponseStatus :: Lens.Lens' UpdateMemberDetectorsResponse Lude.Int
umdrsResponseStatus = Lens.lens (responseStatus :: UpdateMemberDetectorsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateMemberDetectorsResponse)
{-# DEPRECATED umdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | A list of member account IDs that were unable to be processed along with an explanation for why they were not processed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
umdrsUnprocessedAccounts :: Lens.Lens' UpdateMemberDetectorsResponse [UnprocessedAccount]
umdrsUnprocessedAccounts = Lens.lens (unprocessedAccounts :: UpdateMemberDetectorsResponse -> [UnprocessedAccount]) (\s a -> s {unprocessedAccounts = a} :: UpdateMemberDetectorsResponse)
{-# DEPRECATED umdrsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}
