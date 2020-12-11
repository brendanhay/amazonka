{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetMemberDetectors
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes which data sources are enabled for the member account's detector.
module Network.AWS.GuardDuty.GetMemberDetectors
  ( -- * Creating a request
    GetMemberDetectors (..),
    mkGetMemberDetectors,

    -- ** Request lenses
    gmdDetectorId,
    gmdAccountIds,

    -- * Destructuring the response
    GetMemberDetectorsResponse (..),
    mkGetMemberDetectorsResponse,

    -- ** Response lenses
    gmdrsResponseStatus,
    gmdrsMemberDataSourceConfigurations,
    gmdrsUnprocessedAccounts,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMemberDetectors' smart constructor.
data GetMemberDetectors = GetMemberDetectors'
  { detectorId ::
      Lude.Text,
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

-- | Creates a value of 'GetMemberDetectors' with the minimum fields required to make a request.
--
-- * 'accountIds' - The account ID of the member account.
-- * 'detectorId' - The detector ID for the master account.
mkGetMemberDetectors ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'accountIds'
  Lude.NonEmpty Lude.Text ->
  GetMemberDetectors
mkGetMemberDetectors pDetectorId_ pAccountIds_ =
  GetMemberDetectors'
    { detectorId = pDetectorId_,
      accountIds = pAccountIds_
    }

-- | The detector ID for the master account.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdDetectorId :: Lens.Lens' GetMemberDetectors Lude.Text
gmdDetectorId = Lens.lens (detectorId :: GetMemberDetectors -> Lude.Text) (\s a -> s {detectorId = a} :: GetMemberDetectors)
{-# DEPRECATED gmdDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The account ID of the member account.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdAccountIds :: Lens.Lens' GetMemberDetectors (Lude.NonEmpty Lude.Text)
gmdAccountIds = Lens.lens (accountIds :: GetMemberDetectors -> Lude.NonEmpty Lude.Text) (\s a -> s {accountIds = a} :: GetMemberDetectors)
{-# DEPRECATED gmdAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

instance Lude.AWSRequest GetMemberDetectors where
  type Rs GetMemberDetectors = GetMemberDetectorsResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMemberDetectorsResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..:> "members")
            Lude.<*> (x Lude..?> "unprocessedAccounts" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders GetMemberDetectors where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMemberDetectors where
  toJSON GetMemberDetectors' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("accountIds" Lude..= accountIds)])

instance Lude.ToPath GetMemberDetectors where
  toPath GetMemberDetectors' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/member/detector/get"]

instance Lude.ToQuery GetMemberDetectors where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMemberDetectorsResponse' smart constructor.
data GetMemberDetectorsResponse = GetMemberDetectorsResponse'
  { responseStatus ::
      Lude.Int,
    memberDataSourceConfigurations ::
      Lude.NonEmpty
        MemberDataSourceConfiguration,
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

-- | Creates a value of 'GetMemberDetectorsResponse' with the minimum fields required to make a request.
--
-- * 'memberDataSourceConfigurations' - An object that describes which data sources are enabled for a member account.
-- * 'responseStatus' - The response status code.
-- * 'unprocessedAccounts' - A list of member account IDs that were unable to be processed along with an explanation for why they were not processed.
mkGetMemberDetectorsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'memberDataSourceConfigurations'
  Lude.NonEmpty MemberDataSourceConfiguration ->
  GetMemberDetectorsResponse
mkGetMemberDetectorsResponse
  pResponseStatus_
  pMemberDataSourceConfigurations_ =
    GetMemberDetectorsResponse'
      { responseStatus = pResponseStatus_,
        memberDataSourceConfigurations = pMemberDataSourceConfigurations_,
        unprocessedAccounts = Lude.mempty
      }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrsResponseStatus :: Lens.Lens' GetMemberDetectorsResponse Lude.Int
gmdrsResponseStatus = Lens.lens (responseStatus :: GetMemberDetectorsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMemberDetectorsResponse)
{-# DEPRECATED gmdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | An object that describes which data sources are enabled for a member account.
--
-- /Note:/ Consider using 'memberDataSourceConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrsMemberDataSourceConfigurations :: Lens.Lens' GetMemberDetectorsResponse (Lude.NonEmpty MemberDataSourceConfiguration)
gmdrsMemberDataSourceConfigurations = Lens.lens (memberDataSourceConfigurations :: GetMemberDetectorsResponse -> Lude.NonEmpty MemberDataSourceConfiguration) (\s a -> s {memberDataSourceConfigurations = a} :: GetMemberDetectorsResponse)
{-# DEPRECATED gmdrsMemberDataSourceConfigurations "Use generic-lens or generic-optics with 'memberDataSourceConfigurations' instead." #-}

-- | A list of member account IDs that were unable to be processed along with an explanation for why they were not processed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrsUnprocessedAccounts :: Lens.Lens' GetMemberDetectorsResponse [UnprocessedAccount]
gmdrsUnprocessedAccounts = Lens.lens (unprocessedAccounts :: GetMemberDetectorsResponse -> [UnprocessedAccount]) (\s a -> s {unprocessedAccounts = a} :: GetMemberDetectorsResponse)
{-# DEPRECATED gmdrsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}
