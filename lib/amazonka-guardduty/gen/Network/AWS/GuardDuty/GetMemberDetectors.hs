{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    gmdAccountIds,
    gmdDetectorId,

    -- * Destructuring the response
    GetMemberDetectorsResponse (..),
    mkGetMemberDetectorsResponse,

    -- ** Response lenses
    gmdrsUnprocessedAccounts,
    gmdrsMemberDataSourceConfigurations,
    gmdrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMemberDetectors' smart constructor.
data GetMemberDetectors = GetMemberDetectors'
  { -- | The account ID of the member account.
    accountIds :: Lude.NonEmpty Lude.Text,
    -- | The detector ID for the master account.
    detectorId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMemberDetectors' with the minimum fields required to make a request.
--
-- * 'accountIds' - The account ID of the member account.
-- * 'detectorId' - The detector ID for the master account.
mkGetMemberDetectors ::
  -- | 'accountIds'
  Lude.NonEmpty Lude.Text ->
  -- | 'detectorId'
  Lude.Text ->
  GetMemberDetectors
mkGetMemberDetectors pAccountIds_ pDetectorId_ =
  GetMemberDetectors'
    { accountIds = pAccountIds_,
      detectorId = pDetectorId_
    }

-- | The account ID of the member account.
--
-- /Note:/ Consider using 'accountIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdAccountIds :: Lens.Lens' GetMemberDetectors (Lude.NonEmpty Lude.Text)
gmdAccountIds = Lens.lens (accountIds :: GetMemberDetectors -> Lude.NonEmpty Lude.Text) (\s a -> s {accountIds = a} :: GetMemberDetectors)
{-# DEPRECATED gmdAccountIds "Use generic-lens or generic-optics with 'accountIds' instead." #-}

-- | The detector ID for the master account.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdDetectorId :: Lens.Lens' GetMemberDetectors Lude.Text
gmdDetectorId = Lens.lens (detectorId :: GetMemberDetectors -> Lude.Text) (\s a -> s {detectorId = a} :: GetMemberDetectors)
{-# DEPRECATED gmdDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

instance Lude.AWSRequest GetMemberDetectors where
  type Rs GetMemberDetectors = GetMemberDetectorsResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMemberDetectorsResponse'
            Lude.<$> (x Lude..?> "unprocessedAccounts" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "members")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
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
  { -- | A list of member account IDs that were unable to be processed along with an explanation for why they were not processed.
    unprocessedAccounts :: [UnprocessedAccount],
    -- | An object that describes which data sources are enabled for a member account.
    memberDataSourceConfigurations :: Lude.NonEmpty MemberDataSourceConfiguration,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMemberDetectorsResponse' with the minimum fields required to make a request.
--
-- * 'unprocessedAccounts' - A list of member account IDs that were unable to be processed along with an explanation for why they were not processed.
-- * 'memberDataSourceConfigurations' - An object that describes which data sources are enabled for a member account.
-- * 'responseStatus' - The response status code.
mkGetMemberDetectorsResponse ::
  -- | 'memberDataSourceConfigurations'
  Lude.NonEmpty MemberDataSourceConfiguration ->
  -- | 'responseStatus'
  Lude.Int ->
  GetMemberDetectorsResponse
mkGetMemberDetectorsResponse
  pMemberDataSourceConfigurations_
  pResponseStatus_ =
    GetMemberDetectorsResponse'
      { unprocessedAccounts = Lude.mempty,
        memberDataSourceConfigurations = pMemberDataSourceConfigurations_,
        responseStatus = pResponseStatus_
      }

-- | A list of member account IDs that were unable to be processed along with an explanation for why they were not processed.
--
-- /Note:/ Consider using 'unprocessedAccounts' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrsUnprocessedAccounts :: Lens.Lens' GetMemberDetectorsResponse [UnprocessedAccount]
gmdrsUnprocessedAccounts = Lens.lens (unprocessedAccounts :: GetMemberDetectorsResponse -> [UnprocessedAccount]) (\s a -> s {unprocessedAccounts = a} :: GetMemberDetectorsResponse)
{-# DEPRECATED gmdrsUnprocessedAccounts "Use generic-lens or generic-optics with 'unprocessedAccounts' instead." #-}

-- | An object that describes which data sources are enabled for a member account.
--
-- /Note:/ Consider using 'memberDataSourceConfigurations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrsMemberDataSourceConfigurations :: Lens.Lens' GetMemberDetectorsResponse (Lude.NonEmpty MemberDataSourceConfiguration)
gmdrsMemberDataSourceConfigurations = Lens.lens (memberDataSourceConfigurations :: GetMemberDetectorsResponse -> Lude.NonEmpty MemberDataSourceConfiguration) (\s a -> s {memberDataSourceConfigurations = a} :: GetMemberDetectorsResponse)
{-# DEPRECATED gmdrsMemberDataSourceConfigurations "Use generic-lens or generic-optics with 'memberDataSourceConfigurations' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmdrsResponseStatus :: Lens.Lens' GetMemberDetectorsResponse Lude.Int
gmdrsResponseStatus = Lens.lens (responseStatus :: GetMemberDetectorsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMemberDetectorsResponse)
{-# DEPRECATED gmdrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
