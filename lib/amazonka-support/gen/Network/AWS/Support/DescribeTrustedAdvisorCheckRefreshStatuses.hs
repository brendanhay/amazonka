{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the refresh status of the AWS Trusted Advisor checks that have the specified check IDs. You can get the check IDs by calling the 'DescribeTrustedAdvisorChecks' operation.
--
-- Some checks are refreshed automatically, and you can't return their refresh statuses by using the @DescribeTrustedAdvisorCheckRefreshStatuses@ operation. If you call this operation for these checks, you might see an @InvalidParameterValue@ error.
module Network.AWS.Support.DescribeTrustedAdvisorCheckRefreshStatuses
  ( -- * Creating a request
    DescribeTrustedAdvisorCheckRefreshStatuses (..),
    mkDescribeTrustedAdvisorCheckRefreshStatuses,

    -- ** Request lenses
    dtacrsCheckIds,

    -- * Destructuring the response
    DescribeTrustedAdvisorCheckRefreshStatusesResponse (..),
    mkDescribeTrustedAdvisorCheckRefreshStatusesResponse,

    -- ** Response lenses
    dtacrsrsStatuses,
    dtacrsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Support.Types

-- | /See:/ 'mkDescribeTrustedAdvisorCheckRefreshStatuses' smart constructor.
newtype DescribeTrustedAdvisorCheckRefreshStatuses = DescribeTrustedAdvisorCheckRefreshStatuses'
  { -- | The IDs of the Trusted Advisor checks to get the status of.
    checkIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrustedAdvisorCheckRefreshStatuses' with the minimum fields required to make a request.
--
-- * 'checkIds' - The IDs of the Trusted Advisor checks to get the status of.
mkDescribeTrustedAdvisorCheckRefreshStatuses ::
  DescribeTrustedAdvisorCheckRefreshStatuses
mkDescribeTrustedAdvisorCheckRefreshStatuses =
  DescribeTrustedAdvisorCheckRefreshStatuses'
    { checkIds =
        Lude.mempty
    }

-- | The IDs of the Trusted Advisor checks to get the status of.
--
-- /Note:/ Consider using 'checkIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacrsCheckIds :: Lens.Lens' DescribeTrustedAdvisorCheckRefreshStatuses [Lude.Text]
dtacrsCheckIds = Lens.lens (checkIds :: DescribeTrustedAdvisorCheckRefreshStatuses -> [Lude.Text]) (\s a -> s {checkIds = a} :: DescribeTrustedAdvisorCheckRefreshStatuses)
{-# DEPRECATED dtacrsCheckIds "Use generic-lens or generic-optics with 'checkIds' instead." #-}

instance Lude.AWSRequest DescribeTrustedAdvisorCheckRefreshStatuses where
  type
    Rs DescribeTrustedAdvisorCheckRefreshStatuses =
      DescribeTrustedAdvisorCheckRefreshStatusesResponse
  request = Req.postJSON supportService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTrustedAdvisorCheckRefreshStatusesResponse'
            Lude.<$> (x Lude..?> "statuses" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTrustedAdvisorCheckRefreshStatuses where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSSupport_20130415.DescribeTrustedAdvisorCheckRefreshStatuses" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTrustedAdvisorCheckRefreshStatuses where
  toJSON DescribeTrustedAdvisorCheckRefreshStatuses' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("checkIds" Lude..= checkIds)])

instance Lude.ToPath DescribeTrustedAdvisorCheckRefreshStatuses where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTrustedAdvisorCheckRefreshStatuses where
  toQuery = Lude.const Lude.mempty

-- | The statuses of the Trusted Advisor checks returned by the 'DescribeTrustedAdvisorCheckRefreshStatuses' operation.
--
-- /See:/ 'mkDescribeTrustedAdvisorCheckRefreshStatusesResponse' smart constructor.
data DescribeTrustedAdvisorCheckRefreshStatusesResponse = DescribeTrustedAdvisorCheckRefreshStatusesResponse'
  { -- | The refresh status of the specified Trusted Advisor checks.
    statuses :: [TrustedAdvisorCheckRefreshStatus],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrustedAdvisorCheckRefreshStatusesResponse' with the minimum fields required to make a request.
--
-- * 'statuses' - The refresh status of the specified Trusted Advisor checks.
-- * 'responseStatus' - The response status code.
mkDescribeTrustedAdvisorCheckRefreshStatusesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTrustedAdvisorCheckRefreshStatusesResponse
mkDescribeTrustedAdvisorCheckRefreshStatusesResponse
  pResponseStatus_ =
    DescribeTrustedAdvisorCheckRefreshStatusesResponse'
      { statuses =
          Lude.mempty,
        responseStatus = pResponseStatus_
      }

-- | The refresh status of the specified Trusted Advisor checks.
--
-- /Note:/ Consider using 'statuses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacrsrsStatuses :: Lens.Lens' DescribeTrustedAdvisorCheckRefreshStatusesResponse [TrustedAdvisorCheckRefreshStatus]
dtacrsrsStatuses = Lens.lens (statuses :: DescribeTrustedAdvisorCheckRefreshStatusesResponse -> [TrustedAdvisorCheckRefreshStatus]) (\s a -> s {statuses = a} :: DescribeTrustedAdvisorCheckRefreshStatusesResponse)
{-# DEPRECATED dtacrsrsStatuses "Use generic-lens or generic-optics with 'statuses' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacrsrsResponseStatus :: Lens.Lens' DescribeTrustedAdvisorCheckRefreshStatusesResponse Lude.Int
dtacrsrsResponseStatus = Lens.lens (responseStatus :: DescribeTrustedAdvisorCheckRefreshStatusesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTrustedAdvisorCheckRefreshStatusesResponse)
{-# DEPRECATED dtacrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
