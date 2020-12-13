{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.DescribeTrustedAdvisorCheckSummaries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the results for the AWS Trusted Advisor check summaries for the check IDs that you specified. You can get the check IDs by calling the 'DescribeTrustedAdvisorChecks' operation.
--
-- The response contains an array of 'TrustedAdvisorCheckSummary' objects.
module Network.AWS.Support.DescribeTrustedAdvisorCheckSummaries
  ( -- * Creating a request
    DescribeTrustedAdvisorCheckSummaries (..),
    mkDescribeTrustedAdvisorCheckSummaries,

    -- ** Request lenses
    dtacsCheckIds,

    -- * Destructuring the response
    DescribeTrustedAdvisorCheckSummariesResponse (..),
    mkDescribeTrustedAdvisorCheckSummariesResponse,

    -- ** Response lenses
    dtacsrsSummaries,
    dtacsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Support.Types

-- | /See:/ 'mkDescribeTrustedAdvisorCheckSummaries' smart constructor.
newtype DescribeTrustedAdvisorCheckSummaries = DescribeTrustedAdvisorCheckSummaries'
  { -- | The IDs of the Trusted Advisor checks.
    checkIds :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrustedAdvisorCheckSummaries' with the minimum fields required to make a request.
--
-- * 'checkIds' - The IDs of the Trusted Advisor checks.
mkDescribeTrustedAdvisorCheckSummaries ::
  DescribeTrustedAdvisorCheckSummaries
mkDescribeTrustedAdvisorCheckSummaries =
  DescribeTrustedAdvisorCheckSummaries' {checkIds = Lude.mempty}

-- | The IDs of the Trusted Advisor checks.
--
-- /Note:/ Consider using 'checkIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacsCheckIds :: Lens.Lens' DescribeTrustedAdvisorCheckSummaries [Lude.Text]
dtacsCheckIds = Lens.lens (checkIds :: DescribeTrustedAdvisorCheckSummaries -> [Lude.Text]) (\s a -> s {checkIds = a} :: DescribeTrustedAdvisorCheckSummaries)
{-# DEPRECATED dtacsCheckIds "Use generic-lens or generic-optics with 'checkIds' instead." #-}

instance Lude.AWSRequest DescribeTrustedAdvisorCheckSummaries where
  type
    Rs DescribeTrustedAdvisorCheckSummaries =
      DescribeTrustedAdvisorCheckSummariesResponse
  request = Req.postJSON supportService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTrustedAdvisorCheckSummariesResponse'
            Lude.<$> (x Lude..?> "summaries" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTrustedAdvisorCheckSummaries where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSSupport_20130415.DescribeTrustedAdvisorCheckSummaries" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTrustedAdvisorCheckSummaries where
  toJSON DescribeTrustedAdvisorCheckSummaries' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("checkIds" Lude..= checkIds)])

instance Lude.ToPath DescribeTrustedAdvisorCheckSummaries where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTrustedAdvisorCheckSummaries where
  toQuery = Lude.const Lude.mempty

-- | The summaries of the Trusted Advisor checks returned by the 'DescribeTrustedAdvisorCheckSummaries' operation.
--
-- /See:/ 'mkDescribeTrustedAdvisorCheckSummariesResponse' smart constructor.
data DescribeTrustedAdvisorCheckSummariesResponse = DescribeTrustedAdvisorCheckSummariesResponse'
  { -- | The summary information for the requested Trusted Advisor checks.
    summaries :: [TrustedAdvisorCheckSummary],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTrustedAdvisorCheckSummariesResponse' with the minimum fields required to make a request.
--
-- * 'summaries' - The summary information for the requested Trusted Advisor checks.
-- * 'responseStatus' - The response status code.
mkDescribeTrustedAdvisorCheckSummariesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTrustedAdvisorCheckSummariesResponse
mkDescribeTrustedAdvisorCheckSummariesResponse pResponseStatus_ =
  DescribeTrustedAdvisorCheckSummariesResponse'
    { summaries =
        Lude.mempty,
      responseStatus = pResponseStatus_
    }

-- | The summary information for the requested Trusted Advisor checks.
--
-- /Note:/ Consider using 'summaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacsrsSummaries :: Lens.Lens' DescribeTrustedAdvisorCheckSummariesResponse [TrustedAdvisorCheckSummary]
dtacsrsSummaries = Lens.lens (summaries :: DescribeTrustedAdvisorCheckSummariesResponse -> [TrustedAdvisorCheckSummary]) (\s a -> s {summaries = a} :: DescribeTrustedAdvisorCheckSummariesResponse)
{-# DEPRECATED dtacsrsSummaries "Use generic-lens or generic-optics with 'summaries' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtacsrsResponseStatus :: Lens.Lens' DescribeTrustedAdvisorCheckSummariesResponse Lude.Int
dtacsrsResponseStatus = Lens.lens (responseStatus :: DescribeTrustedAdvisorCheckSummariesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTrustedAdvisorCheckSummariesResponse)
{-# DEPRECATED dtacsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
