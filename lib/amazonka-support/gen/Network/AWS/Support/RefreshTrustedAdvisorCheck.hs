{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.RefreshTrustedAdvisorCheck
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Refreshes the AWS Trusted Advisor check that you specify using the check ID. You can get the check IDs by calling the 'DescribeTrustedAdvisorChecks' operation.
--
-- The response contains a 'TrustedAdvisorCheckRefreshStatus' object.
module Network.AWS.Support.RefreshTrustedAdvisorCheck
  ( -- * Creating a request
    RefreshTrustedAdvisorCheck (..),
    mkRefreshTrustedAdvisorCheck,

    -- ** Request lenses
    rtacCheckId,

    -- * Destructuring the response
    RefreshTrustedAdvisorCheckResponse (..),
    mkRefreshTrustedAdvisorCheckResponse,

    -- ** Response lenses
    rtacrsStatus,
    rtacrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Support.Types

-- |
--
-- /See:/ 'mkRefreshTrustedAdvisorCheck' smart constructor.
newtype RefreshTrustedAdvisorCheck = RefreshTrustedAdvisorCheck'
  { -- | The unique identifier for the Trusted Advisor check to refresh. __Note:__ Specifying the check ID of a check that is automatically refreshed causes an @InvalidParameterValue@ error.
    checkId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RefreshTrustedAdvisorCheck' with the minimum fields required to make a request.
--
-- * 'checkId' - The unique identifier for the Trusted Advisor check to refresh. __Note:__ Specifying the check ID of a check that is automatically refreshed causes an @InvalidParameterValue@ error.
mkRefreshTrustedAdvisorCheck ::
  -- | 'checkId'
  Lude.Text ->
  RefreshTrustedAdvisorCheck
mkRefreshTrustedAdvisorCheck pCheckId_ =
  RefreshTrustedAdvisorCheck' {checkId = pCheckId_}

-- | The unique identifier for the Trusted Advisor check to refresh. __Note:__ Specifying the check ID of a check that is automatically refreshed causes an @InvalidParameterValue@ error.
--
-- /Note:/ Consider using 'checkId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacCheckId :: Lens.Lens' RefreshTrustedAdvisorCheck Lude.Text
rtacCheckId = Lens.lens (checkId :: RefreshTrustedAdvisorCheck -> Lude.Text) (\s a -> s {checkId = a} :: RefreshTrustedAdvisorCheck)
{-# DEPRECATED rtacCheckId "Use generic-lens or generic-optics with 'checkId' instead." #-}

instance Lude.AWSRequest RefreshTrustedAdvisorCheck where
  type
    Rs RefreshTrustedAdvisorCheck =
      RefreshTrustedAdvisorCheckResponse
  request = Req.postJSON supportService
  response =
    Res.receiveJSON
      ( \s h x ->
          RefreshTrustedAdvisorCheckResponse'
            Lude.<$> (x Lude..:> "status") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders RefreshTrustedAdvisorCheck where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSSupport_20130415.RefreshTrustedAdvisorCheck" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON RefreshTrustedAdvisorCheck where
  toJSON RefreshTrustedAdvisorCheck' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("checkId" Lude..= checkId)])

instance Lude.ToPath RefreshTrustedAdvisorCheck where
  toPath = Lude.const "/"

instance Lude.ToQuery RefreshTrustedAdvisorCheck where
  toQuery = Lude.const Lude.mempty

-- | The current refresh status of a Trusted Advisor check.
--
-- /See:/ 'mkRefreshTrustedAdvisorCheckResponse' smart constructor.
data RefreshTrustedAdvisorCheckResponse = RefreshTrustedAdvisorCheckResponse'
  { -- | The current refresh status for a check, including the amount of time until the check is eligible for refresh.
    status :: TrustedAdvisorCheckRefreshStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RefreshTrustedAdvisorCheckResponse' with the minimum fields required to make a request.
--
-- * 'status' - The current refresh status for a check, including the amount of time until the check is eligible for refresh.
-- * 'responseStatus' - The response status code.
mkRefreshTrustedAdvisorCheckResponse ::
  -- | 'status'
  TrustedAdvisorCheckRefreshStatus ->
  -- | 'responseStatus'
  Lude.Int ->
  RefreshTrustedAdvisorCheckResponse
mkRefreshTrustedAdvisorCheckResponse pStatus_ pResponseStatus_ =
  RefreshTrustedAdvisorCheckResponse'
    { status = pStatus_,
      responseStatus = pResponseStatus_
    }

-- | The current refresh status for a check, including the amount of time until the check is eligible for refresh.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacrsStatus :: Lens.Lens' RefreshTrustedAdvisorCheckResponse TrustedAdvisorCheckRefreshStatus
rtacrsStatus = Lens.lens (status :: RefreshTrustedAdvisorCheckResponse -> TrustedAdvisorCheckRefreshStatus) (\s a -> s {status = a} :: RefreshTrustedAdvisorCheckResponse)
{-# DEPRECATED rtacrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtacrsResponseStatus :: Lens.Lens' RefreshTrustedAdvisorCheckResponse Lude.Int
rtacrsResponseStatus = Lens.lens (responseStatus :: RefreshTrustedAdvisorCheckResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: RefreshTrustedAdvisorCheckResponse)
{-# DEPRECATED rtacrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
