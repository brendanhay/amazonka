{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppStream.CreateUsageReportSubscription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a usage report subscription. Usage reports are generated daily.
module Network.AWS.AppStream.CreateUsageReportSubscription
  ( -- * Creating a request
    CreateUsageReportSubscription (..),
    mkCreateUsageReportSubscription,

    -- * Destructuring the response
    CreateUsageReportSubscriptionResponse (..),
    mkCreateUsageReportSubscriptionResponse,

    -- ** Response lenses
    cursrsSchedule,
    cursrsS3BucketName,
    cursrsResponseStatus,
  )
where

import Network.AWS.AppStream.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateUsageReportSubscription' smart constructor.
data CreateUsageReportSubscription = CreateUsageReportSubscription'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUsageReportSubscription' with the minimum fields required to make a request.
mkCreateUsageReportSubscription ::
  CreateUsageReportSubscription
mkCreateUsageReportSubscription = CreateUsageReportSubscription'

instance Lude.AWSRequest CreateUsageReportSubscription where
  type
    Rs CreateUsageReportSubscription =
      CreateUsageReportSubscriptionResponse
  request = Req.postJSON appStreamService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateUsageReportSubscriptionResponse'
            Lude.<$> (x Lude..?> "Schedule")
            Lude.<*> (x Lude..?> "S3BucketName")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateUsageReportSubscription where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "PhotonAdminProxyService.CreateUsageReportSubscription" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateUsageReportSubscription where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath CreateUsageReportSubscription where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateUsageReportSubscription where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateUsageReportSubscriptionResponse' smart constructor.
data CreateUsageReportSubscriptionResponse = CreateUsageReportSubscriptionResponse'
  { schedule ::
      Lude.Maybe
        UsageReportSchedule,
    s3BucketName ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateUsageReportSubscriptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 's3BucketName' - The Amazon S3 bucket where generated reports are stored.
--
-- If you enabled on-instance session scripts and Amazon S3 logging for your session script configuration, AppStream 2.0 created an S3 bucket to store the script output. The bucket is unique to your account and Region. When you enable usage reporting in this case, AppStream 2.0 uses the same bucket to store your usage reports. If you haven't already enabled on-instance session scripts, when you enable usage reports, AppStream 2.0 creates a new S3 bucket.
-- * 'schedule' - The schedule for generating usage reports.
mkCreateUsageReportSubscriptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateUsageReportSubscriptionResponse
mkCreateUsageReportSubscriptionResponse pResponseStatus_ =
  CreateUsageReportSubscriptionResponse'
    { schedule = Lude.Nothing,
      s3BucketName = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The schedule for generating usage reports.
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursrsSchedule :: Lens.Lens' CreateUsageReportSubscriptionResponse (Lude.Maybe UsageReportSchedule)
cursrsSchedule = Lens.lens (schedule :: CreateUsageReportSubscriptionResponse -> Lude.Maybe UsageReportSchedule) (\s a -> s {schedule = a} :: CreateUsageReportSubscriptionResponse)
{-# DEPRECATED cursrsSchedule "Use generic-lens or generic-optics with 'schedule' instead." #-}

-- | The Amazon S3 bucket where generated reports are stored.
--
-- If you enabled on-instance session scripts and Amazon S3 logging for your session script configuration, AppStream 2.0 created an S3 bucket to store the script output. The bucket is unique to your account and Region. When you enable usage reporting in this case, AppStream 2.0 uses the same bucket to store your usage reports. If you haven't already enabled on-instance session scripts, when you enable usage reports, AppStream 2.0 creates a new S3 bucket.
--
-- /Note:/ Consider using 's3BucketName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursrsS3BucketName :: Lens.Lens' CreateUsageReportSubscriptionResponse (Lude.Maybe Lude.Text)
cursrsS3BucketName = Lens.lens (s3BucketName :: CreateUsageReportSubscriptionResponse -> Lude.Maybe Lude.Text) (\s a -> s {s3BucketName = a} :: CreateUsageReportSubscriptionResponse)
{-# DEPRECATED cursrsS3BucketName "Use generic-lens or generic-optics with 's3BucketName' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cursrsResponseStatus :: Lens.Lens' CreateUsageReportSubscriptionResponse Lude.Int
cursrsResponseStatus = Lens.lens (responseStatus :: CreateUsageReportSubscriptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateUsageReportSubscriptionResponse)
{-# DEPRECATED cursrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
