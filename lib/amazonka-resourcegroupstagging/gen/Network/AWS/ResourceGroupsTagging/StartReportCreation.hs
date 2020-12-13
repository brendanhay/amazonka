{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.StartReportCreation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a report that lists all tagged resources in accounts across your organization and tells whether each resource is compliant with the effective tag policy. Compliance data is refreshed daily.
--
-- The generated report is saved to the following location:
-- @s3://example-bucket/AwsTagPolicies/o-exampleorgid/YYYY-MM-ddTHH:mm:ssZ/report.csv@
-- You can call this operation only from the organization's master account and from the us-east-1 Region.
module Network.AWS.ResourceGroupsTagging.StartReportCreation
  ( -- * Creating a request
    StartReportCreation (..),
    mkStartReportCreation,

    -- ** Request lenses
    srcS3Bucket,

    -- * Destructuring the response
    StartReportCreationResponse (..),
    mkStartReportCreationResponse,

    -- ** Response lenses
    srcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroupsTagging.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkStartReportCreation' smart constructor.
newtype StartReportCreation = StartReportCreation'
  { -- | The name of the Amazon S3 bucket where the report will be stored; for example:
    --
    -- @awsexamplebucket@
    -- For more information on S3 bucket requirements, including an example bucket policy, see the example S3 bucket policy on this page.
    s3Bucket :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartReportCreation' with the minimum fields required to make a request.
--
-- * 's3Bucket' - The name of the Amazon S3 bucket where the report will be stored; for example:
--
-- @awsexamplebucket@
-- For more information on S3 bucket requirements, including an example bucket policy, see the example S3 bucket policy on this page.
mkStartReportCreation ::
  -- | 's3Bucket'
  Lude.Text ->
  StartReportCreation
mkStartReportCreation pS3Bucket_ =
  StartReportCreation' {s3Bucket = pS3Bucket_}

-- | The name of the Amazon S3 bucket where the report will be stored; for example:
--
-- @awsexamplebucket@
-- For more information on S3 bucket requirements, including an example bucket policy, see the example S3 bucket policy on this page.
--
-- /Note:/ Consider using 's3Bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcS3Bucket :: Lens.Lens' StartReportCreation Lude.Text
srcS3Bucket = Lens.lens (s3Bucket :: StartReportCreation -> Lude.Text) (\s a -> s {s3Bucket = a} :: StartReportCreation)
{-# DEPRECATED srcS3Bucket "Use generic-lens or generic-optics with 's3Bucket' instead." #-}

instance Lude.AWSRequest StartReportCreation where
  type Rs StartReportCreation = StartReportCreationResponse
  request = Req.postJSON resourceGroupsTaggingService
  response =
    Res.receiveEmpty
      ( \s h x ->
          StartReportCreationResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartReportCreation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "ResourceGroupsTaggingAPI_20170126.StartReportCreation" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartReportCreation where
  toJSON StartReportCreation' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("S3Bucket" Lude..= s3Bucket)])

instance Lude.ToPath StartReportCreation where
  toPath = Lude.const "/"

instance Lude.ToQuery StartReportCreation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkStartReportCreationResponse' smart constructor.
newtype StartReportCreationResponse = StartReportCreationResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartReportCreationResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkStartReportCreationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartReportCreationResponse
mkStartReportCreationResponse pResponseStatus_ =
  StartReportCreationResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srcrsResponseStatus :: Lens.Lens' StartReportCreationResponse Lude.Int
srcrsResponseStatus = Lens.lens (responseStatus :: StartReportCreationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartReportCreationResponse)
{-# DEPRECATED srcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
