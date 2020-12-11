{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ResourceGroupsTagging.DescribeReportCreation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of the @StartReportCreation@ operation.
--
-- You can call this operation only from the organization's master account and from the us-east-1 Region.
module Network.AWS.ResourceGroupsTagging.DescribeReportCreation
  ( -- * Creating a request
    DescribeReportCreation (..),
    mkDescribeReportCreation,

    -- * Destructuring the response
    DescribeReportCreationResponse (..),
    mkDescribeReportCreationResponse,

    -- ** Response lenses
    drcrsStatus,
    drcrsS3Location,
    drcrsErrorMessage,
    drcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import Network.AWS.ResourceGroupsTagging.Types
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeReportCreation' smart constructor.
data DescribeReportCreation = DescribeReportCreation'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReportCreation' with the minimum fields required to make a request.
mkDescribeReportCreation ::
  DescribeReportCreation
mkDescribeReportCreation = DescribeReportCreation'

instance Lude.AWSRequest DescribeReportCreation where
  type Rs DescribeReportCreation = DescribeReportCreationResponse
  request = Req.postJSON resourceGroupsTaggingService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeReportCreationResponse'
            Lude.<$> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "S3Location")
            Lude.<*> (x Lude..?> "ErrorMessage")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeReportCreation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "ResourceGroupsTaggingAPI_20170126.DescribeReportCreation" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeReportCreation where
  toJSON = Lude.const (Lude.Object Lude.mempty)

instance Lude.ToPath DescribeReportCreation where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeReportCreation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeReportCreationResponse' smart constructor.
data DescribeReportCreationResponse = DescribeReportCreationResponse'
  { status ::
      Lude.Maybe Lude.Text,
    s3Location ::
      Lude.Maybe Lude.Text,
    errorMessage ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeReportCreationResponse' with the minimum fields required to make a request.
--
-- * 'errorMessage' - Details of the common errors that all operations return.
-- * 'responseStatus' - The response status code.
-- * 's3Location' - The path to the Amazon S3 bucket where the report was stored on creation.
-- * 'status' - Reports the status of the operation.
--
-- The operation status can be one of the following:
--
--     * @RUNNING@ - Report creation is in progress.
--
--
--     * @SUCCEEDED@ - Report creation is complete. You can open the report from the Amazon S3 bucket that you specified when you ran @StartReportCreation@ .
--
--
--     * @FAILED@ - Report creation timed out or the Amazon S3 bucket is not accessible.
--
--
--     * @NO REPORT@ - No report was generated in the last 90 days.
mkDescribeReportCreationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeReportCreationResponse
mkDescribeReportCreationResponse pResponseStatus_ =
  DescribeReportCreationResponse'
    { status = Lude.Nothing,
      s3Location = Lude.Nothing,
      errorMessage = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Reports the status of the operation.
--
-- The operation status can be one of the following:
--
--     * @RUNNING@ - Report creation is in progress.
--
--
--     * @SUCCEEDED@ - Report creation is complete. You can open the report from the Amazon S3 bucket that you specified when you ran @StartReportCreation@ .
--
--
--     * @FAILED@ - Report creation timed out or the Amazon S3 bucket is not accessible.
--
--
--     * @NO REPORT@ - No report was generated in the last 90 days.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrsStatus :: Lens.Lens' DescribeReportCreationResponse (Lude.Maybe Lude.Text)
drcrsStatus = Lens.lens (status :: DescribeReportCreationResponse -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: DescribeReportCreationResponse)
{-# DEPRECATED drcrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The path to the Amazon S3 bucket where the report was stored on creation.
--
-- /Note:/ Consider using 's3Location' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrsS3Location :: Lens.Lens' DescribeReportCreationResponse (Lude.Maybe Lude.Text)
drcrsS3Location = Lens.lens (s3Location :: DescribeReportCreationResponse -> Lude.Maybe Lude.Text) (\s a -> s {s3Location = a} :: DescribeReportCreationResponse)
{-# DEPRECATED drcrsS3Location "Use generic-lens or generic-optics with 's3Location' instead." #-}

-- | Details of the common errors that all operations return.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrsErrorMessage :: Lens.Lens' DescribeReportCreationResponse (Lude.Maybe Lude.Text)
drcrsErrorMessage = Lens.lens (errorMessage :: DescribeReportCreationResponse -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: DescribeReportCreationResponse)
{-# DEPRECATED drcrsErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drcrsResponseStatus :: Lens.Lens' DescribeReportCreationResponse Lude.Int
drcrsResponseStatus = Lens.lens (responseStatus :: DescribeReportCreationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeReportCreationResponse)
{-# DEPRECATED drcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
