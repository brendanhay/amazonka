{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.DeleteApplicationCloudWatchLoggingOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a CloudWatch log stream from an application. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> .
module Network.AWS.KinesisAnalytics.DeleteApplicationCloudWatchLoggingOption
  ( -- * Creating a request
    DeleteApplicationCloudWatchLoggingOption (..),
    mkDeleteApplicationCloudWatchLoggingOption,

    -- ** Request lenses
    dacwloCurrentApplicationVersionId,
    dacwloCloudWatchLoggingOptionId,
    dacwloApplicationName,

    -- * Destructuring the response
    DeleteApplicationCloudWatchLoggingOptionResponse (..),
    mkDeleteApplicationCloudWatchLoggingOptionResponse,

    -- ** Response lenses
    dacwlorsResponseStatus,
  )
where

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteApplicationCloudWatchLoggingOption' smart constructor.
data DeleteApplicationCloudWatchLoggingOption = DeleteApplicationCloudWatchLoggingOption'
  { -- | The version ID of the Kinesis Analytics application.
    currentApplicationVersionId :: Lude.Natural,
    -- | The @CloudWatchLoggingOptionId@ of the CloudWatch logging option to delete. You can get the @CloudWatchLoggingOptionId@ by using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
    cloudWatchLoggingOptionId :: Lude.Text,
    -- | The Kinesis Analytics application name.
    applicationName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApplicationCloudWatchLoggingOption' with the minimum fields required to make a request.
--
-- * 'currentApplicationVersionId' - The version ID of the Kinesis Analytics application.
-- * 'cloudWatchLoggingOptionId' - The @CloudWatchLoggingOptionId@ of the CloudWatch logging option to delete. You can get the @CloudWatchLoggingOptionId@ by using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
-- * 'applicationName' - The Kinesis Analytics application name.
mkDeleteApplicationCloudWatchLoggingOption ::
  -- | 'currentApplicationVersionId'
  Lude.Natural ->
  -- | 'cloudWatchLoggingOptionId'
  Lude.Text ->
  -- | 'applicationName'
  Lude.Text ->
  DeleteApplicationCloudWatchLoggingOption
mkDeleteApplicationCloudWatchLoggingOption
  pCurrentApplicationVersionId_
  pCloudWatchLoggingOptionId_
  pApplicationName_ =
    DeleteApplicationCloudWatchLoggingOption'
      { currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        cloudWatchLoggingOptionId =
          pCloudWatchLoggingOptionId_,
        applicationName = pApplicationName_
      }

-- | The version ID of the Kinesis Analytics application.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacwloCurrentApplicationVersionId :: Lens.Lens' DeleteApplicationCloudWatchLoggingOption Lude.Natural
dacwloCurrentApplicationVersionId = Lens.lens (currentApplicationVersionId :: DeleteApplicationCloudWatchLoggingOption -> Lude.Natural) (\s a -> s {currentApplicationVersionId = a} :: DeleteApplicationCloudWatchLoggingOption)
{-# DEPRECATED dacwloCurrentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead." #-}

-- | The @CloudWatchLoggingOptionId@ of the CloudWatch logging option to delete. You can get the @CloudWatchLoggingOptionId@ by using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation.
--
-- /Note:/ Consider using 'cloudWatchLoggingOptionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacwloCloudWatchLoggingOptionId :: Lens.Lens' DeleteApplicationCloudWatchLoggingOption Lude.Text
dacwloCloudWatchLoggingOptionId = Lens.lens (cloudWatchLoggingOptionId :: DeleteApplicationCloudWatchLoggingOption -> Lude.Text) (\s a -> s {cloudWatchLoggingOptionId = a} :: DeleteApplicationCloudWatchLoggingOption)
{-# DEPRECATED dacwloCloudWatchLoggingOptionId "Use generic-lens or generic-optics with 'cloudWatchLoggingOptionId' instead." #-}

-- | The Kinesis Analytics application name.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacwloApplicationName :: Lens.Lens' DeleteApplicationCloudWatchLoggingOption Lude.Text
dacwloApplicationName = Lens.lens (applicationName :: DeleteApplicationCloudWatchLoggingOption -> Lude.Text) (\s a -> s {applicationName = a} :: DeleteApplicationCloudWatchLoggingOption)
{-# DEPRECATED dacwloApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Lude.AWSRequest DeleteApplicationCloudWatchLoggingOption where
  type
    Rs DeleteApplicationCloudWatchLoggingOption =
      DeleteApplicationCloudWatchLoggingOptionResponse
  request = Req.postJSON kinesisAnalyticsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteApplicationCloudWatchLoggingOptionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteApplicationCloudWatchLoggingOption where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "KinesisAnalytics_20150814.DeleteApplicationCloudWatchLoggingOption" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteApplicationCloudWatchLoggingOption where
  toJSON DeleteApplicationCloudWatchLoggingOption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "CurrentApplicationVersionId"
                  Lude..= currentApplicationVersionId
              ),
            Lude.Just
              ("CloudWatchLoggingOptionId" Lude..= cloudWatchLoggingOptionId),
            Lude.Just ("ApplicationName" Lude..= applicationName)
          ]
      )

instance Lude.ToPath DeleteApplicationCloudWatchLoggingOption where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteApplicationCloudWatchLoggingOption where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteApplicationCloudWatchLoggingOptionResponse' smart constructor.
newtype DeleteApplicationCloudWatchLoggingOptionResponse = DeleteApplicationCloudWatchLoggingOptionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApplicationCloudWatchLoggingOptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteApplicationCloudWatchLoggingOptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteApplicationCloudWatchLoggingOptionResponse
mkDeleteApplicationCloudWatchLoggingOptionResponse pResponseStatus_ =
  DeleteApplicationCloudWatchLoggingOptionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dacwlorsResponseStatus :: Lens.Lens' DeleteApplicationCloudWatchLoggingOptionResponse Lude.Int
dacwlorsResponseStatus = Lens.lens (responseStatus :: DeleteApplicationCloudWatchLoggingOptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteApplicationCloudWatchLoggingOptionResponse)
{-# DEPRECATED dacwlorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
