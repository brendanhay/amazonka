{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.AddApplicationCloudWatchLoggingOption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a CloudWatch log stream to monitor application configuration errors. For more information about using CloudWatch log streams with Amazon Kinesis Analytics applications, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/cloudwatch-logs.html Working with Amazon CloudWatch Logs> .
module Network.AWS.KinesisAnalytics.AddApplicationCloudWatchLoggingOption
  ( -- * Creating a request
    AddApplicationCloudWatchLoggingOption (..),
    mkAddApplicationCloudWatchLoggingOption,

    -- ** Request lenses
    aacwloApplicationName,
    aacwloCurrentApplicationVersionId,
    aacwloCloudWatchLoggingOption,

    -- * Destructuring the response
    AddApplicationCloudWatchLoggingOptionResponse (..),
    mkAddApplicationCloudWatchLoggingOptionResponse,

    -- ** Response lenses
    aacwlorsResponseStatus,
  )
where

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddApplicationCloudWatchLoggingOption' smart constructor.
data AddApplicationCloudWatchLoggingOption = AddApplicationCloudWatchLoggingOption'
  { applicationName ::
      Lude.Text,
    currentApplicationVersionId ::
      Lude.Natural,
    cloudWatchLoggingOption ::
      CloudWatchLoggingOption
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddApplicationCloudWatchLoggingOption' with the minimum fields required to make a request.
--
-- * 'applicationName' - The Kinesis Analytics application name.
-- * 'cloudWatchLoggingOption' - Provides the CloudWatch log stream Amazon Resource Name (ARN) and the IAM role ARN. Note: To write application messages to CloudWatch, the IAM role that is used must have the @PutLogEvents@ policy action enabled.
-- * 'currentApplicationVersionId' - The version ID of the Kinesis Analytics application.
mkAddApplicationCloudWatchLoggingOption ::
  -- | 'applicationName'
  Lude.Text ->
  -- | 'currentApplicationVersionId'
  Lude.Natural ->
  -- | 'cloudWatchLoggingOption'
  CloudWatchLoggingOption ->
  AddApplicationCloudWatchLoggingOption
mkAddApplicationCloudWatchLoggingOption
  pApplicationName_
  pCurrentApplicationVersionId_
  pCloudWatchLoggingOption_ =
    AddApplicationCloudWatchLoggingOption'
      { applicationName =
          pApplicationName_,
        currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        cloudWatchLoggingOption = pCloudWatchLoggingOption_
      }

-- | The Kinesis Analytics application name.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacwloApplicationName :: Lens.Lens' AddApplicationCloudWatchLoggingOption Lude.Text
aacwloApplicationName = Lens.lens (applicationName :: AddApplicationCloudWatchLoggingOption -> Lude.Text) (\s a -> s {applicationName = a} :: AddApplicationCloudWatchLoggingOption)
{-# DEPRECATED aacwloApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | The version ID of the Kinesis Analytics application.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacwloCurrentApplicationVersionId :: Lens.Lens' AddApplicationCloudWatchLoggingOption Lude.Natural
aacwloCurrentApplicationVersionId = Lens.lens (currentApplicationVersionId :: AddApplicationCloudWatchLoggingOption -> Lude.Natural) (\s a -> s {currentApplicationVersionId = a} :: AddApplicationCloudWatchLoggingOption)
{-# DEPRECATED aacwloCurrentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead." #-}

-- | Provides the CloudWatch log stream Amazon Resource Name (ARN) and the IAM role ARN. Note: To write application messages to CloudWatch, the IAM role that is used must have the @PutLogEvents@ policy action enabled.
--
-- /Note:/ Consider using 'cloudWatchLoggingOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacwloCloudWatchLoggingOption :: Lens.Lens' AddApplicationCloudWatchLoggingOption CloudWatchLoggingOption
aacwloCloudWatchLoggingOption = Lens.lens (cloudWatchLoggingOption :: AddApplicationCloudWatchLoggingOption -> CloudWatchLoggingOption) (\s a -> s {cloudWatchLoggingOption = a} :: AddApplicationCloudWatchLoggingOption)
{-# DEPRECATED aacwloCloudWatchLoggingOption "Use generic-lens or generic-optics with 'cloudWatchLoggingOption' instead." #-}

instance Lude.AWSRequest AddApplicationCloudWatchLoggingOption where
  type
    Rs AddApplicationCloudWatchLoggingOption =
      AddApplicationCloudWatchLoggingOptionResponse
  request = Req.postJSON kinesisAnalyticsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AddApplicationCloudWatchLoggingOptionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddApplicationCloudWatchLoggingOption where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "KinesisAnalytics_20150814.AddApplicationCloudWatchLoggingOption" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddApplicationCloudWatchLoggingOption where
  toJSON AddApplicationCloudWatchLoggingOption' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ApplicationName" Lude..= applicationName),
            Lude.Just
              ( "CurrentApplicationVersionId"
                  Lude..= currentApplicationVersionId
              ),
            Lude.Just
              ("CloudWatchLoggingOption" Lude..= cloudWatchLoggingOption)
          ]
      )

instance Lude.ToPath AddApplicationCloudWatchLoggingOption where
  toPath = Lude.const "/"

instance Lude.ToQuery AddApplicationCloudWatchLoggingOption where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkAddApplicationCloudWatchLoggingOptionResponse' smart constructor.
newtype AddApplicationCloudWatchLoggingOptionResponse = AddApplicationCloudWatchLoggingOptionResponse'
  { responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'AddApplicationCloudWatchLoggingOptionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAddApplicationCloudWatchLoggingOptionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddApplicationCloudWatchLoggingOptionResponse
mkAddApplicationCloudWatchLoggingOptionResponse pResponseStatus_ =
  AddApplicationCloudWatchLoggingOptionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aacwlorsResponseStatus :: Lens.Lens' AddApplicationCloudWatchLoggingOptionResponse Lude.Int
aacwlorsResponseStatus = Lens.lens (responseStatus :: AddApplicationCloudWatchLoggingOptionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddApplicationCloudWatchLoggingOptionResponse)
{-# DEPRECATED aacwlorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
