{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.AddApplicationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an external destination to your Amazon Kinesis Analytics application.
--
-- If you want Amazon Kinesis Analytics to deliver data from an in-application stream within your application to an external destination (such as an Amazon Kinesis stream, an Amazon Kinesis Firehose delivery stream, or an AWS Lambda function), you add the relevant configuration to your application using this operation. You can configure one or more outputs for your application. Each output configuration maps an in-application stream and an external destination.
-- You can use one of the output configurations to deliver data from your in-application error stream to an external destination so that you can analyze the errors. For more information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-output.html Understanding Application Output (Destination)> .
-- Any configuration update, including adding a streaming source using this operation, results in a new version of the application. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to find the current application version.
-- For the limits on the number of application inputs and outputs you can configure, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits> .
-- This operation requires permissions to perform the @kinesisanalytics:AddApplicationOutput@ action.
module Network.AWS.KinesisAnalytics.AddApplicationOutput
  ( -- * Creating a request
    AddApplicationOutput (..),
    mkAddApplicationOutput,

    -- ** Request lenses
    aaoCurrentApplicationVersionId,
    aaoOutput,
    aaoApplicationName,

    -- * Destructuring the response
    AddApplicationOutputResponse (..),
    mkAddApplicationOutputResponse,

    -- ** Response lenses
    aaorsResponseStatus,
  )
where

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkAddApplicationOutput' smart constructor.
data AddApplicationOutput = AddApplicationOutput'
  { -- | Version of the application to which you want to add the output configuration. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
    currentApplicationVersionId :: Lude.Natural,
    -- | An array of objects, each describing one output configuration. In the output configuration, you specify the name of an in-application stream, a destination (that is, an Amazon Kinesis stream, an Amazon Kinesis Firehose delivery stream, or an AWS Lambda function), and record the formation to use when writing to the destination.
    output :: Output,
    -- | Name of the application to which you want to add the output configuration.
    applicationName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddApplicationOutput' with the minimum fields required to make a request.
--
-- * 'currentApplicationVersionId' - Version of the application to which you want to add the output configuration. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
-- * 'output' - An array of objects, each describing one output configuration. In the output configuration, you specify the name of an in-application stream, a destination (that is, an Amazon Kinesis stream, an Amazon Kinesis Firehose delivery stream, or an AWS Lambda function), and record the formation to use when writing to the destination.
-- * 'applicationName' - Name of the application to which you want to add the output configuration.
mkAddApplicationOutput ::
  -- | 'currentApplicationVersionId'
  Lude.Natural ->
  -- | 'output'
  Output ->
  -- | 'applicationName'
  Lude.Text ->
  AddApplicationOutput
mkAddApplicationOutput
  pCurrentApplicationVersionId_
  pOutput_
  pApplicationName_ =
    AddApplicationOutput'
      { currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        output = pOutput_,
        applicationName = pApplicationName_
      }

-- | Version of the application to which you want to add the output configuration. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaoCurrentApplicationVersionId :: Lens.Lens' AddApplicationOutput Lude.Natural
aaoCurrentApplicationVersionId = Lens.lens (currentApplicationVersionId :: AddApplicationOutput -> Lude.Natural) (\s a -> s {currentApplicationVersionId = a} :: AddApplicationOutput)
{-# DEPRECATED aaoCurrentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead." #-}

-- | An array of objects, each describing one output configuration. In the output configuration, you specify the name of an in-application stream, a destination (that is, an Amazon Kinesis stream, an Amazon Kinesis Firehose delivery stream, or an AWS Lambda function), and record the formation to use when writing to the destination.
--
-- /Note:/ Consider using 'output' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaoOutput :: Lens.Lens' AddApplicationOutput Output
aaoOutput = Lens.lens (output :: AddApplicationOutput -> Output) (\s a -> s {output = a} :: AddApplicationOutput)
{-# DEPRECATED aaoOutput "Use generic-lens or generic-optics with 'output' instead." #-}

-- | Name of the application to which you want to add the output configuration.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaoApplicationName :: Lens.Lens' AddApplicationOutput Lude.Text
aaoApplicationName = Lens.lens (applicationName :: AddApplicationOutput -> Lude.Text) (\s a -> s {applicationName = a} :: AddApplicationOutput)
{-# DEPRECATED aaoApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Lude.AWSRequest AddApplicationOutput where
  type Rs AddApplicationOutput = AddApplicationOutputResponse
  request = Req.postJSON kinesisAnalyticsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AddApplicationOutputResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddApplicationOutput where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "KinesisAnalytics_20150814.AddApplicationOutput" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddApplicationOutput where
  toJSON AddApplicationOutput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "CurrentApplicationVersionId"
                  Lude..= currentApplicationVersionId
              ),
            Lude.Just ("Output" Lude..= output),
            Lude.Just ("ApplicationName" Lude..= applicationName)
          ]
      )

instance Lude.ToPath AddApplicationOutput where
  toPath = Lude.const "/"

instance Lude.ToQuery AddApplicationOutput where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkAddApplicationOutputResponse' smart constructor.
newtype AddApplicationOutputResponse = AddApplicationOutputResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddApplicationOutputResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAddApplicationOutputResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddApplicationOutputResponse
mkAddApplicationOutputResponse pResponseStatus_ =
  AddApplicationOutputResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aaorsResponseStatus :: Lens.Lens' AddApplicationOutputResponse Lude.Int
aaorsResponseStatus = Lens.lens (responseStatus :: AddApplicationOutputResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddApplicationOutputResponse)
{-# DEPRECATED aaorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
