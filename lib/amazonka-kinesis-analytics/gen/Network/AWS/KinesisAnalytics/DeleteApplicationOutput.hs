{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.DeleteApplicationOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes output destination configuration from your application configuration. Amazon Kinesis Analytics will no longer write data from the corresponding in-application stream to the external output destination.
--
-- This operation requires permissions to perform the @kinesisanalytics:DeleteApplicationOutput@ action.
module Network.AWS.KinesisAnalytics.DeleteApplicationOutput
  ( -- * Creating a request
    DeleteApplicationOutput (..),
    mkDeleteApplicationOutput,

    -- ** Request lenses
    daoCurrentApplicationVersionId,
    daoOutputId,
    daoApplicationName,

    -- * Destructuring the response
    DeleteApplicationOutputResponse (..),
    mkDeleteApplicationOutputResponse,

    -- ** Response lenses
    daorsResponseStatus,
  )
where

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkDeleteApplicationOutput' smart constructor.
data DeleteApplicationOutput = DeleteApplicationOutput'
  { -- | Amazon Kinesis Analytics application version. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
    currentApplicationVersionId :: Lude.Natural,
    -- | The ID of the configuration to delete. Each output configuration that is added to the application, either when the application is created or later using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationOutput.html AddApplicationOutput> operation, has a unique ID. You need to provide the ID to uniquely identify the output configuration that you want to delete from the application configuration. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the specific @OutputId@ .
    outputId :: Lude.Text,
    -- | Amazon Kinesis Analytics application name.
    applicationName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApplicationOutput' with the minimum fields required to make a request.
--
-- * 'currentApplicationVersionId' - Amazon Kinesis Analytics application version. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
-- * 'outputId' - The ID of the configuration to delete. Each output configuration that is added to the application, either when the application is created or later using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationOutput.html AddApplicationOutput> operation, has a unique ID. You need to provide the ID to uniquely identify the output configuration that you want to delete from the application configuration. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the specific @OutputId@ .
-- * 'applicationName' - Amazon Kinesis Analytics application name.
mkDeleteApplicationOutput ::
  -- | 'currentApplicationVersionId'
  Lude.Natural ->
  -- | 'outputId'
  Lude.Text ->
  -- | 'applicationName'
  Lude.Text ->
  DeleteApplicationOutput
mkDeleteApplicationOutput
  pCurrentApplicationVersionId_
  pOutputId_
  pApplicationName_ =
    DeleteApplicationOutput'
      { currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        outputId = pOutputId_,
        applicationName = pApplicationName_
      }

-- | Amazon Kinesis Analytics application version. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoCurrentApplicationVersionId :: Lens.Lens' DeleteApplicationOutput Lude.Natural
daoCurrentApplicationVersionId = Lens.lens (currentApplicationVersionId :: DeleteApplicationOutput -> Lude.Natural) (\s a -> s {currentApplicationVersionId = a} :: DeleteApplicationOutput)
{-# DEPRECATED daoCurrentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead." #-}

-- | The ID of the configuration to delete. Each output configuration that is added to the application, either when the application is created or later using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationOutput.html AddApplicationOutput> operation, has a unique ID. You need to provide the ID to uniquely identify the output configuration that you want to delete from the application configuration. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the specific @OutputId@ .
--
-- /Note:/ Consider using 'outputId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoOutputId :: Lens.Lens' DeleteApplicationOutput Lude.Text
daoOutputId = Lens.lens (outputId :: DeleteApplicationOutput -> Lude.Text) (\s a -> s {outputId = a} :: DeleteApplicationOutput)
{-# DEPRECATED daoOutputId "Use generic-lens or generic-optics with 'outputId' instead." #-}

-- | Amazon Kinesis Analytics application name.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoApplicationName :: Lens.Lens' DeleteApplicationOutput Lude.Text
daoApplicationName = Lens.lens (applicationName :: DeleteApplicationOutput -> Lude.Text) (\s a -> s {applicationName = a} :: DeleteApplicationOutput)
{-# DEPRECATED daoApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Lude.AWSRequest DeleteApplicationOutput where
  type Rs DeleteApplicationOutput = DeleteApplicationOutputResponse
  request = Req.postJSON kinesisAnalyticsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteApplicationOutputResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteApplicationOutput where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "KinesisAnalytics_20150814.DeleteApplicationOutput" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteApplicationOutput where
  toJSON DeleteApplicationOutput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "CurrentApplicationVersionId"
                  Lude..= currentApplicationVersionId
              ),
            Lude.Just ("OutputId" Lude..= outputId),
            Lude.Just ("ApplicationName" Lude..= applicationName)
          ]
      )

instance Lude.ToPath DeleteApplicationOutput where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteApplicationOutput where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkDeleteApplicationOutputResponse' smart constructor.
newtype DeleteApplicationOutputResponse = DeleteApplicationOutputResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApplicationOutputResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteApplicationOutputResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteApplicationOutputResponse
mkDeleteApplicationOutputResponse pResponseStatus_ =
  DeleteApplicationOutputResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daorsResponseStatus :: Lens.Lens' DeleteApplicationOutputResponse Lude.Int
daorsResponseStatus = Lens.lens (responseStatus :: DeleteApplicationOutputResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteApplicationOutputResponse)
{-# DEPRECATED daorsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
