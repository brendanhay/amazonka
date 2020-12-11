{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.DeleteApplicationReferenceDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a reference data source configuration from the specified application configuration.
--
-- If the application is running, Amazon Kinesis Analytics immediately removes the in-application table that you created using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource> operation.
-- This operation requires permissions to perform the @kinesisanalytics.DeleteApplicationReferenceDataSource@ action.
module Network.AWS.KinesisAnalytics.DeleteApplicationReferenceDataSource
  ( -- * Creating a request
    DeleteApplicationReferenceDataSource (..),
    mkDeleteApplicationReferenceDataSource,

    -- ** Request lenses
    dardsApplicationName,
    dardsCurrentApplicationVersionId,
    dardsReferenceId,

    -- * Destructuring the response
    DeleteApplicationReferenceDataSourceResponse (..),
    mkDeleteApplicationReferenceDataSourceResponse,

    -- ** Response lenses
    dardsrsResponseStatus,
  )
where

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteApplicationReferenceDataSource' smart constructor.
data DeleteApplicationReferenceDataSource = DeleteApplicationReferenceDataSource'
  { applicationName ::
      Lude.Text,
    currentApplicationVersionId ::
      Lude.Natural,
    referenceId ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteApplicationReferenceDataSource' with the minimum fields required to make a request.
--
-- * 'applicationName' - Name of an existing application.
-- * 'currentApplicationVersionId' - Version of the application. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
-- * 'referenceId' - ID of the reference data source. When you add a reference data source to your application using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource> , Amazon Kinesis Analytics assigns an ID. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the reference ID.
mkDeleteApplicationReferenceDataSource ::
  -- | 'applicationName'
  Lude.Text ->
  -- | 'currentApplicationVersionId'
  Lude.Natural ->
  -- | 'referenceId'
  Lude.Text ->
  DeleteApplicationReferenceDataSource
mkDeleteApplicationReferenceDataSource
  pApplicationName_
  pCurrentApplicationVersionId_
  pReferenceId_ =
    DeleteApplicationReferenceDataSource'
      { applicationName =
          pApplicationName_,
        currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        referenceId = pReferenceId_
      }

-- | Name of an existing application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dardsApplicationName :: Lens.Lens' DeleteApplicationReferenceDataSource Lude.Text
dardsApplicationName = Lens.lens (applicationName :: DeleteApplicationReferenceDataSource -> Lude.Text) (\s a -> s {applicationName = a} :: DeleteApplicationReferenceDataSource)
{-# DEPRECATED dardsApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | Version of the application. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dardsCurrentApplicationVersionId :: Lens.Lens' DeleteApplicationReferenceDataSource Lude.Natural
dardsCurrentApplicationVersionId = Lens.lens (currentApplicationVersionId :: DeleteApplicationReferenceDataSource -> Lude.Natural) (\s a -> s {currentApplicationVersionId = a} :: DeleteApplicationReferenceDataSource)
{-# DEPRECATED dardsCurrentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead." #-}

-- | ID of the reference data source. When you add a reference data source to your application using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_AddApplicationReferenceDataSource.html AddApplicationReferenceDataSource> , Amazon Kinesis Analytics assigns an ID. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the reference ID.
--
-- /Note:/ Consider using 'referenceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dardsReferenceId :: Lens.Lens' DeleteApplicationReferenceDataSource Lude.Text
dardsReferenceId = Lens.lens (referenceId :: DeleteApplicationReferenceDataSource -> Lude.Text) (\s a -> s {referenceId = a} :: DeleteApplicationReferenceDataSource)
{-# DEPRECATED dardsReferenceId "Use generic-lens or generic-optics with 'referenceId' instead." #-}

instance Lude.AWSRequest DeleteApplicationReferenceDataSource where
  type
    Rs DeleteApplicationReferenceDataSource =
      DeleteApplicationReferenceDataSourceResponse
  request = Req.postJSON kinesisAnalyticsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteApplicationReferenceDataSourceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteApplicationReferenceDataSource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "KinesisAnalytics_20150814.DeleteApplicationReferenceDataSource" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteApplicationReferenceDataSource where
  toJSON DeleteApplicationReferenceDataSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ApplicationName" Lude..= applicationName),
            Lude.Just
              ( "CurrentApplicationVersionId"
                  Lude..= currentApplicationVersionId
              ),
            Lude.Just ("ReferenceId" Lude..= referenceId)
          ]
      )

instance Lude.ToPath DeleteApplicationReferenceDataSource where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteApplicationReferenceDataSource where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteApplicationReferenceDataSourceResponse' smart constructor.
newtype DeleteApplicationReferenceDataSourceResponse = DeleteApplicationReferenceDataSourceResponse'
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

-- | Creates a value of 'DeleteApplicationReferenceDataSourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteApplicationReferenceDataSourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteApplicationReferenceDataSourceResponse
mkDeleteApplicationReferenceDataSourceResponse pResponseStatus_ =
  DeleteApplicationReferenceDataSourceResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dardsrsResponseStatus :: Lens.Lens' DeleteApplicationReferenceDataSourceResponse Lude.Int
dardsrsResponseStatus = Lens.lens (responseStatus :: DeleteApplicationReferenceDataSourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteApplicationReferenceDataSourceResponse)
{-# DEPRECATED dardsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
