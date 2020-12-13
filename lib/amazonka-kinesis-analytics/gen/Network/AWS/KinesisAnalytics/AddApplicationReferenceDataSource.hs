{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.AddApplicationReferenceDataSource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds a reference data source to an existing application.
--
-- Amazon Kinesis Analytics reads reference data (that is, an Amazon S3 object) and creates an in-application table within your application. In the request, you provide the source (S3 bucket name and object key name), name of the in-application table to create, and the necessary mapping information that describes how data in Amazon S3 object maps to columns in the resulting in-application table.
-- For conceptual information, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/how-it-works-input.html Configuring Application Input> . For the limits on data sources you can add to your application, see <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/limits.html Limits> .
-- This operation requires permissions to perform the @kinesisanalytics:AddApplicationOutput@ action.
module Network.AWS.KinesisAnalytics.AddApplicationReferenceDataSource
  ( -- * Creating a request
    AddApplicationReferenceDataSource (..),
    mkAddApplicationReferenceDataSource,

    -- ** Request lenses
    aardsCurrentApplicationVersionId,
    aardsReferenceDataSource,
    aardsApplicationName,

    -- * Destructuring the response
    AddApplicationReferenceDataSourceResponse (..),
    mkAddApplicationReferenceDataSourceResponse,

    -- ** Response lenses
    aardsrsResponseStatus,
  )
where

import Network.AWS.KinesisAnalytics.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkAddApplicationReferenceDataSource' smart constructor.
data AddApplicationReferenceDataSource = AddApplicationReferenceDataSource'
  { -- | Version of the application for which you are adding the reference data source. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
    currentApplicationVersionId :: Lude.Natural,
    -- | The reference data source can be an object in your Amazon S3 bucket. Amazon Kinesis Analytics reads the object and copies the data into the in-application table that is created. You provide an S3 bucket, object key name, and the resulting in-application table that is created. You must also provide an IAM role with the necessary permissions that Amazon Kinesis Analytics can assume to read the object from your S3 bucket on your behalf.
    referenceDataSource :: ReferenceDataSource,
    -- | Name of an existing application.
    applicationName :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddApplicationReferenceDataSource' with the minimum fields required to make a request.
--
-- * 'currentApplicationVersionId' - Version of the application for which you are adding the reference data source. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
-- * 'referenceDataSource' - The reference data source can be an object in your Amazon S3 bucket. Amazon Kinesis Analytics reads the object and copies the data into the in-application table that is created. You provide an S3 bucket, object key name, and the resulting in-application table that is created. You must also provide an IAM role with the necessary permissions that Amazon Kinesis Analytics can assume to read the object from your S3 bucket on your behalf.
-- * 'applicationName' - Name of an existing application.
mkAddApplicationReferenceDataSource ::
  -- | 'currentApplicationVersionId'
  Lude.Natural ->
  -- | 'referenceDataSource'
  ReferenceDataSource ->
  -- | 'applicationName'
  Lude.Text ->
  AddApplicationReferenceDataSource
mkAddApplicationReferenceDataSource
  pCurrentApplicationVersionId_
  pReferenceDataSource_
  pApplicationName_ =
    AddApplicationReferenceDataSource'
      { currentApplicationVersionId =
          pCurrentApplicationVersionId_,
        referenceDataSource = pReferenceDataSource_,
        applicationName = pApplicationName_
      }

-- | Version of the application for which you are adding the reference data source. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aardsCurrentApplicationVersionId :: Lens.Lens' AddApplicationReferenceDataSource Lude.Natural
aardsCurrentApplicationVersionId = Lens.lens (currentApplicationVersionId :: AddApplicationReferenceDataSource -> Lude.Natural) (\s a -> s {currentApplicationVersionId = a} :: AddApplicationReferenceDataSource)
{-# DEPRECATED aardsCurrentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead." #-}

-- | The reference data source can be an object in your Amazon S3 bucket. Amazon Kinesis Analytics reads the object and copies the data into the in-application table that is created. You provide an S3 bucket, object key name, and the resulting in-application table that is created. You must also provide an IAM role with the necessary permissions that Amazon Kinesis Analytics can assume to read the object from your S3 bucket on your behalf.
--
-- /Note:/ Consider using 'referenceDataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aardsReferenceDataSource :: Lens.Lens' AddApplicationReferenceDataSource ReferenceDataSource
aardsReferenceDataSource = Lens.lens (referenceDataSource :: AddApplicationReferenceDataSource -> ReferenceDataSource) (\s a -> s {referenceDataSource = a} :: AddApplicationReferenceDataSource)
{-# DEPRECATED aardsReferenceDataSource "Use generic-lens or generic-optics with 'referenceDataSource' instead." #-}

-- | Name of an existing application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aardsApplicationName :: Lens.Lens' AddApplicationReferenceDataSource Lude.Text
aardsApplicationName = Lens.lens (applicationName :: AddApplicationReferenceDataSource -> Lude.Text) (\s a -> s {applicationName = a} :: AddApplicationReferenceDataSource)
{-# DEPRECATED aardsApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

instance Lude.AWSRequest AddApplicationReferenceDataSource where
  type
    Rs AddApplicationReferenceDataSource =
      AddApplicationReferenceDataSourceResponse
  request = Req.postJSON kinesisAnalyticsService
  response =
    Res.receiveEmpty
      ( \s h x ->
          AddApplicationReferenceDataSourceResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddApplicationReferenceDataSource where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "KinesisAnalytics_20150814.AddApplicationReferenceDataSource" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddApplicationReferenceDataSource where
  toJSON AddApplicationReferenceDataSource' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ( "CurrentApplicationVersionId"
                  Lude..= currentApplicationVersionId
              ),
            Lude.Just ("ReferenceDataSource" Lude..= referenceDataSource),
            Lude.Just ("ApplicationName" Lude..= applicationName)
          ]
      )

instance Lude.ToPath AddApplicationReferenceDataSource where
  toPath = Lude.const "/"

instance Lude.ToQuery AddApplicationReferenceDataSource where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkAddApplicationReferenceDataSourceResponse' smart constructor.
newtype AddApplicationReferenceDataSourceResponse = AddApplicationReferenceDataSourceResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddApplicationReferenceDataSourceResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkAddApplicationReferenceDataSourceResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddApplicationReferenceDataSourceResponse
mkAddApplicationReferenceDataSourceResponse pResponseStatus_ =
  AddApplicationReferenceDataSourceResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aardsrsResponseStatus :: Lens.Lens' AddApplicationReferenceDataSourceResponse Lude.Int
aardsrsResponseStatus = Lens.lens (responseStatus :: AddApplicationReferenceDataSourceResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddApplicationReferenceDataSourceResponse)
{-# DEPRECATED aardsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
