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
    aardsApplicationName,
    aardsCurrentApplicationVersionId,
    aardsReferenceDataSource,

    -- * Destructuring the response
    AddApplicationReferenceDataSourceResponse (..),
    mkAddApplicationReferenceDataSourceResponse,

    -- ** Response lenses
    aardsrrsResponseStatus,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'mkAddApplicationReferenceDataSource' smart constructor.
data AddApplicationReferenceDataSource = AddApplicationReferenceDataSource'
  { -- | Name of an existing application.
    applicationName :: Types.ApplicationName,
    -- | Version of the application for which you are adding the reference data source. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
    currentApplicationVersionId :: Core.Natural,
    -- | The reference data source can be an object in your Amazon S3 bucket. Amazon Kinesis Analytics reads the object and copies the data into the in-application table that is created. You provide an S3 bucket, object key name, and the resulting in-application table that is created. You must also provide an IAM role with the necessary permissions that Amazon Kinesis Analytics can assume to read the object from your S3 bucket on your behalf.
    referenceDataSource :: Types.ReferenceDataSource
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddApplicationReferenceDataSource' value with any optional fields omitted.
mkAddApplicationReferenceDataSource ::
  -- | 'applicationName'
  Types.ApplicationName ->
  -- | 'currentApplicationVersionId'
  Core.Natural ->
  -- | 'referenceDataSource'
  Types.ReferenceDataSource ->
  AddApplicationReferenceDataSource
mkAddApplicationReferenceDataSource
  applicationName
  currentApplicationVersionId
  referenceDataSource =
    AddApplicationReferenceDataSource'
      { applicationName,
        currentApplicationVersionId,
        referenceDataSource
      }

-- | Name of an existing application.
--
-- /Note:/ Consider using 'applicationName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aardsApplicationName :: Lens.Lens' AddApplicationReferenceDataSource Types.ApplicationName
aardsApplicationName = Lens.field @"applicationName"
{-# DEPRECATED aardsApplicationName "Use generic-lens or generic-optics with 'applicationName' instead." #-}

-- | Version of the application for which you are adding the reference data source. You can use the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_DescribeApplication.html DescribeApplication> operation to get the current application version. If the version specified is not the current version, the @ConcurrentModificationException@ is returned.
--
-- /Note:/ Consider using 'currentApplicationVersionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aardsCurrentApplicationVersionId :: Lens.Lens' AddApplicationReferenceDataSource Core.Natural
aardsCurrentApplicationVersionId = Lens.field @"currentApplicationVersionId"
{-# DEPRECATED aardsCurrentApplicationVersionId "Use generic-lens or generic-optics with 'currentApplicationVersionId' instead." #-}

-- | The reference data source can be an object in your Amazon S3 bucket. Amazon Kinesis Analytics reads the object and copies the data into the in-application table that is created. You provide an S3 bucket, object key name, and the resulting in-application table that is created. You must also provide an IAM role with the necessary permissions that Amazon Kinesis Analytics can assume to read the object from your S3 bucket on your behalf.
--
-- /Note:/ Consider using 'referenceDataSource' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aardsReferenceDataSource :: Lens.Lens' AddApplicationReferenceDataSource Types.ReferenceDataSource
aardsReferenceDataSource = Lens.field @"referenceDataSource"
{-# DEPRECATED aardsReferenceDataSource "Use generic-lens or generic-optics with 'referenceDataSource' instead." #-}

instance Core.FromJSON AddApplicationReferenceDataSource where
  toJSON AddApplicationReferenceDataSource {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ApplicationName" Core..= applicationName),
            Core.Just
              ( "CurrentApplicationVersionId"
                  Core..= currentApplicationVersionId
              ),
            Core.Just ("ReferenceDataSource" Core..= referenceDataSource)
          ]
      )

instance Core.AWSRequest AddApplicationReferenceDataSource where
  type
    Rs AddApplicationReferenceDataSource =
      AddApplicationReferenceDataSourceResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "KinesisAnalytics_20150814.AddApplicationReferenceDataSource"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          AddApplicationReferenceDataSourceResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- |
--
-- /See:/ 'mkAddApplicationReferenceDataSourceResponse' smart constructor.
newtype AddApplicationReferenceDataSourceResponse = AddApplicationReferenceDataSourceResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'AddApplicationReferenceDataSourceResponse' value with any optional fields omitted.
mkAddApplicationReferenceDataSourceResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AddApplicationReferenceDataSourceResponse
mkAddApplicationReferenceDataSourceResponse responseStatus =
  AddApplicationReferenceDataSourceResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aardsrrsResponseStatus :: Lens.Lens' AddApplicationReferenceDataSourceResponse Core.Int
aardsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED aardsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
