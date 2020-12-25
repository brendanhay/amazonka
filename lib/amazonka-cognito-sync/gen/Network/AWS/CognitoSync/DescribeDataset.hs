{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.DescribeDataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets meta data about a dataset by identity and dataset name. With Amazon Cognito Sync, each identity has access only to its own data. Thus, the credentials used to make this API call need to have access to the identity data.
--
-- This API can be called with temporary user credentials provided by Cognito Identity or with developer credentials. You should use Cognito Identity credentials to make this API call.
module Network.AWS.CognitoSync.DescribeDataset
  ( -- * Creating a request
    DescribeDataset (..),
    mkDescribeDataset,

    -- ** Request lenses
    ddIdentityPoolId,
    ddIdentityId,
    ddDatasetName,

    -- * Destructuring the response
    DescribeDatasetResponse (..),
    mkDescribeDatasetResponse,

    -- ** Response lenses
    ddrrsDataset,
    ddrrsResponseStatus,
  )
where

import qualified Network.AWS.CognitoSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request for meta data about a dataset (creation date, number of records, size) by owner and dataset name.
--
-- /See:/ 'mkDescribeDataset' smart constructor.
data DescribeDataset = DescribeDataset'
  { -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityPoolId :: Types.IdentityPoolId,
    -- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
    identityId :: Types.IdentityId,
    -- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
    datasetName :: Types.DatasetName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeDataset' value with any optional fields omitted.
mkDescribeDataset ::
  -- | 'identityPoolId'
  Types.IdentityPoolId ->
  -- | 'identityId'
  Types.IdentityId ->
  -- | 'datasetName'
  Types.DatasetName ->
  DescribeDataset
mkDescribeDataset identityPoolId identityId datasetName =
  DescribeDataset' {identityPoolId, identityId, datasetName}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddIdentityPoolId :: Lens.Lens' DescribeDataset Types.IdentityPoolId
ddIdentityPoolId = Lens.field @"identityPoolId"
{-# DEPRECATED ddIdentityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead." #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddIdentityId :: Lens.Lens' DescribeDataset Types.IdentityId
ddIdentityId = Lens.field @"identityId"
{-# DEPRECATED ddIdentityId "Use generic-lens or generic-optics with 'identityId' instead." #-}

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddDatasetName :: Lens.Lens' DescribeDataset Types.DatasetName
ddDatasetName = Lens.field @"datasetName"
{-# DEPRECATED ddDatasetName "Use generic-lens or generic-optics with 'datasetName' instead." #-}

instance Core.AWSRequest DescribeDataset where
  type Rs DescribeDataset = DescribeDatasetResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/identitypools/" Core.<> (Core.toText identityPoolId)
                Core.<> ("/identities/")
                Core.<> (Core.toText identityId)
                Core.<> ("/datasets/")
                Core.<> (Core.toText datasetName)
            ),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDatasetResponse'
            Core.<$> (x Core..:? "Dataset") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | Response to a successful DescribeDataset request.
--
-- /See:/ 'mkDescribeDatasetResponse' smart constructor.
data DescribeDatasetResponse = DescribeDatasetResponse'
  { -- | Meta data for a collection of data for an identity. An identity can have multiple datasets. A dataset can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
    dataset :: Core.Maybe Types.Dataset,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'DescribeDatasetResponse' value with any optional fields omitted.
mkDescribeDatasetResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeDatasetResponse
mkDescribeDatasetResponse responseStatus =
  DescribeDatasetResponse' {dataset = Core.Nothing, responseStatus}

-- | Meta data for a collection of data for an identity. An identity can have multiple datasets. A dataset can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
--
-- /Note:/ Consider using 'dataset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsDataset :: Lens.Lens' DescribeDatasetResponse (Core.Maybe Types.Dataset)
ddrrsDataset = Lens.field @"dataset"
{-# DEPRECATED ddrrsDataset "Use generic-lens or generic-optics with 'dataset' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddrrsResponseStatus :: Lens.Lens' DescribeDatasetResponse Core.Int
ddrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ddrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
