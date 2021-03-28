{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoSync.DeleteDataset
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specific dataset. The dataset will be deleted permanently, and the action can't be undone. Datasets that this dataset was merged with will no longer report the merge. Any subsequent operation on this dataset will result in a ResourceNotFoundException.
--
-- This API can be called with temporary user credentials provided by Cognito Identity or with developer credentials.
module Network.AWS.CognitoSync.DeleteDataset
    (
    -- * Creating a request
      DeleteDataset (..)
    , mkDeleteDataset
    -- ** Request lenses
    , ddfIdentityPoolId
    , ddfIdentityId
    , ddfDatasetName

    -- * Destructuring the response
    , DeleteDatasetResponse (..)
    , mkDeleteDatasetResponse
    -- ** Response lenses
    , drsDataset
    , drsResponseStatus
    ) where

import qualified Network.AWS.CognitoSync.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to delete the specific dataset.
--
-- /See:/ 'mkDeleteDataset' smart constructor.
data DeleteDataset = DeleteDataset'
  { identityPoolId :: Types.IdentityPoolId
    -- ^ A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
  , identityId :: Types.IdentityId
    -- ^ A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
  , datasetName :: Types.DatasetName
    -- ^ A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeleteDataset' value with any optional fields omitted.
mkDeleteDataset
    :: Types.IdentityPoolId -- ^ 'identityPoolId'
    -> Types.IdentityId -- ^ 'identityId'
    -> Types.DatasetName -- ^ 'datasetName'
    -> DeleteDataset
mkDeleteDataset identityPoolId identityId datasetName
  = DeleteDataset'{identityPoolId, identityId, datasetName}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityPoolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfIdentityPoolId :: Lens.Lens' DeleteDataset Types.IdentityPoolId
ddfIdentityPoolId = Lens.field @"identityPoolId"
{-# INLINEABLE ddfIdentityPoolId #-}
{-# DEPRECATED identityPoolId "Use generic-lens or generic-optics with 'identityPoolId' instead"  #-}

-- | A name-spaced GUID (for example, us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon Cognito. GUID generation is unique within a region.
--
-- /Note:/ Consider using 'identityId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfIdentityId :: Lens.Lens' DeleteDataset Types.IdentityId
ddfIdentityId = Lens.field @"identityId"
{-# INLINEABLE ddfIdentityId #-}
{-# DEPRECATED identityId "Use generic-lens or generic-optics with 'identityId' instead"  #-}

-- | A string of up to 128 characters. Allowed characters are a-z, A-Z, 0-9, '_' (underscore), '-' (dash), and '.' (dot).
--
-- /Note:/ Consider using 'datasetName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddfDatasetName :: Lens.Lens' DeleteDataset Types.DatasetName
ddfDatasetName = Lens.field @"datasetName"
{-# INLINEABLE ddfDatasetName #-}
{-# DEPRECATED datasetName "Use generic-lens or generic-optics with 'datasetName' instead"  #-}

instance Core.ToQuery DeleteDataset where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DeleteDataset where
        toHeaders DeleteDataset{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest DeleteDataset where
        type Rs DeleteDataset = DeleteDatasetResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.DELETE,
                         Core._rqPath =
                           "/identitypools/" Core.<> Core.toText identityPoolId Core.<>
                             "/identities/"
                             Core.<> Core.toText identityId
                             Core.<> "/datasets/"
                             Core.<> Core.toText datasetName,
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DeleteDatasetResponse' Core.<$>
                   (x Core..:? "Dataset") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Response to a successful DeleteDataset request.
--
-- /See:/ 'mkDeleteDatasetResponse' smart constructor.
data DeleteDatasetResponse = DeleteDatasetResponse'
  { dataset :: Core.Maybe Types.Dataset
    -- ^ A collection of data for an identity pool. An identity pool can have multiple datasets. A dataset is per identity and can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DeleteDatasetResponse' value with any optional fields omitted.
mkDeleteDatasetResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DeleteDatasetResponse
mkDeleteDatasetResponse responseStatus
  = DeleteDatasetResponse'{dataset = Core.Nothing, responseStatus}

-- | A collection of data for an identity pool. An identity pool can have multiple datasets. A dataset is per identity and can be general or associated with a particular entity in an application (like a saved game). Datasets are automatically created if they don't exist. Data is synced by dataset, and a dataset can hold up to 1MB of key-value pairs.
--
-- /Note:/ Consider using 'dataset' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDataset :: Lens.Lens' DeleteDatasetResponse (Core.Maybe Types.Dataset)
drsDataset = Lens.field @"dataset"
{-# INLINEABLE drsDataset #-}
{-# DEPRECATED dataset "Use generic-lens or generic-optics with 'dataset' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DeleteDatasetResponse Core.Int
drsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
