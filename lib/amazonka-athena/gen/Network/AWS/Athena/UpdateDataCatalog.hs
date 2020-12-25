{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.UpdateDataCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the data catalog that has the specified name.
module Network.AWS.Athena.UpdateDataCatalog
  ( -- * Creating a request
    UpdateDataCatalog (..),
    mkUpdateDataCatalog,

    -- ** Request lenses
    udcName,
    udcType,
    udcDescription,
    udcParameters,

    -- * Destructuring the response
    UpdateDataCatalogResponse (..),
    mkUpdateDataCatalogResponse,

    -- ** Response lenses
    udcrrsResponseStatus,
  )
where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateDataCatalog' smart constructor.
data UpdateDataCatalog = UpdateDataCatalog'
  { -- | The name of the data catalog to update. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
    name :: Types.Name,
    -- | Specifies the type of data catalog to update. Specify @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
    type' :: Types.DataCatalogType,
    -- | New or modified text that describes the data catalog.
    description :: Core.Maybe Types.DescriptionString,
    -- | Specifies the Lambda function or functions to use for updating the data catalog. This is a mapping whose values depend on the catalog type.
    --
    --
    --     * For the @HIVE@ data catalog type, use the following syntax. The @metadata-function@ parameter is required. @The sdk-version@ parameter is optional and defaults to the currently supported version.
    -- @metadata-function=/lambda_arn/ , sdk-version=/version_number/ @
    --
    --
    --     * For the @LAMBDA@ data catalog type, use one of the following sets of required parameters, but not both.
    --
    --     * If you have one Lambda function that processes metadata and another for reading the actual data, use the following syntax. Both parameters are required.
    -- @metadata-function=/lambda_arn/ , record-function=/lambda_arn/ @
    --
    --
    --     * If you have a composite Lambda function that processes both metadata and data, use the following syntax to specify your Lambda function.
    -- @function=/lambda_arn/ @
    --
    --
    --
    --
    --     * The @GLUE@ type has no parameters.
    parameters :: Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDataCatalog' value with any optional fields omitted.
mkUpdateDataCatalog ::
  -- | 'name'
  Types.Name ->
  -- | 'type\''
  Types.DataCatalogType ->
  UpdateDataCatalog
mkUpdateDataCatalog name type' =
  UpdateDataCatalog'
    { name,
      type',
      description = Core.Nothing,
      parameters = Core.Nothing
    }

-- | The name of the data catalog to update. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcName :: Lens.Lens' UpdateDataCatalog Types.Name
udcName = Lens.field @"name"
{-# DEPRECATED udcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Specifies the type of data catalog to update. Specify @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcType :: Lens.Lens' UpdateDataCatalog Types.DataCatalogType
udcType = Lens.field @"type'"
{-# DEPRECATED udcType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | New or modified text that describes the data catalog.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcDescription :: Lens.Lens' UpdateDataCatalog (Core.Maybe Types.DescriptionString)
udcDescription = Lens.field @"description"
{-# DEPRECATED udcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies the Lambda function or functions to use for updating the data catalog. This is a mapping whose values depend on the catalog type.
--
--
--     * For the @HIVE@ data catalog type, use the following syntax. The @metadata-function@ parameter is required. @The sdk-version@ parameter is optional and defaults to the currently supported version.
-- @metadata-function=/lambda_arn/ , sdk-version=/version_number/ @
--
--
--     * For the @LAMBDA@ data catalog type, use one of the following sets of required parameters, but not both.
--
--     * If you have one Lambda function that processes metadata and another for reading the actual data, use the following syntax. Both parameters are required.
-- @metadata-function=/lambda_arn/ , record-function=/lambda_arn/ @
--
--
--     * If you have a composite Lambda function that processes both metadata and data, use the following syntax to specify your Lambda function.
-- @function=/lambda_arn/ @
--
--
--
--
--     * The @GLUE@ type has no parameters.
--
--
--
-- /Note:/ Consider using 'parameters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcParameters :: Lens.Lens' UpdateDataCatalog (Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue))
udcParameters = Lens.field @"parameters"
{-# DEPRECATED udcParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

instance Core.FromJSON UpdateDataCatalog where
  toJSON UpdateDataCatalog {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Type" Core..= type'),
            ("Description" Core..=) Core.<$> description,
            ("Parameters" Core..=) Core.<$> parameters
          ]
      )

instance Core.AWSRequest UpdateDataCatalog where
  type Rs UpdateDataCatalog = UpdateDataCatalogResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonAthena.UpdateDataCatalog")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateDataCatalogResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateDataCatalogResponse' smart constructor.
newtype UpdateDataCatalogResponse = UpdateDataCatalogResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateDataCatalogResponse' value with any optional fields omitted.
mkUpdateDataCatalogResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateDataCatalogResponse
mkUpdateDataCatalogResponse responseStatus =
  UpdateDataCatalogResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcrrsResponseStatus :: Lens.Lens' UpdateDataCatalogResponse Core.Int
udcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED udcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
