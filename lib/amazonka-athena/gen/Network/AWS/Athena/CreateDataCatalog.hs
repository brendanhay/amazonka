{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.CreateDataCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates (registers) a data catalog with the specified name and properties. Catalogs created are visible to all users of the same AWS account.
module Network.AWS.Athena.CreateDataCatalog
  ( -- * Creating a request
    CreateDataCatalog (..),
    mkCreateDataCatalog,

    -- ** Request lenses
    cdcName,
    cdcType,
    cdcDescription,
    cdcParameters,
    cdcTags,

    -- * Destructuring the response
    CreateDataCatalogResponse (..),
    mkCreateDataCatalogResponse,

    -- ** Response lenses
    cdcrrsResponseStatus,
  )
where

import qualified Network.AWS.Athena.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkCreateDataCatalog' smart constructor.
data CreateDataCatalog = CreateDataCatalog'
  { -- | The name of the data catalog to create. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
    name :: Types.Name,
    -- | The type of data catalog to create: @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
    type' :: Types.DataCatalogType,
    -- | A description of the data catalog to be created.
    description :: Core.Maybe Types.DescriptionString,
    -- | Specifies the Lambda function or functions to use for creating the data catalog. This is a mapping whose values depend on the catalog type.
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
    parameters :: Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue),
    -- | A list of comma separated tags to add to the data catalog that is created.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDataCatalog' value with any optional fields omitted.
mkCreateDataCatalog ::
  -- | 'name'
  Types.Name ->
  -- | 'type\''
  Types.DataCatalogType ->
  CreateDataCatalog
mkCreateDataCatalog name type' =
  CreateDataCatalog'
    { name,
      type',
      description = Core.Nothing,
      parameters = Core.Nothing,
      tags = Core.Nothing
    }

-- | The name of the data catalog to create. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcName :: Lens.Lens' CreateDataCatalog Types.Name
cdcName = Lens.field @"name"
{-# DEPRECATED cdcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of data catalog to create: @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcType :: Lens.Lens' CreateDataCatalog Types.DataCatalogType
cdcType = Lens.field @"type'"
{-# DEPRECATED cdcType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A description of the data catalog to be created.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDescription :: Lens.Lens' CreateDataCatalog (Core.Maybe Types.DescriptionString)
cdcDescription = Lens.field @"description"
{-# DEPRECATED cdcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Specifies the Lambda function or functions to use for creating the data catalog. This is a mapping whose values depend on the catalog type.
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
cdcParameters :: Lens.Lens' CreateDataCatalog (Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue))
cdcParameters = Lens.field @"parameters"
{-# DEPRECATED cdcParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | A list of comma separated tags to add to the data catalog that is created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcTags :: Lens.Lens' CreateDataCatalog (Core.Maybe [Types.Tag])
cdcTags = Lens.field @"tags"
{-# DEPRECATED cdcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON CreateDataCatalog where
  toJSON CreateDataCatalog {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("Name" Core..= name),
            Core.Just ("Type" Core..= type'),
            ("Description" Core..=) Core.<$> description,
            ("Parameters" Core..=) Core.<$> parameters,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest CreateDataCatalog where
  type Rs CreateDataCatalog = CreateDataCatalogResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AmazonAthena.CreateDataCatalog")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          CreateDataCatalogResponse' Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkCreateDataCatalogResponse' smart constructor.
newtype CreateDataCatalogResponse = CreateDataCatalogResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateDataCatalogResponse' value with any optional fields omitted.
mkCreateDataCatalogResponse ::
  -- | 'responseStatus'
  Core.Int ->
  CreateDataCatalogResponse
mkCreateDataCatalogResponse responseStatus =
  CreateDataCatalogResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrrsResponseStatus :: Lens.Lens' CreateDataCatalogResponse Core.Int
cdcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED cdcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
