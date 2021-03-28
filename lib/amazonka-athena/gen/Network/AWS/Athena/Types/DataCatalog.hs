{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.DataCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Athena.Types.DataCatalog
  ( DataCatalog (..)
  -- * Smart constructor
  , mkDataCatalog
  -- * Lenses
  , dcName
  , dcType
  , dcDescription
  , dcParameters
  ) where

import qualified Network.AWS.Athena.Types.DataCatalogType as Types
import qualified Network.AWS.Athena.Types.Description as Types
import qualified Network.AWS.Athena.Types.KeyString as Types
import qualified Network.AWS.Athena.Types.Name as Types
import qualified Network.AWS.Athena.Types.ParametersMapValue as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about a data catalog in an AWS account.
--
-- /See:/ 'mkDataCatalog' smart constructor.
data DataCatalog = DataCatalog'
  { name :: Types.Name
    -- ^ The name of the data catalog. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
  , type' :: Types.DataCatalogType
    -- ^ The type of data catalog: @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
  , description :: Core.Maybe Types.Description
    -- ^ An optional description of the data catalog.
  , parameters :: Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue)
    -- ^ Specifies the Lambda function or functions to use for the data catalog. This is a mapping whose values depend on the catalog type. 
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
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DataCatalog' value with any optional fields omitted.
mkDataCatalog
    :: Types.Name -- ^ 'name'
    -> Types.DataCatalogType -- ^ 'type\''
    -> DataCatalog
mkDataCatalog name type'
  = DataCatalog'{name, type', description = Core.Nothing,
                 parameters = Core.Nothing}

-- | The name of the data catalog. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcName :: Lens.Lens' DataCatalog Types.Name
dcName = Lens.field @"name"
{-# INLINEABLE dcName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The type of data catalog: @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcType :: Lens.Lens' DataCatalog Types.DataCatalogType
dcType = Lens.field @"type'"
{-# INLINEABLE dcType #-}
{-# DEPRECATED type' "Use generic-lens or generic-optics with 'type'' instead"  #-}

-- | An optional description of the data catalog.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDescription :: Lens.Lens' DataCatalog (Core.Maybe Types.Description)
dcDescription = Lens.field @"description"
{-# INLINEABLE dcDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | Specifies the Lambda function or functions to use for the data catalog. This is a mapping whose values depend on the catalog type. 
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
dcParameters :: Lens.Lens' DataCatalog (Core.Maybe (Core.HashMap Types.KeyString Types.ParametersMapValue))
dcParameters = Lens.field @"parameters"
{-# INLINEABLE dcParameters #-}
{-# DEPRECATED parameters "Use generic-lens or generic-optics with 'parameters' instead"  #-}

instance Core.FromJSON DataCatalog where
        parseJSON
          = Core.withObject "DataCatalog" Core.$
              \ x ->
                DataCatalog' Core.<$>
                  (x Core..: "Name") Core.<*> x Core..: "Type" Core.<*>
                    x Core..:? "Description"
                    Core.<*> x Core..:? "Parameters"
