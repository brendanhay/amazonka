{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.DataCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.DataCatalog
  ( DataCatalog (..),

    -- * Smart constructor
    mkDataCatalog,

    -- * Lenses
    dcParameters,
    dcDescription,
    dcName,
    dcType,
  )
where

import Network.AWS.Athena.Types.DataCatalogType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about a data catalog in an AWS account.
--
-- /See:/ 'mkDataCatalog' smart constructor.
data DataCatalog = DataCatalog'
  { parameters ::
      Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    description :: Lude.Maybe Lude.Text,
    name :: Lude.Text,
    type' :: DataCatalogType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DataCatalog' with the minimum fields required to make a request.
--
-- * 'description' - An optional description of the data catalog.
-- * 'name' - The name of the data catalog. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
-- * 'parameters' - Specifies the Lambda function or functions to use for the data catalog. This is a mapping whose values depend on the catalog type.
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
-- * 'type'' - The type of data catalog: @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
mkDataCatalog ::
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  DataCatalogType ->
  DataCatalog
mkDataCatalog pName_ pType_ =
  DataCatalog'
    { parameters = Lude.Nothing,
      description = Lude.Nothing,
      name = pName_,
      type' = pType_
    }

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
dcParameters :: Lens.Lens' DataCatalog (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
dcParameters = Lens.lens (parameters :: DataCatalog -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: DataCatalog)
{-# DEPRECATED dcParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | An optional description of the data catalog.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcDescription :: Lens.Lens' DataCatalog (Lude.Maybe Lude.Text)
dcDescription = Lens.lens (description :: DataCatalog -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DataCatalog)
{-# DEPRECATED dcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the data catalog. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcName :: Lens.Lens' DataCatalog Lude.Text
dcName = Lens.lens (name :: DataCatalog -> Lude.Text) (\s a -> s {name = a} :: DataCatalog)
{-# DEPRECATED dcName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The type of data catalog: @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcType :: Lens.Lens' DataCatalog DataCatalogType
dcType = Lens.lens (type' :: DataCatalog -> DataCatalogType) (\s a -> s {type' = a} :: DataCatalog)
{-# DEPRECATED dcType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON DataCatalog where
  parseJSON =
    Lude.withObject
      "DataCatalog"
      ( \x ->
          DataCatalog'
            Lude.<$> (x Lude..:? "Parameters" Lude..!= Lude.mempty)
            Lude.<*> (x Lude..:? "Description")
            Lude.<*> (x Lude..: "Name")
            Lude.<*> (x Lude..: "Type")
      )
