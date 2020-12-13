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
    udcParameters,
    udcType,
    udcDescription,

    -- * Destructuring the response
    UpdateDataCatalogResponse (..),
    mkUpdateDataCatalogResponse,

    -- ** Response lenses
    udcrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDataCatalog' smart constructor.
data UpdateDataCatalog = UpdateDataCatalog'
  { -- | The name of the data catalog to update. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
    name :: Lude.Text,
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
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | Specifies the type of data catalog to update. Specify @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
    type' :: DataCatalogType,
    -- | New or modified text that describes the data catalog.
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDataCatalog' with the minimum fields required to make a request.
--
-- * 'name' - The name of the data catalog to update. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
-- * 'parameters' - Specifies the Lambda function or functions to use for updating the data catalog. This is a mapping whose values depend on the catalog type.
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
-- * 'type'' - Specifies the type of data catalog to update. Specify @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
-- * 'description' - New or modified text that describes the data catalog.
mkUpdateDataCatalog ::
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  DataCatalogType ->
  UpdateDataCatalog
mkUpdateDataCatalog pName_ pType_ =
  UpdateDataCatalog'
    { name = pName_,
      parameters = Lude.Nothing,
      type' = pType_,
      description = Lude.Nothing
    }

-- | The name of the data catalog to update. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcName :: Lens.Lens' UpdateDataCatalog Lude.Text
udcName = Lens.lens (name :: UpdateDataCatalog -> Lude.Text) (\s a -> s {name = a} :: UpdateDataCatalog)
{-# DEPRECATED udcName "Use generic-lens or generic-optics with 'name' instead." #-}

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
udcParameters :: Lens.Lens' UpdateDataCatalog (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
udcParameters = Lens.lens (parameters :: UpdateDataCatalog -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: UpdateDataCatalog)
{-# DEPRECATED udcParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | Specifies the type of data catalog to update. Specify @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcType :: Lens.Lens' UpdateDataCatalog DataCatalogType
udcType = Lens.lens (type' :: UpdateDataCatalog -> DataCatalogType) (\s a -> s {type' = a} :: UpdateDataCatalog)
{-# DEPRECATED udcType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | New or modified text that describes the data catalog.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcDescription :: Lens.Lens' UpdateDataCatalog (Lude.Maybe Lude.Text)
udcDescription = Lens.lens (description :: UpdateDataCatalog -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateDataCatalog)
{-# DEPRECATED udcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateDataCatalog where
  type Rs UpdateDataCatalog = UpdateDataCatalogResponse
  request = Req.postJSON athenaService
  response =
    Res.receiveEmpty
      ( \s h x ->
          UpdateDataCatalogResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDataCatalog where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonAthena.UpdateDataCatalog" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDataCatalog where
  toJSON UpdateDataCatalog' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            ("Parameters" Lude..=) Lude.<$> parameters,
            Lude.Just ("Type" Lude..= type'),
            ("Description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateDataCatalog where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDataCatalog where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDataCatalogResponse' smart constructor.
newtype UpdateDataCatalogResponse = UpdateDataCatalogResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDataCatalogResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkUpdateDataCatalogResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDataCatalogResponse
mkUpdateDataCatalogResponse pResponseStatus_ =
  UpdateDataCatalogResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcrsResponseStatus :: Lens.Lens' UpdateDataCatalogResponse Lude.Int
udcrsResponseStatus = Lens.lens (responseStatus :: UpdateDataCatalogResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDataCatalogResponse)
{-# DEPRECATED udcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
