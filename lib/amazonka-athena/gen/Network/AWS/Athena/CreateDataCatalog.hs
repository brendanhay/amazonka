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
    cdcParameters,
    cdcType,
    cdcDescription,
    cdcTags,

    -- * Destructuring the response
    CreateDataCatalogResponse (..),
    mkCreateDataCatalogResponse,

    -- ** Response lenses
    cdcrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDataCatalog' smart constructor.
data CreateDataCatalog = CreateDataCatalog'
  { -- | The name of the data catalog to create. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
    name :: Lude.Text,
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
    parameters :: Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)),
    -- | The type of data catalog to create: @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
    type' :: DataCatalogType,
    -- | A description of the data catalog to be created.
    description :: Lude.Maybe Lude.Text,
    -- | A list of comma separated tags to add to the data catalog that is created.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDataCatalog' with the minimum fields required to make a request.
--
-- * 'name' - The name of the data catalog to create. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
-- * 'parameters' - Specifies the Lambda function or functions to use for creating the data catalog. This is a mapping whose values depend on the catalog type.
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
-- * 'type'' - The type of data catalog to create: @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
-- * 'description' - A description of the data catalog to be created.
-- * 'tags' - A list of comma separated tags to add to the data catalog that is created.
mkCreateDataCatalog ::
  -- | 'name'
  Lude.Text ->
  -- | 'type''
  DataCatalogType ->
  CreateDataCatalog
mkCreateDataCatalog pName_ pType_ =
  CreateDataCatalog'
    { name = pName_,
      parameters = Lude.Nothing,
      type' = pType_,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The name of the data catalog to create. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcName :: Lens.Lens' CreateDataCatalog Lude.Text
cdcName = Lens.lens (name :: CreateDataCatalog -> Lude.Text) (\s a -> s {name = a} :: CreateDataCatalog)
{-# DEPRECATED cdcName "Use generic-lens or generic-optics with 'name' instead." #-}

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
cdcParameters :: Lens.Lens' CreateDataCatalog (Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text)))
cdcParameters = Lens.lens (parameters :: CreateDataCatalog -> Lude.Maybe (Lude.HashMap Lude.Text (Lude.Text))) (\s a -> s {parameters = a} :: CreateDataCatalog)
{-# DEPRECATED cdcParameters "Use generic-lens or generic-optics with 'parameters' instead." #-}

-- | The type of data catalog to create: @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcType :: Lens.Lens' CreateDataCatalog DataCatalogType
cdcType = Lens.lens (type' :: CreateDataCatalog -> DataCatalogType) (\s a -> s {type' = a} :: CreateDataCatalog)
{-# DEPRECATED cdcType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | A description of the data catalog to be created.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcDescription :: Lens.Lens' CreateDataCatalog (Lude.Maybe Lude.Text)
cdcDescription = Lens.lens (description :: CreateDataCatalog -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateDataCatalog)
{-# DEPRECATED cdcDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of comma separated tags to add to the data catalog that is created.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcTags :: Lens.Lens' CreateDataCatalog (Lude.Maybe [Tag])
cdcTags = Lens.lens (tags :: CreateDataCatalog -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateDataCatalog)
{-# DEPRECATED cdcTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest CreateDataCatalog where
  type Rs CreateDataCatalog = CreateDataCatalogResponse
  request = Req.postJSON athenaService
  response =
    Res.receiveEmpty
      ( \s h x ->
          CreateDataCatalogResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDataCatalog where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonAthena.CreateDataCatalog" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDataCatalog where
  toJSON CreateDataCatalog' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Name" Lude..= name),
            ("Parameters" Lude..=) Lude.<$> parameters,
            Lude.Just ("Type" Lude..= type'),
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath CreateDataCatalog where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDataCatalog where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDataCatalogResponse' smart constructor.
newtype CreateDataCatalogResponse = CreateDataCatalogResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDataCatalogResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkCreateDataCatalogResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDataCatalogResponse
mkCreateDataCatalogResponse pResponseStatus_ =
  CreateDataCatalogResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcrsResponseStatus :: Lens.Lens' CreateDataCatalogResponse Lude.Int
cdcrsResponseStatus = Lens.lens (responseStatus :: CreateDataCatalogResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDataCatalogResponse)
{-# DEPRECATED cdcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
