{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
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
  ( -- * Creating a Request
    createDataCatalog,
    CreateDataCatalog,

    -- * Request Lenses
    cdcParameters,
    cdcDescription,
    cdcTags,
    cdcName,
    cdcType,

    -- * Destructuring the Response
    createDataCatalogResponse,
    CreateDataCatalogResponse,

    -- * Response Lenses
    cdcrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'createDataCatalog' smart constructor.
data CreateDataCatalog = CreateDataCatalog'
  { _cdcParameters ::
      !(Maybe (Map Text (Text))),
    _cdcDescription :: !(Maybe Text),
    _cdcTags :: !(Maybe [Tag]),
    _cdcName :: !Text,
    _cdcType :: !DataCatalogType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDataCatalog' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcParameters' - Specifies the Lambda function or functions to use for creating the data catalog. This is a mapping whose values depend on the catalog type.      * For the @HIVE@ data catalog type, use the following syntax. The @metadata-function@ parameter is required. @The sdk-version@ parameter is optional and defaults to the currently supported version. @metadata-function=/lambda_arn/ , sdk-version=/version_number/ @      * For the @LAMBDA@ data catalog type, use one of the following sets of required parameters, but not both.     * If you have one Lambda function that processes metadata and another for reading the actual data, use the following syntax. Both parameters are required. @metadata-function=/lambda_arn/ , record-function=/lambda_arn/ @      * If you have a composite Lambda function that processes both metadata and data, use the following syntax to specify your Lambda function. @function=/lambda_arn/ @      * The @GLUE@ type has no parameters.
--
-- * 'cdcDescription' - A description of the data catalog to be created.
--
-- * 'cdcTags' - A list of comma separated tags to add to the data catalog that is created.
--
-- * 'cdcName' - The name of the data catalog to create. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
--
-- * 'cdcType' - The type of data catalog to create: @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
createDataCatalog ::
  -- | 'cdcName'
  Text ->
  -- | 'cdcType'
  DataCatalogType ->
  CreateDataCatalog
createDataCatalog pName_ pType_ =
  CreateDataCatalog'
    { _cdcParameters = Nothing,
      _cdcDescription = Nothing,
      _cdcTags = Nothing,
      _cdcName = pName_,
      _cdcType = pType_
    }

-- | Specifies the Lambda function or functions to use for creating the data catalog. This is a mapping whose values depend on the catalog type.      * For the @HIVE@ data catalog type, use the following syntax. The @metadata-function@ parameter is required. @The sdk-version@ parameter is optional and defaults to the currently supported version. @metadata-function=/lambda_arn/ , sdk-version=/version_number/ @      * For the @LAMBDA@ data catalog type, use one of the following sets of required parameters, but not both.     * If you have one Lambda function that processes metadata and another for reading the actual data, use the following syntax. Both parameters are required. @metadata-function=/lambda_arn/ , record-function=/lambda_arn/ @      * If you have a composite Lambda function that processes both metadata and data, use the following syntax to specify your Lambda function. @function=/lambda_arn/ @      * The @GLUE@ type has no parameters.
cdcParameters :: Lens' CreateDataCatalog (HashMap Text (Text))
cdcParameters = lens _cdcParameters (\s a -> s {_cdcParameters = a}) . _Default . _Map

-- | A description of the data catalog to be created.
cdcDescription :: Lens' CreateDataCatalog (Maybe Text)
cdcDescription = lens _cdcDescription (\s a -> s {_cdcDescription = a})

-- | A list of comma separated tags to add to the data catalog that is created.
cdcTags :: Lens' CreateDataCatalog [Tag]
cdcTags = lens _cdcTags (\s a -> s {_cdcTags = a}) . _Default . _Coerce

-- | The name of the data catalog to create. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
cdcName :: Lens' CreateDataCatalog Text
cdcName = lens _cdcName (\s a -> s {_cdcName = a})

-- | The type of data catalog to create: @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
cdcType :: Lens' CreateDataCatalog DataCatalogType
cdcType = lens _cdcType (\s a -> s {_cdcType = a})

instance AWSRequest CreateDataCatalog where
  type Rs CreateDataCatalog = CreateDataCatalogResponse
  request = postJSON athena
  response =
    receiveEmpty
      (\s h x -> CreateDataCatalogResponse' <$> (pure (fromEnum s)))

instance Hashable CreateDataCatalog

instance NFData CreateDataCatalog

instance ToHeaders CreateDataCatalog where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonAthena.CreateDataCatalog" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON CreateDataCatalog where
  toJSON CreateDataCatalog' {..} =
    object
      ( catMaybes
          [ ("Parameters" .=) <$> _cdcParameters,
            ("Description" .=) <$> _cdcDescription,
            ("Tags" .=) <$> _cdcTags,
            Just ("Name" .= _cdcName),
            Just ("Type" .= _cdcType)
          ]
      )

instance ToPath CreateDataCatalog where
  toPath = const "/"

instance ToQuery CreateDataCatalog where
  toQuery = const mempty

-- | /See:/ 'createDataCatalogResponse' smart constructor.
newtype CreateDataCatalogResponse = CreateDataCatalogResponse'
  { _cdcrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'CreateDataCatalogResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cdcrsResponseStatus' - -- | The response status code.
createDataCatalogResponse ::
  -- | 'cdcrsResponseStatus'
  Int ->
  CreateDataCatalogResponse
createDataCatalogResponse pResponseStatus_ =
  CreateDataCatalogResponse'
    { _cdcrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
cdcrsResponseStatus :: Lens' CreateDataCatalogResponse Int
cdcrsResponseStatus = lens _cdcrsResponseStatus (\s a -> s {_cdcrsResponseStatus = a})

instance NFData CreateDataCatalogResponse
