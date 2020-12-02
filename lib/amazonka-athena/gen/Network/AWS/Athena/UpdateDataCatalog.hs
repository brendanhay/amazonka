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
-- Module      : Network.AWS.Athena.UpdateDataCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the data catalog that has the specified name.
module Network.AWS.Athena.UpdateDataCatalog
  ( -- * Creating a Request
    updateDataCatalog,
    UpdateDataCatalog,

    -- * Request Lenses
    udcParameters,
    udcDescription,
    udcName,
    udcType,

    -- * Destructuring the Response
    updateDataCatalogResponse,
    UpdateDataCatalogResponse,

    -- * Response Lenses
    udcrsResponseStatus,
  )
where

import Network.AWS.Athena.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'updateDataCatalog' smart constructor.
data UpdateDataCatalog = UpdateDataCatalog'
  { _udcParameters ::
      !(Maybe (Map Text (Text))),
    _udcDescription :: !(Maybe Text),
    _udcName :: !Text,
    _udcType :: !DataCatalogType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDataCatalog' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udcParameters' - Specifies the Lambda function or functions to use for updating the data catalog. This is a mapping whose values depend on the catalog type.      * For the @HIVE@ data catalog type, use the following syntax. The @metadata-function@ parameter is required. @The sdk-version@ parameter is optional and defaults to the currently supported version. @metadata-function=/lambda_arn/ , sdk-version=/version_number/ @      * For the @LAMBDA@ data catalog type, use one of the following sets of required parameters, but not both.     * If you have one Lambda function that processes metadata and another for reading the actual data, use the following syntax. Both parameters are required. @metadata-function=/lambda_arn/ , record-function=/lambda_arn/ @      * If you have a composite Lambda function that processes both metadata and data, use the following syntax to specify your Lambda function. @function=/lambda_arn/ @      * The @GLUE@ type has no parameters.
--
-- * 'udcDescription' - New or modified text that describes the data catalog.
--
-- * 'udcName' - The name of the data catalog to update. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
--
-- * 'udcType' - Specifies the type of data catalog to update. Specify @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
updateDataCatalog ::
  -- | 'udcName'
  Text ->
  -- | 'udcType'
  DataCatalogType ->
  UpdateDataCatalog
updateDataCatalog pName_ pType_ =
  UpdateDataCatalog'
    { _udcParameters = Nothing,
      _udcDescription = Nothing,
      _udcName = pName_,
      _udcType = pType_
    }

-- | Specifies the Lambda function or functions to use for updating the data catalog. This is a mapping whose values depend on the catalog type.      * For the @HIVE@ data catalog type, use the following syntax. The @metadata-function@ parameter is required. @The sdk-version@ parameter is optional and defaults to the currently supported version. @metadata-function=/lambda_arn/ , sdk-version=/version_number/ @      * For the @LAMBDA@ data catalog type, use one of the following sets of required parameters, but not both.     * If you have one Lambda function that processes metadata and another for reading the actual data, use the following syntax. Both parameters are required. @metadata-function=/lambda_arn/ , record-function=/lambda_arn/ @      * If you have a composite Lambda function that processes both metadata and data, use the following syntax to specify your Lambda function. @function=/lambda_arn/ @      * The @GLUE@ type has no parameters.
udcParameters :: Lens' UpdateDataCatalog (HashMap Text (Text))
udcParameters = lens _udcParameters (\s a -> s {_udcParameters = a}) . _Default . _Map

-- | New or modified text that describes the data catalog.
udcDescription :: Lens' UpdateDataCatalog (Maybe Text)
udcDescription = lens _udcDescription (\s a -> s {_udcDescription = a})

-- | The name of the data catalog to update. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
udcName :: Lens' UpdateDataCatalog Text
udcName = lens _udcName (\s a -> s {_udcName = a})

-- | Specifies the type of data catalog to update. Specify @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
udcType :: Lens' UpdateDataCatalog DataCatalogType
udcType = lens _udcType (\s a -> s {_udcType = a})

instance AWSRequest UpdateDataCatalog where
  type Rs UpdateDataCatalog = UpdateDataCatalogResponse
  request = postJSON athena
  response =
    receiveEmpty
      (\s h x -> UpdateDataCatalogResponse' <$> (pure (fromEnum s)))

instance Hashable UpdateDataCatalog

instance NFData UpdateDataCatalog

instance ToHeaders UpdateDataCatalog where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("AmazonAthena.UpdateDataCatalog" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateDataCatalog where
  toJSON UpdateDataCatalog' {..} =
    object
      ( catMaybes
          [ ("Parameters" .=) <$> _udcParameters,
            ("Description" .=) <$> _udcDescription,
            Just ("Name" .= _udcName),
            Just ("Type" .= _udcType)
          ]
      )

instance ToPath UpdateDataCatalog where
  toPath = const "/"

instance ToQuery UpdateDataCatalog where
  toQuery = const mempty

-- | /See:/ 'updateDataCatalogResponse' smart constructor.
newtype UpdateDataCatalogResponse = UpdateDataCatalogResponse'
  { _udcrsResponseStatus ::
      Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateDataCatalogResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udcrsResponseStatus' - -- | The response status code.
updateDataCatalogResponse ::
  -- | 'udcrsResponseStatus'
  Int ->
  UpdateDataCatalogResponse
updateDataCatalogResponse pResponseStatus_ =
  UpdateDataCatalogResponse'
    { _udcrsResponseStatus =
        pResponseStatus_
    }

-- | -- | The response status code.
udcrsResponseStatus :: Lens' UpdateDataCatalogResponse Int
udcrsResponseStatus = lens _udcrsResponseStatus (\s a -> s {_udcrsResponseStatus = a})

instance NFData UpdateDataCatalogResponse
