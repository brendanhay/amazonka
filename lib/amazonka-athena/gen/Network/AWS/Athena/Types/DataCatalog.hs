{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.DataCatalog
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.DataCatalog where

import Network.AWS.Athena.Types.DataCatalogType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about a data catalog in an AWS account.
--
--
--
-- /See:/ 'dataCatalog' smart constructor.
data DataCatalog = DataCatalog'
  { _dcParameters ::
      !(Maybe (Map Text (Text))),
    _dcDescription :: !(Maybe Text),
    _dcName :: !Text,
    _dcType :: !DataCatalogType
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DataCatalog' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcParameters' - Specifies the Lambda function or functions to use for the data catalog. This is a mapping whose values depend on the catalog type.      * For the @HIVE@ data catalog type, use the following syntax. The @metadata-function@ parameter is required. @The sdk-version@ parameter is optional and defaults to the currently supported version. @metadata-function=/lambda_arn/ , sdk-version=/version_number/ @      * For the @LAMBDA@ data catalog type, use one of the following sets of required parameters, but not both.     * If you have one Lambda function that processes metadata and another for reading the actual data, use the following syntax. Both parameters are required. @metadata-function=/lambda_arn/ , record-function=/lambda_arn/ @      * If you have a composite Lambda function that processes both metadata and data, use the following syntax to specify your Lambda function. @function=/lambda_arn/ @      * The @GLUE@ type has no parameters.
--
-- * 'dcDescription' - An optional description of the data catalog.
--
-- * 'dcName' - The name of the data catalog. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
--
-- * 'dcType' - The type of data catalog: @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
dataCatalog ::
  -- | 'dcName'
  Text ->
  -- | 'dcType'
  DataCatalogType ->
  DataCatalog
dataCatalog pName_ pType_ =
  DataCatalog'
    { _dcParameters = Nothing,
      _dcDescription = Nothing,
      _dcName = pName_,
      _dcType = pType_
    }

-- | Specifies the Lambda function or functions to use for the data catalog. This is a mapping whose values depend on the catalog type.      * For the @HIVE@ data catalog type, use the following syntax. The @metadata-function@ parameter is required. @The sdk-version@ parameter is optional and defaults to the currently supported version. @metadata-function=/lambda_arn/ , sdk-version=/version_number/ @      * For the @LAMBDA@ data catalog type, use one of the following sets of required parameters, but not both.     * If you have one Lambda function that processes metadata and another for reading the actual data, use the following syntax. Both parameters are required. @metadata-function=/lambda_arn/ , record-function=/lambda_arn/ @      * If you have a composite Lambda function that processes both metadata and data, use the following syntax to specify your Lambda function. @function=/lambda_arn/ @      * The @GLUE@ type has no parameters.
dcParameters :: Lens' DataCatalog (HashMap Text (Text))
dcParameters = lens _dcParameters (\s a -> s {_dcParameters = a}) . _Default . _Map

-- | An optional description of the data catalog.
dcDescription :: Lens' DataCatalog (Maybe Text)
dcDescription = lens _dcDescription (\s a -> s {_dcDescription = a})

-- | The name of the data catalog. The catalog name must be unique for the AWS account and can use a maximum of 128 alphanumeric, underscore, at sign, or hyphen characters.
dcName :: Lens' DataCatalog Text
dcName = lens _dcName (\s a -> s {_dcName = a})

-- | The type of data catalog: @LAMBDA@ for a federated catalog, @GLUE@ for AWS Glue Catalog, or @HIVE@ for an external hive metastore.
dcType :: Lens' DataCatalog DataCatalogType
dcType = lens _dcType (\s a -> s {_dcType = a})

instance FromJSON DataCatalog where
  parseJSON =
    withObject
      "DataCatalog"
      ( \x ->
          DataCatalog'
            <$> (x .:? "Parameters" .!= mempty)
            <*> (x .:? "Description")
            <*> (x .: "Name")
            <*> (x .: "Type")
      )

instance Hashable DataCatalog

instance NFData DataCatalog
