{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.UserDefinedFunction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.UserDefinedFunction where

import Network.AWS.Glue.Types.PrincipalType
import Network.AWS.Glue.Types.ResourceURI
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the equivalent of a Hive user-defined function (@UDF@ ) definition.
--
--
--
-- /See:/ 'userDefinedFunction' smart constructor.
data UserDefinedFunction = UserDefinedFunction'
  { _udfOwnerName ::
      !(Maybe Text),
    _udfCatalogId :: !(Maybe Text),
    _udfResourceURIs :: !(Maybe [ResourceURI]),
    _udfDatabaseName :: !(Maybe Text),
    _udfFunctionName :: !(Maybe Text),
    _udfOwnerType :: !(Maybe PrincipalType),
    _udfCreateTime :: !(Maybe POSIX),
    _udfClassName :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UserDefinedFunction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'udfOwnerName' - The owner of the function.
--
-- * 'udfCatalogId' - The ID of the Data Catalog in which the function resides.
--
-- * 'udfResourceURIs' - The resource URIs for the function.
--
-- * 'udfDatabaseName' - The name of the catalog database that contains the function.
--
-- * 'udfFunctionName' - The name of the function.
--
-- * 'udfOwnerType' - The owner type.
--
-- * 'udfCreateTime' - The time at which the function was created.
--
-- * 'udfClassName' - The Java class that contains the function code.
userDefinedFunction ::
  UserDefinedFunction
userDefinedFunction =
  UserDefinedFunction'
    { _udfOwnerName = Nothing,
      _udfCatalogId = Nothing,
      _udfResourceURIs = Nothing,
      _udfDatabaseName = Nothing,
      _udfFunctionName = Nothing,
      _udfOwnerType = Nothing,
      _udfCreateTime = Nothing,
      _udfClassName = Nothing
    }

-- | The owner of the function.
udfOwnerName :: Lens' UserDefinedFunction (Maybe Text)
udfOwnerName = lens _udfOwnerName (\s a -> s {_udfOwnerName = a})

-- | The ID of the Data Catalog in which the function resides.
udfCatalogId :: Lens' UserDefinedFunction (Maybe Text)
udfCatalogId = lens _udfCatalogId (\s a -> s {_udfCatalogId = a})

-- | The resource URIs for the function.
udfResourceURIs :: Lens' UserDefinedFunction [ResourceURI]
udfResourceURIs = lens _udfResourceURIs (\s a -> s {_udfResourceURIs = a}) . _Default . _Coerce

-- | The name of the catalog database that contains the function.
udfDatabaseName :: Lens' UserDefinedFunction (Maybe Text)
udfDatabaseName = lens _udfDatabaseName (\s a -> s {_udfDatabaseName = a})

-- | The name of the function.
udfFunctionName :: Lens' UserDefinedFunction (Maybe Text)
udfFunctionName = lens _udfFunctionName (\s a -> s {_udfFunctionName = a})

-- | The owner type.
udfOwnerType :: Lens' UserDefinedFunction (Maybe PrincipalType)
udfOwnerType = lens _udfOwnerType (\s a -> s {_udfOwnerType = a})

-- | The time at which the function was created.
udfCreateTime :: Lens' UserDefinedFunction (Maybe UTCTime)
udfCreateTime = lens _udfCreateTime (\s a -> s {_udfCreateTime = a}) . mapping _Time

-- | The Java class that contains the function code.
udfClassName :: Lens' UserDefinedFunction (Maybe Text)
udfClassName = lens _udfClassName (\s a -> s {_udfClassName = a})

instance FromJSON UserDefinedFunction where
  parseJSON =
    withObject
      "UserDefinedFunction"
      ( \x ->
          UserDefinedFunction'
            <$> (x .:? "OwnerName")
            <*> (x .:? "CatalogId")
            <*> (x .:? "ResourceUris" .!= mempty)
            <*> (x .:? "DatabaseName")
            <*> (x .:? "FunctionName")
            <*> (x .:? "OwnerType")
            <*> (x .:? "CreateTime")
            <*> (x .:? "ClassName")
      )

instance Hashable UserDefinedFunction

instance NFData UserDefinedFunction
