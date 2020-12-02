{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.SupportedEndpointType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.SupportedEndpointType where

import Network.AWS.DMS.Types.ReplicationEndpointTypeValue
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about types of supported endpoints in response to a request by the @DescribeEndpointTypes@ operation. This information includes the type of endpoint, the database engine name, and whether change data capture (CDC) is supported.
--
--
--
-- /See:/ 'supportedEndpointType' smart constructor.
data SupportedEndpointType = SupportedEndpointType'
  { _setEngineDisplayName ::
      !(Maybe Text),
    _setEndpointType ::
      !(Maybe ReplicationEndpointTypeValue),
    _setEngineName :: !(Maybe Text),
    _setReplicationInstanceEngineMinimumVersion ::
      !(Maybe Text),
    _setSupportsCDC :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SupportedEndpointType' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'setEngineDisplayName' - The expanded name for the engine name. For example, if the @EngineName@ parameter is "aurora," this value would be "Amazon Aurora MySQL."
--
-- * 'setEndpointType' - The type of endpoint. Valid values are @source@ and @target@ .
--
-- * 'setEngineName' - The database engine name. Valid values, depending on the EndpointType, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"documentdb"@ , @"sqlserver"@ , and @"neptune"@ .
--
-- * 'setReplicationInstanceEngineMinimumVersion' - The earliest AWS DMS engine version that supports this endpoint engine. Note that endpoint engines released with AWS DMS versions earlier than 3.1.1 do not return a value for this parameter.
--
-- * 'setSupportsCDC' - Indicates if Change Data Capture (CDC) is supported.
supportedEndpointType ::
  SupportedEndpointType
supportedEndpointType =
  SupportedEndpointType'
    { _setEngineDisplayName = Nothing,
      _setEndpointType = Nothing,
      _setEngineName = Nothing,
      _setReplicationInstanceEngineMinimumVersion = Nothing,
      _setSupportsCDC = Nothing
    }

-- | The expanded name for the engine name. For example, if the @EngineName@ parameter is "aurora," this value would be "Amazon Aurora MySQL."
setEngineDisplayName :: Lens' SupportedEndpointType (Maybe Text)
setEngineDisplayName = lens _setEngineDisplayName (\s a -> s {_setEngineDisplayName = a})

-- | The type of endpoint. Valid values are @source@ and @target@ .
setEndpointType :: Lens' SupportedEndpointType (Maybe ReplicationEndpointTypeValue)
setEndpointType = lens _setEndpointType (\s a -> s {_setEndpointType = a})

-- | The database engine name. Valid values, depending on the EndpointType, include @"mysql"@ , @"oracle"@ , @"postgres"@ , @"mariadb"@ , @"aurora"@ , @"aurora-postgresql"@ , @"redshift"@ , @"s3"@ , @"db2"@ , @"azuredb"@ , @"sybase"@ , @"dynamodb"@ , @"mongodb"@ , @"kinesis"@ , @"kafka"@ , @"elasticsearch"@ , @"documentdb"@ , @"sqlserver"@ , and @"neptune"@ .
setEngineName :: Lens' SupportedEndpointType (Maybe Text)
setEngineName = lens _setEngineName (\s a -> s {_setEngineName = a})

-- | The earliest AWS DMS engine version that supports this endpoint engine. Note that endpoint engines released with AWS DMS versions earlier than 3.1.1 do not return a value for this parameter.
setReplicationInstanceEngineMinimumVersion :: Lens' SupportedEndpointType (Maybe Text)
setReplicationInstanceEngineMinimumVersion = lens _setReplicationInstanceEngineMinimumVersion (\s a -> s {_setReplicationInstanceEngineMinimumVersion = a})

-- | Indicates if Change Data Capture (CDC) is supported.
setSupportsCDC :: Lens' SupportedEndpointType (Maybe Bool)
setSupportsCDC = lens _setSupportsCDC (\s a -> s {_setSupportsCDC = a})

instance FromJSON SupportedEndpointType where
  parseJSON =
    withObject
      "SupportedEndpointType"
      ( \x ->
          SupportedEndpointType'
            <$> (x .:? "EngineDisplayName")
            <*> (x .:? "EndpointType")
            <*> (x .:? "EngineName")
            <*> (x .:? "ReplicationInstanceEngineMinimumVersion")
            <*> (x .:? "SupportsCDC")
      )

instance Hashable SupportedEndpointType

instance NFData SupportedEndpointType
