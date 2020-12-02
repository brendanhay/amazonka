{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.RDSHTTPEndpointConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AppSync.Types.RDSHTTPEndpointConfig where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The Amazon RDS HTTP endpoint configuration.
--
--
--
-- /See:/ 'rdsHTTPEndpointConfig' smart constructor.
data RDSHTTPEndpointConfig = RDSHTTPEndpointConfig'
  { _rhttpecDbClusterIdentifier ::
      !(Maybe Text),
    _rhttpecSchema :: !(Maybe Text),
    _rhttpecDatabaseName :: !(Maybe Text),
    _rhttpecAwsRegion :: !(Maybe Text),
    _rhttpecAwsSecretStoreARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RDSHTTPEndpointConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rhttpecDbClusterIdentifier' - Amazon RDS cluster ARN.
--
-- * 'rhttpecSchema' - Logical schema name.
--
-- * 'rhttpecDatabaseName' - Logical database name.
--
-- * 'rhttpecAwsRegion' - AWS Region for RDS HTTP endpoint.
--
-- * 'rhttpecAwsSecretStoreARN' - AWS secret store ARN for database credentials.
rdsHTTPEndpointConfig ::
  RDSHTTPEndpointConfig
rdsHTTPEndpointConfig =
  RDSHTTPEndpointConfig'
    { _rhttpecDbClusterIdentifier = Nothing,
      _rhttpecSchema = Nothing,
      _rhttpecDatabaseName = Nothing,
      _rhttpecAwsRegion = Nothing,
      _rhttpecAwsSecretStoreARN = Nothing
    }

-- | Amazon RDS cluster ARN.
rhttpecDbClusterIdentifier :: Lens' RDSHTTPEndpointConfig (Maybe Text)
rhttpecDbClusterIdentifier = lens _rhttpecDbClusterIdentifier (\s a -> s {_rhttpecDbClusterIdentifier = a})

-- | Logical schema name.
rhttpecSchema :: Lens' RDSHTTPEndpointConfig (Maybe Text)
rhttpecSchema = lens _rhttpecSchema (\s a -> s {_rhttpecSchema = a})

-- | Logical database name.
rhttpecDatabaseName :: Lens' RDSHTTPEndpointConfig (Maybe Text)
rhttpecDatabaseName = lens _rhttpecDatabaseName (\s a -> s {_rhttpecDatabaseName = a})

-- | AWS Region for RDS HTTP endpoint.
rhttpecAwsRegion :: Lens' RDSHTTPEndpointConfig (Maybe Text)
rhttpecAwsRegion = lens _rhttpecAwsRegion (\s a -> s {_rhttpecAwsRegion = a})

-- | AWS secret store ARN for database credentials.
rhttpecAwsSecretStoreARN :: Lens' RDSHTTPEndpointConfig (Maybe Text)
rhttpecAwsSecretStoreARN = lens _rhttpecAwsSecretStoreARN (\s a -> s {_rhttpecAwsSecretStoreARN = a})

instance FromJSON RDSHTTPEndpointConfig where
  parseJSON =
    withObject
      "RDSHTTPEndpointConfig"
      ( \x ->
          RDSHTTPEndpointConfig'
            <$> (x .:? "dbClusterIdentifier")
            <*> (x .:? "schema")
            <*> (x .:? "databaseName")
            <*> (x .:? "awsRegion")
            <*> (x .:? "awsSecretStoreArn")
      )

instance Hashable RDSHTTPEndpointConfig

instance NFData RDSHTTPEndpointConfig

instance ToJSON RDSHTTPEndpointConfig where
  toJSON RDSHTTPEndpointConfig' {..} =
    object
      ( catMaybes
          [ ("dbClusterIdentifier" .=) <$> _rhttpecDbClusterIdentifier,
            ("schema" .=) <$> _rhttpecSchema,
            ("databaseName" .=) <$> _rhttpecDatabaseName,
            ("awsRegion" .=) <$> _rhttpecAwsRegion,
            ("awsSecretStoreArn" .=) <$> _rhttpecAwsSecretStoreARN
          ]
      )
