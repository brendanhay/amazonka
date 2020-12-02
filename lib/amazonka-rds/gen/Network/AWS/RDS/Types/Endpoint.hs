{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.Endpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.Endpoint where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | This data type represents the information you need to connect to an Amazon RDS DB instance. This data type is used as a response element in the following actions:
--
--
--     * @CreateDBInstance@
--
--     * @DescribeDBInstances@
--
--     * @DeleteDBInstance@
--
--
--
-- For the data structure that represents Amazon Aurora DB cluster endpoints, see @DBClusterEndpoint@ .
--
--
-- /See:/ 'endpoint' smart constructor.
data Endpoint = Endpoint'
  { _eHostedZoneId :: !(Maybe Text),
    _eAddress :: !(Maybe Text),
    _ePort :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Endpoint' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eHostedZoneId' - Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
--
-- * 'eAddress' - Specifies the DNS address of the DB instance.
--
-- * 'ePort' - Specifies the port that the database engine is listening on.
endpoint ::
  Endpoint
endpoint =
  Endpoint'
    { _eHostedZoneId = Nothing,
      _eAddress = Nothing,
      _ePort = Nothing
    }

-- | Specifies the ID that Amazon Route 53 assigns when you create a hosted zone.
eHostedZoneId :: Lens' Endpoint (Maybe Text)
eHostedZoneId = lens _eHostedZoneId (\s a -> s {_eHostedZoneId = a})

-- | Specifies the DNS address of the DB instance.
eAddress :: Lens' Endpoint (Maybe Text)
eAddress = lens _eAddress (\s a -> s {_eAddress = a})

-- | Specifies the port that the database engine is listening on.
ePort :: Lens' Endpoint (Maybe Int)
ePort = lens _ePort (\s a -> s {_ePort = a})

instance FromXML Endpoint where
  parseXML x =
    Endpoint'
      <$> (x .@? "HostedZoneId") <*> (x .@? "Address") <*> (x .@? "Port")

instance Hashable Endpoint

instance NFData Endpoint
