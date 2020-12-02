{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBProxyTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBProxyTarget where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.TargetHealth
import Network.AWS.RDS.Types.TargetType

-- | Contains the details for an RDS Proxy target. It represents an RDS DB instance or Aurora DB cluster that the proxy can connect to. One or more targets are associated with an RDS Proxy target group.
--
--
-- This data type is used as a response element in the @DescribeDBProxyTargets@ action.
--
--
-- /See:/ 'dbProxyTarget' smart constructor.
data DBProxyTarget = DBProxyTarget'
  { _dptTargetARN :: !(Maybe Text),
    _dptTargetHealth :: !(Maybe TargetHealth),
    _dptTrackedClusterId :: !(Maybe Text),
    _dptRDSResourceId :: !(Maybe Text),
    _dptType :: !(Maybe TargetType),
    _dptEndpoint :: !(Maybe Text),
    _dptPort :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBProxyTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dptTargetARN' - The Amazon Resource Name (ARN) for the RDS DB instance or Aurora DB cluster.
--
-- * 'dptTargetHealth' - Information about the connection health of the RDS Proxy target.
--
-- * 'dptTrackedClusterId' - The DB cluster identifier when the target represents an Aurora DB cluster. This field is blank when the target represents an RDS DB instance.
--
-- * 'dptRDSResourceId' - The identifier representing the target. It can be the instance identifier for an RDS DB instance, or the cluster identifier for an Aurora DB cluster.
--
-- * 'dptType' - Specifies the kind of database, such as an RDS DB instance or an Aurora DB cluster, that the target represents.
--
-- * 'dptEndpoint' - The writer endpoint for the RDS DB instance or Aurora DB cluster.
--
-- * 'dptPort' - The port that the RDS Proxy uses to connect to the target RDS DB instance or Aurora DB cluster.
dbProxyTarget ::
  DBProxyTarget
dbProxyTarget =
  DBProxyTarget'
    { _dptTargetARN = Nothing,
      _dptTargetHealth = Nothing,
      _dptTrackedClusterId = Nothing,
      _dptRDSResourceId = Nothing,
      _dptType = Nothing,
      _dptEndpoint = Nothing,
      _dptPort = Nothing
    }

-- | The Amazon Resource Name (ARN) for the RDS DB instance or Aurora DB cluster.
dptTargetARN :: Lens' DBProxyTarget (Maybe Text)
dptTargetARN = lens _dptTargetARN (\s a -> s {_dptTargetARN = a})

-- | Information about the connection health of the RDS Proxy target.
dptTargetHealth :: Lens' DBProxyTarget (Maybe TargetHealth)
dptTargetHealth = lens _dptTargetHealth (\s a -> s {_dptTargetHealth = a})

-- | The DB cluster identifier when the target represents an Aurora DB cluster. This field is blank when the target represents an RDS DB instance.
dptTrackedClusterId :: Lens' DBProxyTarget (Maybe Text)
dptTrackedClusterId = lens _dptTrackedClusterId (\s a -> s {_dptTrackedClusterId = a})

-- | The identifier representing the target. It can be the instance identifier for an RDS DB instance, or the cluster identifier for an Aurora DB cluster.
dptRDSResourceId :: Lens' DBProxyTarget (Maybe Text)
dptRDSResourceId = lens _dptRDSResourceId (\s a -> s {_dptRDSResourceId = a})

-- | Specifies the kind of database, such as an RDS DB instance or an Aurora DB cluster, that the target represents.
dptType :: Lens' DBProxyTarget (Maybe TargetType)
dptType = lens _dptType (\s a -> s {_dptType = a})

-- | The writer endpoint for the RDS DB instance or Aurora DB cluster.
dptEndpoint :: Lens' DBProxyTarget (Maybe Text)
dptEndpoint = lens _dptEndpoint (\s a -> s {_dptEndpoint = a})

-- | The port that the RDS Proxy uses to connect to the target RDS DB instance or Aurora DB cluster.
dptPort :: Lens' DBProxyTarget (Maybe Int)
dptPort = lens _dptPort (\s a -> s {_dptPort = a})

instance FromXML DBProxyTarget where
  parseXML x =
    DBProxyTarget'
      <$> (x .@? "TargetArn")
      <*> (x .@? "TargetHealth")
      <*> (x .@? "TrackedClusterId")
      <*> (x .@? "RdsResourceId")
      <*> (x .@? "Type")
      <*> (x .@? "Endpoint")
      <*> (x .@? "Port")

instance Hashable DBProxyTarget

instance NFData DBProxyTarget
