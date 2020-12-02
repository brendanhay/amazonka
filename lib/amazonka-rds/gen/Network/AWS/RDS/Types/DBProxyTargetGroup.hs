{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBProxyTargetGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBProxyTargetGroup where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types.ConnectionPoolConfigurationInfo

-- | Represents a set of RDS DB instances, Aurora DB clusters, or both that a proxy can connect to. Currently, each target group is associated with exactly one RDS DB instance or Aurora DB cluster.
--
--
-- This data type is used as a response element in the @DescribeDBProxyTargetGroups@ action.
--
--
-- /See:/ 'dbProxyTargetGroup' smart constructor.
data DBProxyTargetGroup = DBProxyTargetGroup'
  { _dptgStatus ::
      !(Maybe Text),
    _dptgConnectionPoolConfig ::
      !(Maybe ConnectionPoolConfigurationInfo),
    _dptgTargetGroupARN :: !(Maybe Text),
    _dptgUpdatedDate :: !(Maybe ISO8601),
    _dptgCreatedDate :: !(Maybe ISO8601),
    _dptgDBProxyName :: !(Maybe Text),
    _dptgTargetGroupName :: !(Maybe Text),
    _dptgIsDefault :: !(Maybe Bool)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBProxyTargetGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dptgStatus' - The current status of this target group. A status of @available@ means the target group is correctly associated with a database. Other values indicate that you must wait for the target group to be ready, or take some action to resolve an issue.
--
-- * 'dptgConnectionPoolConfig' - The settings that determine the size and behavior of the connection pool for the target group.
--
-- * 'dptgTargetGroupARN' - The Amazon Resource Name (ARN) representing the target group.
--
-- * 'dptgUpdatedDate' - The date and time when the target group was last updated.
--
-- * 'dptgCreatedDate' - The date and time when the target group was first created.
--
-- * 'dptgDBProxyName' - The identifier for the RDS proxy associated with this target group.
--
-- * 'dptgTargetGroupName' - The identifier for the target group. This name must be unique for all target groups owned by your AWS account in the specified AWS Region.
--
-- * 'dptgIsDefault' - Whether this target group is the first one used for connection requests by the associated proxy. Because each proxy is currently associated with a single target group, currently this setting is always @true@ .
dbProxyTargetGroup ::
  DBProxyTargetGroup
dbProxyTargetGroup =
  DBProxyTargetGroup'
    { _dptgStatus = Nothing,
      _dptgConnectionPoolConfig = Nothing,
      _dptgTargetGroupARN = Nothing,
      _dptgUpdatedDate = Nothing,
      _dptgCreatedDate = Nothing,
      _dptgDBProxyName = Nothing,
      _dptgTargetGroupName = Nothing,
      _dptgIsDefault = Nothing
    }

-- | The current status of this target group. A status of @available@ means the target group is correctly associated with a database. Other values indicate that you must wait for the target group to be ready, or take some action to resolve an issue.
dptgStatus :: Lens' DBProxyTargetGroup (Maybe Text)
dptgStatus = lens _dptgStatus (\s a -> s {_dptgStatus = a})

-- | The settings that determine the size and behavior of the connection pool for the target group.
dptgConnectionPoolConfig :: Lens' DBProxyTargetGroup (Maybe ConnectionPoolConfigurationInfo)
dptgConnectionPoolConfig = lens _dptgConnectionPoolConfig (\s a -> s {_dptgConnectionPoolConfig = a})

-- | The Amazon Resource Name (ARN) representing the target group.
dptgTargetGroupARN :: Lens' DBProxyTargetGroup (Maybe Text)
dptgTargetGroupARN = lens _dptgTargetGroupARN (\s a -> s {_dptgTargetGroupARN = a})

-- | The date and time when the target group was last updated.
dptgUpdatedDate :: Lens' DBProxyTargetGroup (Maybe UTCTime)
dptgUpdatedDate = lens _dptgUpdatedDate (\s a -> s {_dptgUpdatedDate = a}) . mapping _Time

-- | The date and time when the target group was first created.
dptgCreatedDate :: Lens' DBProxyTargetGroup (Maybe UTCTime)
dptgCreatedDate = lens _dptgCreatedDate (\s a -> s {_dptgCreatedDate = a}) . mapping _Time

-- | The identifier for the RDS proxy associated with this target group.
dptgDBProxyName :: Lens' DBProxyTargetGroup (Maybe Text)
dptgDBProxyName = lens _dptgDBProxyName (\s a -> s {_dptgDBProxyName = a})

-- | The identifier for the target group. This name must be unique for all target groups owned by your AWS account in the specified AWS Region.
dptgTargetGroupName :: Lens' DBProxyTargetGroup (Maybe Text)
dptgTargetGroupName = lens _dptgTargetGroupName (\s a -> s {_dptgTargetGroupName = a})

-- | Whether this target group is the first one used for connection requests by the associated proxy. Because each proxy is currently associated with a single target group, currently this setting is always @true@ .
dptgIsDefault :: Lens' DBProxyTargetGroup (Maybe Bool)
dptgIsDefault = lens _dptgIsDefault (\s a -> s {_dptgIsDefault = a})

instance FromXML DBProxyTargetGroup where
  parseXML x =
    DBProxyTargetGroup'
      <$> (x .@? "Status")
      <*> (x .@? "ConnectionPoolConfig")
      <*> (x .@? "TargetGroupArn")
      <*> (x .@? "UpdatedDate")
      <*> (x .@? "CreatedDate")
      <*> (x .@? "DBProxyName")
      <*> (x .@? "TargetGroupName")
      <*> (x .@? "IsDefault")

instance Hashable DBProxyTargetGroup

instance NFData DBProxyTargetGroup
