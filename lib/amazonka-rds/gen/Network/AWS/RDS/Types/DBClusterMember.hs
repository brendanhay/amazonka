{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterMember
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterMember where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Contains information about an instance that is part of a DB cluster.
--
--
--
-- /See:/ 'dbClusterMember' smart constructor.
data DBClusterMember = DBClusterMember'
  { _dcmPromotionTier ::
      !(Maybe Int),
    _dcmDBInstanceIdentifier :: !(Maybe Text),
    _dcmIsClusterWriter :: !(Maybe Bool),
    _dcmDBClusterParameterGroupStatus :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBClusterMember' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcmPromotionTier' - A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
--
-- * 'dcmDBInstanceIdentifier' - Specifies the instance identifier for this member of the DB cluster.
--
-- * 'dcmIsClusterWriter' - Value that is @true@ if the cluster member is the primary instance for the DB cluster and @false@ otherwise.
--
-- * 'dcmDBClusterParameterGroupStatus' - Specifies the status of the DB cluster parameter group for this member of the DB cluster.
dbClusterMember ::
  DBClusterMember
dbClusterMember =
  DBClusterMember'
    { _dcmPromotionTier = Nothing,
      _dcmDBInstanceIdentifier = Nothing,
      _dcmIsClusterWriter = Nothing,
      _dcmDBClusterParameterGroupStatus = Nothing
    }

-- | A value that specifies the order in which an Aurora Replica is promoted to the primary instance after a failure of the existing primary instance. For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/Aurora.Managing.Backups.html#Aurora.Managing.FaultTolerance Fault Tolerance for an Aurora DB Cluster> in the /Amazon Aurora User Guide/ .
dcmPromotionTier :: Lens' DBClusterMember (Maybe Int)
dcmPromotionTier = lens _dcmPromotionTier (\s a -> s {_dcmPromotionTier = a})

-- | Specifies the instance identifier for this member of the DB cluster.
dcmDBInstanceIdentifier :: Lens' DBClusterMember (Maybe Text)
dcmDBInstanceIdentifier = lens _dcmDBInstanceIdentifier (\s a -> s {_dcmDBInstanceIdentifier = a})

-- | Value that is @true@ if the cluster member is the primary instance for the DB cluster and @false@ otherwise.
dcmIsClusterWriter :: Lens' DBClusterMember (Maybe Bool)
dcmIsClusterWriter = lens _dcmIsClusterWriter (\s a -> s {_dcmIsClusterWriter = a})

-- | Specifies the status of the DB cluster parameter group for this member of the DB cluster.
dcmDBClusterParameterGroupStatus :: Lens' DBClusterMember (Maybe Text)
dcmDBClusterParameterGroupStatus = lens _dcmDBClusterParameterGroupStatus (\s a -> s {_dcmDBClusterParameterGroupStatus = a})

instance FromXML DBClusterMember where
  parseXML x =
    DBClusterMember'
      <$> (x .@? "PromotionTier")
      <*> (x .@? "DBInstanceIdentifier")
      <*> (x .@? "IsClusterWriter")
      <*> (x .@? "DBClusterParameterGroupStatus")

instance Hashable DBClusterMember

instance NFData DBClusterMember
