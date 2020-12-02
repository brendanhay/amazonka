{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.RDSDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.RDSDBInstance where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an Amazon RDS instance.
--
--
--
-- /See:/ 'rdsDBInstance' smart constructor.
data RDSDBInstance = RDSDBInstance'
  { _rdiRDSDBInstanceARN ::
      !(Maybe Text),
    _rdiDBUser :: !(Maybe Text),
    _rdiMissingOnRDS :: !(Maybe Bool),
    _rdiEngine :: !(Maybe Text),
    _rdiAddress :: !(Maybe Text),
    _rdiDBInstanceIdentifier :: !(Maybe Text),
    _rdiRegion :: !(Maybe Text),
    _rdiStackId :: !(Maybe Text),
    _rdiDBPassword :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RDSDBInstance' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rdiRDSDBInstanceARN' - The instance's ARN.
--
-- * 'rdiDBUser' - The master user name.
--
-- * 'rdiMissingOnRDS' - Set to @true@ if AWS OpsWorks Stacks is unable to discover the Amazon RDS instance. AWS OpsWorks Stacks attempts to discover the instance only once. If this value is set to @true@ , you must deregister the instance, and then register it again.
--
-- * 'rdiEngine' - The instance's database engine.
--
-- * 'rdiAddress' - The instance's address.
--
-- * 'rdiDBInstanceIdentifier' - The DB instance identifier.
--
-- * 'rdiRegion' - The instance's AWS region.
--
-- * 'rdiStackId' - The ID of the stack with which the instance is registered.
--
-- * 'rdiDBPassword' - AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
rdsDBInstance ::
  RDSDBInstance
rdsDBInstance =
  RDSDBInstance'
    { _rdiRDSDBInstanceARN = Nothing,
      _rdiDBUser = Nothing,
      _rdiMissingOnRDS = Nothing,
      _rdiEngine = Nothing,
      _rdiAddress = Nothing,
      _rdiDBInstanceIdentifier = Nothing,
      _rdiRegion = Nothing,
      _rdiStackId = Nothing,
      _rdiDBPassword = Nothing
    }

-- | The instance's ARN.
rdiRDSDBInstanceARN :: Lens' RDSDBInstance (Maybe Text)
rdiRDSDBInstanceARN = lens _rdiRDSDBInstanceARN (\s a -> s {_rdiRDSDBInstanceARN = a})

-- | The master user name.
rdiDBUser :: Lens' RDSDBInstance (Maybe Text)
rdiDBUser = lens _rdiDBUser (\s a -> s {_rdiDBUser = a})

-- | Set to @true@ if AWS OpsWorks Stacks is unable to discover the Amazon RDS instance. AWS OpsWorks Stacks attempts to discover the instance only once. If this value is set to @true@ , you must deregister the instance, and then register it again.
rdiMissingOnRDS :: Lens' RDSDBInstance (Maybe Bool)
rdiMissingOnRDS = lens _rdiMissingOnRDS (\s a -> s {_rdiMissingOnRDS = a})

-- | The instance's database engine.
rdiEngine :: Lens' RDSDBInstance (Maybe Text)
rdiEngine = lens _rdiEngine (\s a -> s {_rdiEngine = a})

-- | The instance's address.
rdiAddress :: Lens' RDSDBInstance (Maybe Text)
rdiAddress = lens _rdiAddress (\s a -> s {_rdiAddress = a})

-- | The DB instance identifier.
rdiDBInstanceIdentifier :: Lens' RDSDBInstance (Maybe Text)
rdiDBInstanceIdentifier = lens _rdiDBInstanceIdentifier (\s a -> s {_rdiDBInstanceIdentifier = a})

-- | The instance's AWS region.
rdiRegion :: Lens' RDSDBInstance (Maybe Text)
rdiRegion = lens _rdiRegion (\s a -> s {_rdiRegion = a})

-- | The ID of the stack with which the instance is registered.
rdiStackId :: Lens' RDSDBInstance (Maybe Text)
rdiStackId = lens _rdiStackId (\s a -> s {_rdiStackId = a})

-- | AWS OpsWorks Stacks returns @*****FILTERED*****@ instead of the actual value.
rdiDBPassword :: Lens' RDSDBInstance (Maybe Text)
rdiDBPassword = lens _rdiDBPassword (\s a -> s {_rdiDBPassword = a})

instance FromJSON RDSDBInstance where
  parseJSON =
    withObject
      "RDSDBInstance"
      ( \x ->
          RDSDBInstance'
            <$> (x .:? "RdsDbInstanceArn")
            <*> (x .:? "DbUser")
            <*> (x .:? "MissingOnRds")
            <*> (x .:? "Engine")
            <*> (x .:? "Address")
            <*> (x .:? "DbInstanceIdentifier")
            <*> (x .:? "Region")
            <*> (x .:? "StackId")
            <*> (x .:? "DbPassword")
      )

instance Hashable RDSDBInstance

instance NFData RDSDBInstance
