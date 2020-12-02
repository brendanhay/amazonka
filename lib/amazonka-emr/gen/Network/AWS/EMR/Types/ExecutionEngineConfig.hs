{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.ExecutionEngineConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.ExecutionEngineConfig where

import Network.AWS.EMR.Types.ExecutionEngineType
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specifies the execution engine (cluster) to run the notebook and perform the notebook execution, for example, an EMR cluster.
--
--
--
-- /See:/ 'executionEngineConfig' smart constructor.
data ExecutionEngineConfig = ExecutionEngineConfig'
  { _eecMasterInstanceSecurityGroupId ::
      !(Maybe Text),
    _eecType :: !(Maybe ExecutionEngineType),
    _eecId :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ExecutionEngineConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'eecMasterInstanceSecurityGroupId' - An optional unique ID of an EC2 security group to associate with the master instance of the EMR cluster for this notebook execution. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-managed-notebooks-security-groups.html Specifying EC2 Security Groups for EMR Notebooks> in the /EMR Management Guide/ .
--
-- * 'eecType' - The type of execution engine. A value of @EMR@ specifies an EMR cluster.
--
-- * 'eecId' - The unique identifier of the execution engine. For an EMR cluster, this is the cluster ID.
executionEngineConfig ::
  -- | 'eecId'
  Text ->
  ExecutionEngineConfig
executionEngineConfig pId_ =
  ExecutionEngineConfig'
    { _eecMasterInstanceSecurityGroupId =
        Nothing,
      _eecType = Nothing,
      _eecId = pId_
    }

-- | An optional unique ID of an EC2 security group to associate with the master instance of the EMR cluster for this notebook execution. For more information see <https://docs.aws.amazon.com/emr/latest/ManagementGuide/emr-managed-notebooks-security-groups.html Specifying EC2 Security Groups for EMR Notebooks> in the /EMR Management Guide/ .
eecMasterInstanceSecurityGroupId :: Lens' ExecutionEngineConfig (Maybe Text)
eecMasterInstanceSecurityGroupId = lens _eecMasterInstanceSecurityGroupId (\s a -> s {_eecMasterInstanceSecurityGroupId = a})

-- | The type of execution engine. A value of @EMR@ specifies an EMR cluster.
eecType :: Lens' ExecutionEngineConfig (Maybe ExecutionEngineType)
eecType = lens _eecType (\s a -> s {_eecType = a})

-- | The unique identifier of the execution engine. For an EMR cluster, this is the cluster ID.
eecId :: Lens' ExecutionEngineConfig Text
eecId = lens _eecId (\s a -> s {_eecId = a})

instance FromJSON ExecutionEngineConfig where
  parseJSON =
    withObject
      "ExecutionEngineConfig"
      ( \x ->
          ExecutionEngineConfig'
            <$> (x .:? "MasterInstanceSecurityGroupId")
            <*> (x .:? "Type")
            <*> (x .: "Id")
      )

instance Hashable ExecutionEngineConfig

instance NFData ExecutionEngineConfig

instance ToJSON ExecutionEngineConfig where
  toJSON ExecutionEngineConfig' {..} =
    object
      ( catMaybes
          [ ("MasterInstanceSecurityGroupId" .=)
              <$> _eecMasterInstanceSecurityGroupId,
            ("Type" .=) <$> _eecType,
            Just ("Id" .= _eecId)
          ]
      )
