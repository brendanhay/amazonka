{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterRole where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes an AWS Identity and Access Management (IAM) role that is associated with a DB cluster.
--
--
--
-- /See:/ 'dbClusterRole' smart constructor.
data DBClusterRole = DBClusterRole'
  { _dcrStatus :: !(Maybe Text),
    _dcrFeatureName :: !(Maybe Text),
    _dcrRoleARN :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DBClusterRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcrStatus' - Describes the state of association between the IAM role and the DB cluster. The Status property returns one of the following values:     * @ACTIVE@ - the IAM role ARN is associated with the DB cluster and can be used to access other AWS services on your behalf.     * @PENDING@ - the IAM role ARN is being associated with the DB cluster.     * @INVALID@ - the IAM role ARN is associated with the DB cluster, but the DB cluster is unable to assume the IAM role in order to access other AWS services on your behalf.
--
-- * 'dcrFeatureName' - The name of the feature associated with the AWS Identity and Access Management (IAM) role. For the list of supported feature names, see 'DBEngineVersion' .
--
-- * 'dcrRoleARN' - The Amazon Resource Name (ARN) of the IAM role that is associated with the DB cluster.
dbClusterRole ::
  DBClusterRole
dbClusterRole =
  DBClusterRole'
    { _dcrStatus = Nothing,
      _dcrFeatureName = Nothing,
      _dcrRoleARN = Nothing
    }

-- | Describes the state of association between the IAM role and the DB cluster. The Status property returns one of the following values:     * @ACTIVE@ - the IAM role ARN is associated with the DB cluster and can be used to access other AWS services on your behalf.     * @PENDING@ - the IAM role ARN is being associated with the DB cluster.     * @INVALID@ - the IAM role ARN is associated with the DB cluster, but the DB cluster is unable to assume the IAM role in order to access other AWS services on your behalf.
dcrStatus :: Lens' DBClusterRole (Maybe Text)
dcrStatus = lens _dcrStatus (\s a -> s {_dcrStatus = a})

-- | The name of the feature associated with the AWS Identity and Access Management (IAM) role. For the list of supported feature names, see 'DBEngineVersion' .
dcrFeatureName :: Lens' DBClusterRole (Maybe Text)
dcrFeatureName = lens _dcrFeatureName (\s a -> s {_dcrFeatureName = a})

-- | The Amazon Resource Name (ARN) of the IAM role that is associated with the DB cluster.
dcrRoleARN :: Lens' DBClusterRole (Maybe Text)
dcrRoleARN = lens _dcrRoleARN (\s a -> s {_dcrRoleARN = a})

instance FromXML DBClusterRole where
  parseXML x =
    DBClusterRole'
      <$> (x .@? "Status") <*> (x .@? "FeatureName") <*> (x .@? "RoleArn")

instance Hashable DBClusterRole

instance NFData DBClusterRole
