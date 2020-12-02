{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterIAMRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterIAMRole where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | An AWS Identity and Access Management (IAM) role that can be used by the associated Amazon Redshift cluster to access other AWS services.
--
--
--
-- /See:/ 'clusterIAMRole' smart constructor.
data ClusterIAMRole = ClusterIAMRole'
  { _cirIAMRoleARN ::
      !(Maybe Text),
    _cirApplyStatus :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ClusterIAMRole' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cirIAMRoleARN' - The Amazon Resource Name (ARN) of the IAM role, for example, @arn:aws:iam::123456789012:role/RedshiftCopyUnload@ .
--
-- * 'cirApplyStatus' - A value that describes the status of the IAM role's association with an Amazon Redshift cluster. The following are possible statuses and descriptions.     * @in-sync@ : The role is available for use by the cluster.     * @adding@ : The role is in the process of being associated with the cluster.     * @removing@ : The role is in the process of being disassociated with the cluster.
clusterIAMRole ::
  ClusterIAMRole
clusterIAMRole =
  ClusterIAMRole'
    { _cirIAMRoleARN = Nothing,
      _cirApplyStatus = Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM role, for example, @arn:aws:iam::123456789012:role/RedshiftCopyUnload@ .
cirIAMRoleARN :: Lens' ClusterIAMRole (Maybe Text)
cirIAMRoleARN = lens _cirIAMRoleARN (\s a -> s {_cirIAMRoleARN = a})

-- | A value that describes the status of the IAM role's association with an Amazon Redshift cluster. The following are possible statuses and descriptions.     * @in-sync@ : The role is available for use by the cluster.     * @adding@ : The role is in the process of being associated with the cluster.     * @removing@ : The role is in the process of being disassociated with the cluster.
cirApplyStatus :: Lens' ClusterIAMRole (Maybe Text)
cirApplyStatus = lens _cirApplyStatus (\s a -> s {_cirApplyStatus = a})

instance FromXML ClusterIAMRole where
  parseXML x =
    ClusterIAMRole'
      <$> (x .@? "IamRoleArn") <*> (x .@? "ApplyStatus")

instance Hashable ClusterIAMRole

instance NFData ClusterIAMRole
