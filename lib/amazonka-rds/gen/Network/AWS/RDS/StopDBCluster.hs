{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.StopDBCluster
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stops an Amazon Aurora DB cluster. When you stop a DB cluster, Aurora retains the DB cluster's metadata, including its endpoints and DB parameter groups. Aurora also retains the transaction logs so you can do a point-in-time restore if necessary.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-cluster-stop-start.html Stopping and Starting an Aurora Cluster> in the /Amazon Aurora User Guide./
module Network.AWS.RDS.StopDBCluster
  ( -- * Creating a Request
    stopDBCluster,
    StopDBCluster,

    -- * Request Lenses
    sdbcDBClusterIdentifier,

    -- * Destructuring the Response
    stopDBClusterResponse,
    StopDBClusterResponse,

    -- * Response Lenses
    sdbcrsDBCluster,
    sdbcrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'stopDBCluster' smart constructor.
newtype StopDBCluster = StopDBCluster'
  { _sdbcDBClusterIdentifier ::
      Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopDBCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdbcDBClusterIdentifier' - The DB cluster identifier of the Amazon Aurora DB cluster to be stopped. This parameter is stored as a lowercase string.
stopDBCluster ::
  -- | 'sdbcDBClusterIdentifier'
  Text ->
  StopDBCluster
stopDBCluster pDBClusterIdentifier_ =
  StopDBCluster' {_sdbcDBClusterIdentifier = pDBClusterIdentifier_}

-- | The DB cluster identifier of the Amazon Aurora DB cluster to be stopped. This parameter is stored as a lowercase string.
sdbcDBClusterIdentifier :: Lens' StopDBCluster Text
sdbcDBClusterIdentifier = lens _sdbcDBClusterIdentifier (\s a -> s {_sdbcDBClusterIdentifier = a})

instance AWSRequest StopDBCluster where
  type Rs StopDBCluster = StopDBClusterResponse
  request = postQuery rds
  response =
    receiveXMLWrapper
      "StopDBClusterResult"
      ( \s h x ->
          StopDBClusterResponse'
            <$> (x .@? "DBCluster") <*> (pure (fromEnum s))
      )

instance Hashable StopDBCluster

instance NFData StopDBCluster

instance ToHeaders StopDBCluster where
  toHeaders = const mempty

instance ToPath StopDBCluster where
  toPath = const "/"

instance ToQuery StopDBCluster where
  toQuery StopDBCluster' {..} =
    mconcat
      [ "Action" =: ("StopDBCluster" :: ByteString),
        "Version" =: ("2014-10-31" :: ByteString),
        "DBClusterIdentifier" =: _sdbcDBClusterIdentifier
      ]

-- | /See:/ 'stopDBClusterResponse' smart constructor.
data StopDBClusterResponse = StopDBClusterResponse'
  { _sdbcrsDBCluster ::
      !(Maybe DBCluster),
    _sdbcrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StopDBClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdbcrsDBCluster' - Undocumented member.
--
-- * 'sdbcrsResponseStatus' - -- | The response status code.
stopDBClusterResponse ::
  -- | 'sdbcrsResponseStatus'
  Int ->
  StopDBClusterResponse
stopDBClusterResponse pResponseStatus_ =
  StopDBClusterResponse'
    { _sdbcrsDBCluster = Nothing,
      _sdbcrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
sdbcrsDBCluster :: Lens' StopDBClusterResponse (Maybe DBCluster)
sdbcrsDBCluster = lens _sdbcrsDBCluster (\s a -> s {_sdbcrsDBCluster = a})

-- | -- | The response status code.
sdbcrsResponseStatus :: Lens' StopDBClusterResponse Int
sdbcrsResponseStatus = lens _sdbcrsResponseStatus (\s a -> s {_sdbcrsResponseStatus = a})

instance NFData StopDBClusterResponse
