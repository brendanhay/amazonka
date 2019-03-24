{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.StartDBCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts an Amazon Aurora DB cluster that was stopped using the AWS console, the stop-db-cluster AWS CLI command, or the StopDBCluster action.
--
--
-- For more information, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/aurora-cluster-stop-start.html Stopping and Starting an Aurora Cluster> in the /Amazon Aurora User Guide./
--
module Network.AWS.RDS.StartDBCluster
    (
    -- * Creating a Request
      startDBCluster
    , StartDBCluster
    -- * Request Lenses
    , sdcDBClusterIdentifier

    -- * Destructuring the Response
    , startDBClusterResponse
    , StartDBClusterResponse
    -- * Response Lenses
    , sdcrsDBCluster
    , sdcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'startDBCluster' smart constructor.
newtype StartDBCluster = StartDBCluster'
  { _sdcDBClusterIdentifier :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartDBCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcDBClusterIdentifier' - The DB cluster identifier of the Amazon Aurora DB cluster to be started. This parameter is stored as a lowercase string.
startDBCluster
    :: Text -- ^ 'sdcDBClusterIdentifier'
    -> StartDBCluster
startDBCluster pDBClusterIdentifier_ =
  StartDBCluster' {_sdcDBClusterIdentifier = pDBClusterIdentifier_}


-- | The DB cluster identifier of the Amazon Aurora DB cluster to be started. This parameter is stored as a lowercase string.
sdcDBClusterIdentifier :: Lens' StartDBCluster Text
sdcDBClusterIdentifier = lens _sdcDBClusterIdentifier (\ s a -> s{_sdcDBClusterIdentifier = a})

instance AWSRequest StartDBCluster where
        type Rs StartDBCluster = StartDBClusterResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "StartDBClusterResult"
              (\ s h x ->
                 StartDBClusterResponse' <$>
                   (x .@? "DBCluster") <*> (pure (fromEnum s)))

instance Hashable StartDBCluster where

instance NFData StartDBCluster where

instance ToHeaders StartDBCluster where
        toHeaders = const mempty

instance ToPath StartDBCluster where
        toPath = const "/"

instance ToQuery StartDBCluster where
        toQuery StartDBCluster'{..}
          = mconcat
              ["Action" =: ("StartDBCluster" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBClusterIdentifier" =: _sdcDBClusterIdentifier]

-- | /See:/ 'startDBClusterResponse' smart constructor.
data StartDBClusterResponse = StartDBClusterResponse'
  { _sdcrsDBCluster      :: !(Maybe DBCluster)
  , _sdcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'StartDBClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sdcrsDBCluster' - Undocumented member.
--
-- * 'sdcrsResponseStatus' - -- | The response status code.
startDBClusterResponse
    :: Int -- ^ 'sdcrsResponseStatus'
    -> StartDBClusterResponse
startDBClusterResponse pResponseStatus_ =
  StartDBClusterResponse'
    {_sdcrsDBCluster = Nothing, _sdcrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
sdcrsDBCluster :: Lens' StartDBClusterResponse (Maybe DBCluster)
sdcrsDBCluster = lens _sdcrsDBCluster (\ s a -> s{_sdcrsDBCluster = a})

-- | -- | The response status code.
sdcrsResponseStatus :: Lens' StartDBClusterResponse Int
sdcrsResponseStatus = lens _sdcrsResponseStatus (\ s a -> s{_sdcrsResponseStatus = a})

instance NFData StartDBClusterResponse where
