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
-- Module      : Network.AWS.RDS.FailoverDBCluster
-- Copyright   : (c) 2013-2016 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Forces a failover for a DB cluster.
--
-- A failover for a DB cluster promotes one of the read-only instances in
-- the DB cluster to the master DB instance (the cluster writer) and
-- deletes the current primary instance.
--
-- Amazon Aurora will automatically fail over to a read-only instance, if
-- one exists, when the primary instance fails. You can force a failover
-- when you want to simulate a failure of a DB instance for testing.
-- Because each instance in a DB cluster has its own endpoint address, you
-- will need to clean up and re-establish any existing connections that use
-- those endpoint addresses when the failover is complete.
--
-- For more information on Amazon Aurora, see
-- <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/CHAP_Aurora.html Aurora on Amazon RDS>
-- in the /Amazon RDS User Guide./
module Network.AWS.RDS.FailoverDBCluster
    (
    -- * Creating a Request
      failoverDBCluster
    , FailoverDBCluster
    -- * Request Lenses
    , fdcDBClusterIdentifier

    -- * Destructuring the Response
    , failoverDBClusterResponse
    , FailoverDBClusterResponse
    -- * Response Lenses
    , fdcrsDBCluster
    , fdcrsResponseStatus
    ) where

import           Network.AWS.Lens
import           Network.AWS.Prelude
import           Network.AWS.RDS.Types
import           Network.AWS.RDS.Types.Product
import           Network.AWS.Request
import           Network.AWS.Response

-- |
--
-- /See:/ 'failoverDBCluster' smart constructor.
newtype FailoverDBCluster = FailoverDBCluster'
    { _fdcDBClusterIdentifier :: Maybe Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FailoverDBCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdcDBClusterIdentifier'
failoverDBCluster
    :: FailoverDBCluster
failoverDBCluster =
    FailoverDBCluster'
    { _fdcDBClusterIdentifier = Nothing
    }

-- | A DB cluster identifier to force a failover for. This parameter is not
-- case-sensitive.
--
-- Constraints:
--
-- -   Must contain from 1 to 63 alphanumeric characters or hyphens
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
fdcDBClusterIdentifier :: Lens' FailoverDBCluster (Maybe Text)
fdcDBClusterIdentifier = lens _fdcDBClusterIdentifier (\ s a -> s{_fdcDBClusterIdentifier = a});

instance AWSRequest FailoverDBCluster where
        type Rs FailoverDBCluster = FailoverDBClusterResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "FailoverDBClusterResult"
              (\ s h x ->
                 FailoverDBClusterResponse' <$>
                   (x .@? "DBCluster") <*> (pure (fromEnum s)))

instance Hashable FailoverDBCluster

instance NFData FailoverDBCluster

instance ToHeaders FailoverDBCluster where
        toHeaders = const mempty

instance ToPath FailoverDBCluster where
        toPath = const "/"

instance ToQuery FailoverDBCluster where
        toQuery FailoverDBCluster'{..}
          = mconcat
              ["Action" =: ("FailoverDBCluster" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBClusterIdentifier" =: _fdcDBClusterIdentifier]

-- | /See:/ 'failoverDBClusterResponse' smart constructor.
data FailoverDBClusterResponse = FailoverDBClusterResponse'
    { _fdcrsDBCluster      :: !(Maybe DBCluster)
    , _fdcrsResponseStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'FailoverDBClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdcrsDBCluster'
--
-- * 'fdcrsResponseStatus'
failoverDBClusterResponse
    :: Int -- ^ 'fdcrsResponseStatus'
    -> FailoverDBClusterResponse
failoverDBClusterResponse pResponseStatus_ =
    FailoverDBClusterResponse'
    { _fdcrsDBCluster = Nothing
    , _fdcrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
fdcrsDBCluster :: Lens' FailoverDBClusterResponse (Maybe DBCluster)
fdcrsDBCluster = lens _fdcrsDBCluster (\ s a -> s{_fdcrsDBCluster = a});

-- | The response status code.
fdcrsResponseStatus :: Lens' FailoverDBClusterResponse Int
fdcrsResponseStatus = lens _fdcrsResponseStatus (\ s a -> s{_fdcrsResponseStatus = a});

instance NFData FailoverDBClusterResponse
