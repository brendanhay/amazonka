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
-- Module      : Network.AWS.RDS.AddRoleToDBCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an Identity and Access Management (IAM) role from an Aurora DB cluster. For more information, see <http://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/Aurora.Authorizing.AWSServices.html Authorizing Amazon Aurora to Access Other AWS Services On Your Behalf> .
--
--
module Network.AWS.RDS.AddRoleToDBCluster
    (
    -- * Creating a Request
      addRoleToDBCluster
    , AddRoleToDBCluster
    -- * Request Lenses
    , artdcDBClusterIdentifier
    , artdcRoleARN

    -- * Destructuring the Response
    , addRoleToDBClusterResponse
    , AddRoleToDBClusterResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'addRoleToDBCluster' smart constructor.
data AddRoleToDBCluster = AddRoleToDBCluster'
  { _artdcDBClusterIdentifier :: !Text
  , _artdcRoleARN             :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddRoleToDBCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'artdcDBClusterIdentifier' - The name of the DB cluster to associate the IAM role with.
--
-- * 'artdcRoleARN' - The Amazon Resource Name (ARN) of the IAM role to associate with the Aurora DB cluster, for example @arn:aws:iam::123456789012:role/AuroraAccessRole@ .
addRoleToDBCluster
    :: Text -- ^ 'artdcDBClusterIdentifier'
    -> Text -- ^ 'artdcRoleARN'
    -> AddRoleToDBCluster
addRoleToDBCluster pDBClusterIdentifier_ pRoleARN_ =
  AddRoleToDBCluster'
    { _artdcDBClusterIdentifier = pDBClusterIdentifier_
    , _artdcRoleARN = pRoleARN_
    }


-- | The name of the DB cluster to associate the IAM role with.
artdcDBClusterIdentifier :: Lens' AddRoleToDBCluster Text
artdcDBClusterIdentifier = lens _artdcDBClusterIdentifier (\ s a -> s{_artdcDBClusterIdentifier = a})

-- | The Amazon Resource Name (ARN) of the IAM role to associate with the Aurora DB cluster, for example @arn:aws:iam::123456789012:role/AuroraAccessRole@ .
artdcRoleARN :: Lens' AddRoleToDBCluster Text
artdcRoleARN = lens _artdcRoleARN (\ s a -> s{_artdcRoleARN = a})

instance AWSRequest AddRoleToDBCluster where
        type Rs AddRoleToDBCluster =
             AddRoleToDBClusterResponse
        request = postQuery rds
        response = receiveNull AddRoleToDBClusterResponse'

instance Hashable AddRoleToDBCluster where

instance NFData AddRoleToDBCluster where

instance ToHeaders AddRoleToDBCluster where
        toHeaders = const mempty

instance ToPath AddRoleToDBCluster where
        toPath = const "/"

instance ToQuery AddRoleToDBCluster where
        toQuery AddRoleToDBCluster'{..}
          = mconcat
              ["Action" =: ("AddRoleToDBCluster" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DBClusterIdentifier" =: _artdcDBClusterIdentifier,
               "RoleArn" =: _artdcRoleARN]

-- | /See:/ 'addRoleToDBClusterResponse' smart constructor.
data AddRoleToDBClusterResponse =
  AddRoleToDBClusterResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'AddRoleToDBClusterResponse' with the minimum fields required to make a request.
--
addRoleToDBClusterResponse
    :: AddRoleToDBClusterResponse
addRoleToDBClusterResponse = AddRoleToDBClusterResponse'


instance NFData AddRoleToDBClusterResponse where
