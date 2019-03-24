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
-- Module      : Network.AWS.RDS.ModifyGlobalCluster
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modify a setting for an Amazon Aurora global cluster. You can change one or more database configuration parameters by specifying these parameters and the new values in the request. For more information on Amazon Aurora, see <https://docs.aws.amazon.com/AmazonRDS/latest/AuroraUserGuide/CHAP_AuroraOverview.html What Is Amazon Aurora?> in the /Amazon Aurora User Guide./
--
--
module Network.AWS.RDS.ModifyGlobalCluster
    (
    -- * Creating a Request
      modifyGlobalCluster
    , ModifyGlobalCluster
    -- * Request Lenses
    , mgcDeletionProtection
    , mgcGlobalClusterIdentifier
    , mgcNewGlobalClusterIdentifier

    -- * Destructuring the Response
    , modifyGlobalClusterResponse
    , ModifyGlobalClusterResponse
    -- * Response Lenses
    , mgcrsGlobalCluster
    , mgcrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.RDS.Types
import Network.AWS.RDS.Types.Product
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'modifyGlobalCluster' smart constructor.
data ModifyGlobalCluster = ModifyGlobalCluster'
  { _mgcDeletionProtection         :: !(Maybe Bool)
  , _mgcGlobalClusterIdentifier    :: !(Maybe Text)
  , _mgcNewGlobalClusterIdentifier :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyGlobalCluster' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mgcDeletionProtection' - Indicates if the global database cluster has deletion protection enabled. The global database cluster can't be deleted when this value is set to true.
--
-- * 'mgcGlobalClusterIdentifier' - The DB cluster identifier for the global cluster being modified. This parameter is not case-sensitive.  Constraints:     * Must match the identifier of an existing global database cluster.
--
-- * 'mgcNewGlobalClusterIdentifier' - The new cluster identifier for the global database cluster when modifying a global database cluster. This value is stored as a lowercase string.  Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens     * The first character must be a letter     * Can't end with a hyphen or contain two consecutive hyphens Example: @my-cluster2@
modifyGlobalCluster
    :: ModifyGlobalCluster
modifyGlobalCluster =
  ModifyGlobalCluster'
    { _mgcDeletionProtection = Nothing
    , _mgcGlobalClusterIdentifier = Nothing
    , _mgcNewGlobalClusterIdentifier = Nothing
    }


-- | Indicates if the global database cluster has deletion protection enabled. The global database cluster can't be deleted when this value is set to true.
mgcDeletionProtection :: Lens' ModifyGlobalCluster (Maybe Bool)
mgcDeletionProtection = lens _mgcDeletionProtection (\ s a -> s{_mgcDeletionProtection = a})

-- | The DB cluster identifier for the global cluster being modified. This parameter is not case-sensitive.  Constraints:     * Must match the identifier of an existing global database cluster.
mgcGlobalClusterIdentifier :: Lens' ModifyGlobalCluster (Maybe Text)
mgcGlobalClusterIdentifier = lens _mgcGlobalClusterIdentifier (\ s a -> s{_mgcGlobalClusterIdentifier = a})

-- | The new cluster identifier for the global database cluster when modifying a global database cluster. This value is stored as a lowercase string.  Constraints:     * Must contain from 1 to 63 letters, numbers, or hyphens     * The first character must be a letter     * Can't end with a hyphen or contain two consecutive hyphens Example: @my-cluster2@
mgcNewGlobalClusterIdentifier :: Lens' ModifyGlobalCluster (Maybe Text)
mgcNewGlobalClusterIdentifier = lens _mgcNewGlobalClusterIdentifier (\ s a -> s{_mgcNewGlobalClusterIdentifier = a})

instance AWSRequest ModifyGlobalCluster where
        type Rs ModifyGlobalCluster =
             ModifyGlobalClusterResponse
        request = postQuery rds
        response
          = receiveXMLWrapper "ModifyGlobalClusterResult"
              (\ s h x ->
                 ModifyGlobalClusterResponse' <$>
                   (x .@? "GlobalCluster") <*> (pure (fromEnum s)))

instance Hashable ModifyGlobalCluster where

instance NFData ModifyGlobalCluster where

instance ToHeaders ModifyGlobalCluster where
        toHeaders = const mempty

instance ToPath ModifyGlobalCluster where
        toPath = const "/"

instance ToQuery ModifyGlobalCluster where
        toQuery ModifyGlobalCluster'{..}
          = mconcat
              ["Action" =: ("ModifyGlobalCluster" :: ByteString),
               "Version" =: ("2014-10-31" :: ByteString),
               "DeletionProtection" =: _mgcDeletionProtection,
               "GlobalClusterIdentifier" =:
                 _mgcGlobalClusterIdentifier,
               "NewGlobalClusterIdentifier" =:
                 _mgcNewGlobalClusterIdentifier]

-- | /See:/ 'modifyGlobalClusterResponse' smart constructor.
data ModifyGlobalClusterResponse = ModifyGlobalClusterResponse'
  { _mgcrsGlobalCluster  :: !(Maybe GlobalCluster)
  , _mgcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyGlobalClusterResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mgcrsGlobalCluster' - Undocumented member.
--
-- * 'mgcrsResponseStatus' - -- | The response status code.
modifyGlobalClusterResponse
    :: Int -- ^ 'mgcrsResponseStatus'
    -> ModifyGlobalClusterResponse
modifyGlobalClusterResponse pResponseStatus_ =
  ModifyGlobalClusterResponse'
    {_mgcrsGlobalCluster = Nothing, _mgcrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
mgcrsGlobalCluster :: Lens' ModifyGlobalClusterResponse (Maybe GlobalCluster)
mgcrsGlobalCluster = lens _mgcrsGlobalCluster (\ s a -> s{_mgcrsGlobalCluster = a})

-- | -- | The response status code.
mgcrsResponseStatus :: Lens' ModifyGlobalClusterResponse Int
mgcrsResponseStatus = lens _mgcrsResponseStatus (\ s a -> s{_mgcrsResponseStatus = a})

instance NFData ModifyGlobalClusterResponse where
