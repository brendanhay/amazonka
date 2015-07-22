{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CreatePlacementGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Creates a placement group that you launch cluster instances into. You
-- must give the group a name that\'s unique within the scope of your
-- account.
--
-- For more information about placement groups and cluster instances, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using_cluster_computing.html Cluster Instances>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreatePlacementGroup.html>
module Network.AWS.EC2.CreatePlacementGroup
    (
    -- * Request
      CreatePlacementGroup
    -- ** Request constructor
    , createPlacementGroup
    -- ** Request lenses
    , cpgrqDryRun
    , cpgrqGroupName
    , cpgrqStrategy

    -- * Response
    , CreatePlacementGroupResponse
    -- ** Response constructor
    , createPlacementGroupResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'createPlacementGroup' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpgrqDryRun'
--
-- * 'cpgrqGroupName'
--
-- * 'cpgrqStrategy'
data CreatePlacementGroup = CreatePlacementGroup'
    { _cpgrqDryRun    :: !(Maybe Bool)
    , _cpgrqGroupName :: !Text
    , _cpgrqStrategy  :: !PlacementStrategy
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePlacementGroup' smart constructor.
createPlacementGroup :: Text -> PlacementStrategy -> CreatePlacementGroup
createPlacementGroup pGroupName pStrategy =
    CreatePlacementGroup'
    { _cpgrqDryRun = Nothing
    , _cpgrqGroupName = pGroupName
    , _cpgrqStrategy = pStrategy
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
cpgrqDryRun :: Lens' CreatePlacementGroup (Maybe Bool)
cpgrqDryRun = lens _cpgrqDryRun (\ s a -> s{_cpgrqDryRun = a});

-- | A name for the placement group.
--
-- Constraints: Up to 255 ASCII characters
cpgrqGroupName :: Lens' CreatePlacementGroup Text
cpgrqGroupName = lens _cpgrqGroupName (\ s a -> s{_cpgrqGroupName = a});

-- | The placement strategy.
cpgrqStrategy :: Lens' CreatePlacementGroup PlacementStrategy
cpgrqStrategy = lens _cpgrqStrategy (\ s a -> s{_cpgrqStrategy = a});

instance AWSRequest CreatePlacementGroup where
        type Sv CreatePlacementGroup = EC2
        type Rs CreatePlacementGroup =
             CreatePlacementGroupResponse
        request = post
        response = receiveNull CreatePlacementGroupResponse'

instance ToHeaders CreatePlacementGroup where
        toHeaders = const mempty

instance ToPath CreatePlacementGroup where
        toPath = const "/"

instance ToQuery CreatePlacementGroup where
        toQuery CreatePlacementGroup'{..}
          = mconcat
              ["Action" =: ("CreatePlacementGroup" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "DryRun" =: _cpgrqDryRun,
               "GroupName" =: _cpgrqGroupName,
               "Strategy" =: _cpgrqStrategy]

-- | /See:/ 'createPlacementGroupResponse' smart constructor.
data CreatePlacementGroupResponse =
    CreatePlacementGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CreatePlacementGroupResponse' smart constructor.
createPlacementGroupResponse :: CreatePlacementGroupResponse
createPlacementGroupResponse = CreatePlacementGroupResponse'
