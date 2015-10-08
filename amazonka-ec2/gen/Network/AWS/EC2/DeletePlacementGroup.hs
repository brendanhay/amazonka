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
-- Module      : Network.AWS.EC2.DeletePlacementGroup
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified placement group. You must terminate all instances
-- in the placement group before you can delete the placement group. For
-- more information about placement groups and cluster instances, see
-- <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using_cluster_computing.html Cluster Instances>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeletePlacementGroup.html AWS API Reference> for DeletePlacementGroup.
module Network.AWS.EC2.DeletePlacementGroup
    (
    -- * Creating a Request
      deletePlacementGroup
    , DeletePlacementGroup
    -- * Request Lenses
    , dpgDryRun
    , dpgGroupName

    -- * Destructuring the Response
    , deletePlacementGroupResponse
    , DeletePlacementGroupResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'deletePlacementGroup' smart constructor.
data DeletePlacementGroup = DeletePlacementGroup'
    { _dpgDryRun    :: !(Maybe Bool)
    , _dpgGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeletePlacementGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dpgDryRun'
--
-- * 'dpgGroupName'
deletePlacementGroup
    :: Text -- ^ 'dpgGroupName'
    -> DeletePlacementGroup
deletePlacementGroup pGroupName_ =
    DeletePlacementGroup'
    { _dpgDryRun = Nothing
    , _dpgGroupName = pGroupName_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
dpgDryRun :: Lens' DeletePlacementGroup (Maybe Bool)
dpgDryRun = lens _dpgDryRun (\ s a -> s{_dpgDryRun = a});

-- | The name of the placement group.
dpgGroupName :: Lens' DeletePlacementGroup Text
dpgGroupName = lens _dpgGroupName (\ s a -> s{_dpgGroupName = a});

instance AWSRequest DeletePlacementGroup where
        type Rs DeletePlacementGroup =
             DeletePlacementGroupResponse
        request = postQuery eC2
        response = receiveNull DeletePlacementGroupResponse'

instance ToHeaders DeletePlacementGroup where
        toHeaders = const mempty

instance ToPath DeletePlacementGroup where
        toPath = const "/"

instance ToQuery DeletePlacementGroup where
        toQuery DeletePlacementGroup'{..}
          = mconcat
              ["Action" =: ("DeletePlacementGroup" :: ByteString),
               "Version" =: ("2015-10-01" :: ByteString),
               "DryRun" =: _dpgDryRun, "GroupName" =: _dpgGroupName]

-- | /See:/ 'deletePlacementGroupResponse' smart constructor.
data DeletePlacementGroupResponse =
    DeletePlacementGroupResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeletePlacementGroupResponse' with the minimum fields required to make a request.
--
deletePlacementGroupResponse
    :: DeletePlacementGroupResponse
deletePlacementGroupResponse = DeletePlacementGroupResponse'
