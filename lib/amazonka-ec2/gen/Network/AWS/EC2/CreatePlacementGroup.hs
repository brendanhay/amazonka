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
-- Module      : Network.AWS.EC2.CreatePlacementGroup
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a placement group in which to launch instances. The strategy of the placement group determines how the instances are organized within the group.
--
--
-- A @cluster@ placement group is a logical grouping of instances within a single Availability Zone that benefit from low network latency, high network throughput. A @spread@ placement group places instances on distinct hardware.
--
-- For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/placement-groups.html Placement Groups> in the /Amazon Elastic Compute Cloud User Guide/ .
--
module Network.AWS.EC2.CreatePlacementGroup
    (
    -- * Creating a Request
      createPlacementGroup
    , CreatePlacementGroup
    -- * Request Lenses
    , cpgDryRun
    , cpgGroupName
    , cpgStrategy

    -- * Destructuring the Response
    , createPlacementGroupResponse
    , CreatePlacementGroupResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CreatePlacementGroup.
--
--
--
-- /See:/ 'createPlacementGroup' smart constructor.
data CreatePlacementGroup = CreatePlacementGroup'
  { _cpgDryRun    :: !(Maybe Bool)
  , _cpgGroupName :: !Text
  , _cpgStrategy  :: !PlacementStrategy
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePlacementGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cpgDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'cpgGroupName' - A name for the placement group. Must be unique within the scope of your account for the region. Constraints: Up to 255 ASCII characters
--
-- * 'cpgStrategy' - The placement strategy.
createPlacementGroup
    :: Text -- ^ 'cpgGroupName'
    -> PlacementStrategy -- ^ 'cpgStrategy'
    -> CreatePlacementGroup
createPlacementGroup pGroupName_ pStrategy_ =
  CreatePlacementGroup'
    { _cpgDryRun = Nothing
    , _cpgGroupName = pGroupName_
    , _cpgStrategy = pStrategy_
    }


-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cpgDryRun :: Lens' CreatePlacementGroup (Maybe Bool)
cpgDryRun = lens _cpgDryRun (\ s a -> s{_cpgDryRun = a})

-- | A name for the placement group. Must be unique within the scope of your account for the region. Constraints: Up to 255 ASCII characters
cpgGroupName :: Lens' CreatePlacementGroup Text
cpgGroupName = lens _cpgGroupName (\ s a -> s{_cpgGroupName = a})

-- | The placement strategy.
cpgStrategy :: Lens' CreatePlacementGroup PlacementStrategy
cpgStrategy = lens _cpgStrategy (\ s a -> s{_cpgStrategy = a})

instance AWSRequest CreatePlacementGroup where
        type Rs CreatePlacementGroup =
             CreatePlacementGroupResponse
        request = postQuery ec2
        response = receiveNull CreatePlacementGroupResponse'

instance Hashable CreatePlacementGroup where

instance NFData CreatePlacementGroup where

instance ToHeaders CreatePlacementGroup where
        toHeaders = const mempty

instance ToPath CreatePlacementGroup where
        toPath = const "/"

instance ToQuery CreatePlacementGroup where
        toQuery CreatePlacementGroup'{..}
          = mconcat
              ["Action" =: ("CreatePlacementGroup" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "DryRun" =: _cpgDryRun, "GroupName" =: _cpgGroupName,
               "Strategy" =: _cpgStrategy]

-- | /See:/ 'createPlacementGroupResponse' smart constructor.
data CreatePlacementGroupResponse =
  CreatePlacementGroupResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CreatePlacementGroupResponse' with the minimum fields required to make a request.
--
createPlacementGroupResponse
    :: CreatePlacementGroupResponse
createPlacementGroupResponse = CreatePlacementGroupResponse'


instance NFData CreatePlacementGroupResponse where
