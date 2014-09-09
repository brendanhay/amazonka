{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreatePlacementGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a placement group that you launch cluster instances into. You must
-- give the group a name that's unique within the scope of your account. For
-- more information about placement groups and cluster instances, see Cluster
-- Instances in the Amazon Elastic Compute Cloud User Guide. Example This
-- example creates a placement group named XYZ-cluster.
-- https://ec2.amazonaws.com/?Action=CreatePlacementGroup
-- &amp;GroupName=XYZ-cluster &amp;Strategy=cluster &amp;AUTHPARAMS
-- &lt;CreatePlacementGroupResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;d4904fd9-82c2-4ea5-adfe-a9cc3EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/CreatePlacementGroupResponse&gt;.
module Network.AWS.EC2.V2014_06_15.CreatePlacementGroup
    (
    -- * Request
      CreatePlacementGroup
    -- ** Request constructor
    , mkCreatePlacementGroup
    -- ** Request lenses
    , cpgGroupName
    , cpgStrategy

    -- * Response
    , CreatePlacementGroupResponse
    -- ** Response constructor
    , mkCreatePlacementGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

data CreatePlacementGroup = CreatePlacementGroup
    { _cpgGroupName :: Text
    , _cpgStrategy :: PlacementStrategy
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreatePlacementGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @Text@
--
-- * @Strategy ::@ @PlacementStrategy@
--
mkCreatePlacementGroup :: Text -- ^ 'cpgGroupName'
                       -> PlacementStrategy -- ^ 'cpgStrategy'
                       -> CreatePlacementGroup
mkCreatePlacementGroup p1 p2 = CreatePlacementGroup
    { _cpgGroupName = p1
    , _cpgStrategy = p2
    }

-- | A name for the placement group. Constraints: Up to 255 ASCII characters.
cpgGroupName :: Lens' CreatePlacementGroup Text
cpgGroupName = lens _cpgGroupName (\s a -> s { _cpgGroupName = a })

-- | The placement strategy.
cpgStrategy :: Lens' CreatePlacementGroup PlacementStrategy
cpgStrategy = lens _cpgStrategy (\s a -> s { _cpgStrategy = a })

instance ToQuery CreatePlacementGroup where
    toQuery = genericQuery def

data CreatePlacementGroupResponse = CreatePlacementGroupResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'CreatePlacementGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkCreatePlacementGroupResponse :: CreatePlacementGroupResponse
mkCreatePlacementGroupResponse = CreatePlacementGroupResponse

instance AWSRequest CreatePlacementGroup where
    type Sv CreatePlacementGroup = EC2
    type Rs CreatePlacementGroup = CreatePlacementGroupResponse

    request = post "CreatePlacementGroup"
    response _ = nullaryResponse CreatePlacementGroupResponse
