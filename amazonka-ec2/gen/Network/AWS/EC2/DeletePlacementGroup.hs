{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeletePlacementGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified placement group. You must terminate all instances in
-- the placement group before you can delete the placement group. For more
-- information about placement groups and cluster instances, see Cluster
-- Instances in the Amazon Elastic Compute Cloud User Guide. Example This
-- example deletes the placement group named XYZ-cluster.
-- https://ec2.amazonaws.com/?Action=DeletePlacementGroup
-- &amp;GroupName=XYZ-cluster &amp;AUTHPARAMS &lt;DeletePlacementGroupResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;d4904fd9-82c2-4ea5-adfe-a9cc3EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeletePlacementGroupResponse&gt;.
module Network.AWS.EC2.DeletePlacementGroup
    (
    -- * Request
      DeletePlacementGroup
    -- ** Request constructor
    , mkDeletePlacementGroup
    -- ** Request lenses
    , dpgGroupName

    -- * Response
    , DeletePlacementGroupResponse
    -- ** Response constructor
    , mkDeletePlacementGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype DeletePlacementGroup = DeletePlacementGroup
    { _dpgGroupName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeletePlacementGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @GroupName ::@ @Text@
--
mkDeletePlacementGroup :: Text -- ^ 'dpgGroupName'
                       -> DeletePlacementGroup
mkDeletePlacementGroup p1 = DeletePlacementGroup
    { _dpgGroupName = p1
    }

-- | The name of the placement group.
dpgGroupName :: Lens' DeletePlacementGroup Text
dpgGroupName = lens _dpgGroupName (\s a -> s { _dpgGroupName = a })

instance ToQuery DeletePlacementGroup where
    toQuery = genericQuery def

data DeletePlacementGroupResponse = DeletePlacementGroupResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeletePlacementGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeletePlacementGroupResponse :: DeletePlacementGroupResponse
mkDeletePlacementGroupResponse = DeletePlacementGroupResponse

instance AWSRequest DeletePlacementGroup where
    type Sv DeletePlacementGroup = EC2
    type Rs DeletePlacementGroup = DeletePlacementGroupResponse

    request = post "DeletePlacementGroup"
    response _ = nullaryResponse DeletePlacementGroupResponse
