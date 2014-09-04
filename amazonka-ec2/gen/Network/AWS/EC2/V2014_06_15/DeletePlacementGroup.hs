{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeletePlacementGroup
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
module Network.AWS.EC2.V2014_06_15.DeletePlacementGroup
    (
    -- * Request
      DeletePlacementGroup
    -- ** Request constructor
    , deletePlacementGroup
    -- ** Request lenses
    , dpgrGroupName

    -- * Response
    , DeletePlacementGroupResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeletePlacementGroup' request.
deletePlacementGroup :: Text -- ^ 'dpgrGroupName'
                     -> DeletePlacementGroup
deletePlacementGroup p1 = DeletePlacementGroup
    { _dpgrGroupName = p1
    }
{-# INLINE deletePlacementGroup #-}

data DeletePlacementGroup = DeletePlacementGroup
    { _dpgrGroupName :: Text
      -- ^ The name of the placement group.
    } deriving (Show, Generic)

-- | The name of the placement group.
dpgrGroupName :: Lens' DeletePlacementGroup (Text)
dpgrGroupName f x =
    f (_dpgrGroupName x)
        <&> \y -> x { _dpgrGroupName = y }
{-# INLINE dpgrGroupName #-}

instance ToQuery DeletePlacementGroup where
    toQuery = genericQuery def

data DeletePlacementGroupResponse = DeletePlacementGroupResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeletePlacementGroup where
    type Sv DeletePlacementGroup = EC2
    type Rs DeletePlacementGroup = DeletePlacementGroupResponse

    request = post "DeletePlacementGroup"
    response _ = nullaryResponse DeletePlacementGroupResponse
