{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
-- Instances in the Amazon Elastic Compute Cloud User Guide.
module Network.AWS.EC2.DeletePlacementGroup
    (
    -- * Request
      DeletePlacementGroup
    -- ** Request constructor
    , deletePlacementGroup
    -- ** Request lenses
    , dpgDryRun
    , dpgGroupName

    -- * Response
    , DeletePlacementGroupResponse
    -- ** Response constructor
    , deletePlacementGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DeletePlacementGroup = DeletePlacementGroup
    { _dpgDryRun    :: Maybe Bool
    , _dpgGroupName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeletePlacementGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpgDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dpgGroupName' @::@ 'Text'
--
deletePlacementGroup :: Text -- ^ 'dpgGroupName'
                     -> DeletePlacementGroup
deletePlacementGroup p1 = DeletePlacementGroup
    { _dpgGroupName = p1
    , _dpgDryRun    = Nothing
    }

dpgDryRun :: Lens' DeletePlacementGroup (Maybe Bool)
dpgDryRun = lens _dpgDryRun (\s a -> s { _dpgDryRun = a })

-- | The name of the placement group.
dpgGroupName :: Lens' DeletePlacementGroup Text
dpgGroupName = lens _dpgGroupName (\s a -> s { _dpgGroupName = a })

instance ToQuery DeletePlacementGroup

instance ToPath DeletePlacementGroup where
    toPath = const "/"

data DeletePlacementGroupResponse = DeletePlacementGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeletePlacementGroupResponse' constructor.
deletePlacementGroupResponse :: DeletePlacementGroupResponse
deletePlacementGroupResponse = DeletePlacementGroupResponse

instance FromXML DeletePlacementGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeletePlacementGroupResponse"

instance AWSRequest DeletePlacementGroup where
    type Sv DeletePlacementGroup = EC2
    type Rs DeletePlacementGroup = DeletePlacementGroupResponse

    request  = post "DeletePlacementGroup"
    response = nullaryResponse DeletePlacementGroupResponse
