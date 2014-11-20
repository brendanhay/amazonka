{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
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
-- Instances in the Amazon Elastic Compute Cloud User Guide.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeletePlacementGroup.html>
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
import qualified GHC.Exts

data DeletePlacementGroup = DeletePlacementGroup
    { _dpgDryRun    :: Maybe Bool
    , _dpgGroupName :: Text
    } deriving (Eq, Ord, Show)

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

data DeletePlacementGroupResponse = DeletePlacementGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeletePlacementGroupResponse' constructor.
deletePlacementGroupResponse :: DeletePlacementGroupResponse
deletePlacementGroupResponse = DeletePlacementGroupResponse

instance ToPath DeletePlacementGroup where
    toPath = const "/"

instance ToQuery DeletePlacementGroup where
    toQuery DeletePlacementGroup{..} = mconcat
        [ "dryRun"    =? _dpgDryRun
        , "groupName" =? _dpgGroupName
        ]

instance ToHeaders DeletePlacementGroup

instance AWSRequest DeletePlacementGroup where
    type Sv DeletePlacementGroup = EC2
    type Rs DeletePlacementGroup = DeletePlacementGroupResponse

    request  = post "DeletePlacementGroup"
    response = nullResponse DeletePlacementGroupResponse
