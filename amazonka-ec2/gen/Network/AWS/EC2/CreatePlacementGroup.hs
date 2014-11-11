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

-- Module      : Network.AWS.EC2.CreatePlacementGroup
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
-- Instances in the Amazon Elastic Compute Cloud User Guide.
module Network.AWS.EC2.CreatePlacementGroup
    (
    -- * Request
      CreatePlacementGroup
    -- ** Request constructor
    , createPlacementGroup
    -- ** Request lenses
    , cpgDryRun
    , cpgGroupName
    , cpgStrategy

    -- * Response
    , CreatePlacementGroupResponse
    -- ** Response constructor
    , createPlacementGroupResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data CreatePlacementGroup = CreatePlacementGroup
    { _cpgDryRun    :: Maybe Bool
    , _cpgGroupName :: Text
    , _cpgStrategy  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreatePlacementGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpgDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'cpgGroupName' @::@ 'Text'
--
-- * 'cpgStrategy' @::@ 'Text'
--
createPlacementGroup :: Text -- ^ 'cpgGroupName'
                     -> Text -- ^ 'cpgStrategy'
                     -> CreatePlacementGroup
createPlacementGroup p1 p2 = CreatePlacementGroup
    { _cpgGroupName = p1
    , _cpgStrategy  = p2
    , _cpgDryRun    = Nothing
    }

cpgDryRun :: Lens' CreatePlacementGroup (Maybe Bool)
cpgDryRun = lens _cpgDryRun (\s a -> s { _cpgDryRun = a })

-- | A name for the placement group. Constraints: Up to 255 ASCII characters.
cpgGroupName :: Lens' CreatePlacementGroup Text
cpgGroupName = lens _cpgGroupName (\s a -> s { _cpgGroupName = a })

-- | The placement strategy.
cpgStrategy :: Lens' CreatePlacementGroup Text
cpgStrategy = lens _cpgStrategy (\s a -> s { _cpgStrategy = a })
instance ToQuery CreatePlacementGroup

instance ToPath CreatePlacementGroup where
    toPath = const "/"

data CreatePlacementGroupResponse = CreatePlacementGroupResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'CreatePlacementGroupResponse' constructor.
createPlacementGroupResponse :: CreatePlacementGroupResponse
createPlacementGroupResponse = CreatePlacementGroupResponse
instance FromXML CreatePlacementGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreatePlacementGroupResponse"

instance AWSRequest CreatePlacementGroup where
    type Sv CreatePlacementGroup = EC2
    type Rs CreatePlacementGroup = CreatePlacementGroupResponse

    request  = post "CreatePlacementGroup"
    response = nullaryResponse CreatePlacementGroupResponse
