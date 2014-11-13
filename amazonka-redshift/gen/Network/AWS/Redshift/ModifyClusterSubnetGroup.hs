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

-- Module      : Network.AWS.Redshift.ModifyClusterSubnetGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies a cluster subnet group to include the specified list of VPC
-- subnets. The operation replaces the existing list of subnets with the new
-- list of subnets.
module Network.AWS.Redshift.ModifyClusterSubnetGroup
    (
    -- * Request
      ModifyClusterSubnetGroup
    -- ** Request constructor
    , modifyClusterSubnetGroup
    -- ** Request lenses
    , mcsgClusterSubnetGroupName
    , mcsgDescription
    , mcsgSubnetIds

    -- * Response
    , ModifyClusterSubnetGroupResponse
    -- ** Response constructor
    , modifyClusterSubnetGroupResponse
    -- ** Response lenses
    , mcsgrClusterSubnetGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data ModifyClusterSubnetGroup = ModifyClusterSubnetGroup
    { _mcsgClusterSubnetGroupName :: Text
    , _mcsgDescription            :: Maybe Text
    , _mcsgSubnetIds              :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'ModifyClusterSubnetGroup' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcsgClusterSubnetGroupName' @::@ 'Text'
--
-- * 'mcsgDescription' @::@ 'Maybe' 'Text'
--
-- * 'mcsgSubnetIds' @::@ ['Text']
--
modifyClusterSubnetGroup :: Text -- ^ 'mcsgClusterSubnetGroupName'
                         -> ModifyClusterSubnetGroup
modifyClusterSubnetGroup p1 = ModifyClusterSubnetGroup
    { _mcsgClusterSubnetGroupName = p1
    , _mcsgDescription            = Nothing
    , _mcsgSubnetIds              = mempty
    }

-- | The name of the subnet group to be modified.
mcsgClusterSubnetGroupName :: Lens' ModifyClusterSubnetGroup Text
mcsgClusterSubnetGroupName =
    lens _mcsgClusterSubnetGroupName
        (\s a -> s { _mcsgClusterSubnetGroupName = a })

-- | A text description of the subnet group to be modified.
mcsgDescription :: Lens' ModifyClusterSubnetGroup (Maybe Text)
mcsgDescription = lens _mcsgDescription (\s a -> s { _mcsgDescription = a })

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
-- single request.
mcsgSubnetIds :: Lens' ModifyClusterSubnetGroup [Text]
mcsgSubnetIds = lens _mcsgSubnetIds (\s a -> s { _mcsgSubnetIds = a })

instance ToQuery ModifyClusterSubnetGroup

instance ToPath ModifyClusterSubnetGroup where
    toPath = const "/"

newtype ModifyClusterSubnetGroupResponse = ModifyClusterSubnetGroupResponse
    { _mcsgrClusterSubnetGroup :: Maybe ClusterSubnetGroup
    } deriving (Eq, Show, Generic)

-- | 'ModifyClusterSubnetGroupResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcsgrClusterSubnetGroup' @::@ 'Maybe' 'ClusterSubnetGroup'
--
modifyClusterSubnetGroupResponse :: ModifyClusterSubnetGroupResponse
modifyClusterSubnetGroupResponse = ModifyClusterSubnetGroupResponse
    { _mcsgrClusterSubnetGroup = Nothing
    }

mcsgrClusterSubnetGroup :: Lens' ModifyClusterSubnetGroupResponse (Maybe ClusterSubnetGroup)
mcsgrClusterSubnetGroup =
    lens _mcsgrClusterSubnetGroup (\s a -> s { _mcsgrClusterSubnetGroup = a })

instance FromXML ModifyClusterSubnetGroupResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ModifyClusterSubnetGroupResponse"

instance AWSRequest ModifyClusterSubnetGroup where
    type Sv ModifyClusterSubnetGroup = Redshift
    type Rs ModifyClusterSubnetGroup = ModifyClusterSubnetGroupResponse

    request  = post "ModifyClusterSubnetGroup"
    response = xmlResponse $ \h x -> ModifyClusterSubnetGroupResponse
        <$> x %| "ClusterSubnetGroup"
