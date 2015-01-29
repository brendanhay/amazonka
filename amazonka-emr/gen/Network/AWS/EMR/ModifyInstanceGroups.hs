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

-- Module      : Network.AWS.EMR.ModifyInstanceGroups
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | ModifyInstanceGroups modifies the number of nodes and configuration settings
-- of an instance group. The input parameters include the new target instance
-- count for the group and the instance group ID. The call will either succeed
-- or fail atomically.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_ModifyInstanceGroups.html>
module Network.AWS.EMR.ModifyInstanceGroups
    (
    -- * Request
      ModifyInstanceGroups
    -- ** Request constructor
    , modifyInstanceGroups
    -- ** Request lenses
    , migInstanceGroups

    -- * Response
    , ModifyInstanceGroupsResponse
    -- ** Response constructor
    , modifyInstanceGroupsResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.EMR.Types
import qualified GHC.Exts

newtype ModifyInstanceGroups = ModifyInstanceGroups
    { _migInstanceGroups :: List "InstanceGroups" InstanceGroupModifyConfig
    } deriving (Eq, Read, Show, Monoid, Semigroup)

instance GHC.Exts.IsList ModifyInstanceGroups where
    type Item ModifyInstanceGroups = InstanceGroupModifyConfig

    fromList = ModifyInstanceGroups . GHC.Exts.fromList
    toList   = GHC.Exts.toList . _migInstanceGroups

-- | 'ModifyInstanceGroups' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'migInstanceGroups' @::@ ['InstanceGroupModifyConfig']
--
modifyInstanceGroups :: ModifyInstanceGroups
modifyInstanceGroups = ModifyInstanceGroups
    { _migInstanceGroups = mempty
    }

-- | Instance groups to change.
migInstanceGroups :: Lens' ModifyInstanceGroups [InstanceGroupModifyConfig]
migInstanceGroups =
    lens _migInstanceGroups (\s a -> s { _migInstanceGroups = a })
        . _List

data ModifyInstanceGroupsResponse = ModifyInstanceGroupsResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'ModifyInstanceGroupsResponse' constructor.
modifyInstanceGroupsResponse :: ModifyInstanceGroupsResponse
modifyInstanceGroupsResponse = ModifyInstanceGroupsResponse

instance ToPath ModifyInstanceGroups where
    toPath = const "/"

instance ToQuery ModifyInstanceGroups where
    toQuery = const mempty

instance ToHeaders ModifyInstanceGroups

instance ToJSON ModifyInstanceGroups where
    toJSON ModifyInstanceGroups{..} = object
        [ "InstanceGroups" .= _migInstanceGroups
        ]

instance AWSRequest ModifyInstanceGroups where
    type Sv ModifyInstanceGroups = EMR
    type Rs ModifyInstanceGroups = ModifyInstanceGroupsResponse

    request  = post "ModifyInstanceGroups"
    response = nullResponse ModifyInstanceGroupsResponse
