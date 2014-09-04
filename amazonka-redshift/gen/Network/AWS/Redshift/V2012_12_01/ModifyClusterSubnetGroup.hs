{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.Redshift.V2012_12_01.ModifyClusterSubnetGroup
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
module Network.AWS.Redshift.V2012_12_01.ModifyClusterSubnetGroup
    (
    -- * Request
      ModifyClusterSubnetGroup
    -- ** Request constructor
    , modifyClusterSubnetGroup
    -- ** Request lenses
    , mcsgmClusterSubnetGroupName
    , mcsgmSubnetIds
    , mcsgmDescription

    -- * Response
    , ModifyClusterSubnetGroupResponse
    -- ** Response lenses
    , csgzClusterSubnetGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ModifyClusterSubnetGroup' request.
modifyClusterSubnetGroup :: Text -- ^ 'mcsgmClusterSubnetGroupName'
                         -> [Text] -- ^ 'mcsgmSubnetIds'
                         -> ModifyClusterSubnetGroup
modifyClusterSubnetGroup p1 p2 = ModifyClusterSubnetGroup
    { _mcsgmClusterSubnetGroupName = p1
    , _mcsgmSubnetIds = p2
    , _mcsgmDescription = Nothing
    }
{-# INLINE modifyClusterSubnetGroup #-}

data ModifyClusterSubnetGroup = ModifyClusterSubnetGroup
    { _mcsgmClusterSubnetGroupName :: Text
      -- ^ The name of the subnet group to be modified.
    , _mcsgmSubnetIds :: [Text]
      -- ^ An array of VPC subnet IDs. A maximum of 20 subnets can be
      -- modified in a single request.
    , _mcsgmDescription :: Maybe Text
      -- ^ A text description of the subnet group to be modified.
    } deriving (Show, Generic)

-- | The name of the subnet group to be modified.
mcsgmClusterSubnetGroupName :: Lens' ModifyClusterSubnetGroup (Text)
mcsgmClusterSubnetGroupName f x =
    f (_mcsgmClusterSubnetGroupName x)
        <&> \y -> x { _mcsgmClusterSubnetGroupName = y }
{-# INLINE mcsgmClusterSubnetGroupName #-}

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
-- single request.
mcsgmSubnetIds :: Lens' ModifyClusterSubnetGroup ([Text])
mcsgmSubnetIds f x =
    f (_mcsgmSubnetIds x)
        <&> \y -> x { _mcsgmSubnetIds = y }
{-# INLINE mcsgmSubnetIds #-}

-- | A text description of the subnet group to be modified.
mcsgmDescription :: Lens' ModifyClusterSubnetGroup (Maybe Text)
mcsgmDescription f x =
    f (_mcsgmDescription x)
        <&> \y -> x { _mcsgmDescription = y }
{-# INLINE mcsgmDescription #-}

instance ToQuery ModifyClusterSubnetGroup where
    toQuery = genericQuery def

data ModifyClusterSubnetGroupResponse = ModifyClusterSubnetGroupResponse
    { _csgzClusterSubnetGroup :: Maybe ClusterSubnetGroup
      -- ^ Describes a subnet group.
    } deriving (Show, Generic)

-- | Describes a subnet group.
csgzClusterSubnetGroup :: Lens' ModifyClusterSubnetGroupResponse (Maybe ClusterSubnetGroup)
csgzClusterSubnetGroup f x =
    f (_csgzClusterSubnetGroup x)
        <&> \y -> x { _csgzClusterSubnetGroup = y }
{-# INLINE csgzClusterSubnetGroup #-}

instance FromXML ModifyClusterSubnetGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyClusterSubnetGroup where
    type Sv ModifyClusterSubnetGroup = Redshift
    type Rs ModifyClusterSubnetGroup = ModifyClusterSubnetGroupResponse

    request = post "ModifyClusterSubnetGroup"
    response _ = xmlResponse
