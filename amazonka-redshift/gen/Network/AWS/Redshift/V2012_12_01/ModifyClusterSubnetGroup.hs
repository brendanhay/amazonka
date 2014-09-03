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
mcsgmClusterSubnetGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> ModifyClusterSubnetGroup
    -> f ModifyClusterSubnetGroup
mcsgmClusterSubnetGroupName f x =
    (\y -> x { _mcsgmClusterSubnetGroupName = y })
       <$> f (_mcsgmClusterSubnetGroupName x)
{-# INLINE mcsgmClusterSubnetGroupName #-}

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
-- single request.
mcsgmSubnetIds
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> ModifyClusterSubnetGroup
    -> f ModifyClusterSubnetGroup
mcsgmSubnetIds f x =
    (\y -> x { _mcsgmSubnetIds = y })
       <$> f (_mcsgmSubnetIds x)
{-# INLINE mcsgmSubnetIds #-}

-- | A text description of the subnet group to be modified.
mcsgmDescription
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> ModifyClusterSubnetGroup
    -> f ModifyClusterSubnetGroup
mcsgmDescription f x =
    (\y -> x { _mcsgmDescription = y })
       <$> f (_mcsgmDescription x)
{-# INLINE mcsgmDescription #-}

instance ToQuery ModifyClusterSubnetGroup where
    toQuery = genericQuery def

data ModifyClusterSubnetGroupResponse = ModifyClusterSubnetGroupResponse
    { _csgzClusterSubnetGroup :: Maybe ClusterSubnetGroup
      -- ^ Describes a subnet group.
    } deriving (Show, Generic)

-- | Describes a subnet group.
csgzClusterSubnetGroup
    :: Functor f
    => (Maybe ClusterSubnetGroup
    -> f (Maybe ClusterSubnetGroup))
    -> ModifyClusterSubnetGroupResponse
    -> f ModifyClusterSubnetGroupResponse
csgzClusterSubnetGroup f x =
    (\y -> x { _csgzClusterSubnetGroup = y })
       <$> f (_csgzClusterSubnetGroup x)
{-# INLINE csgzClusterSubnetGroup #-}

instance FromXML ModifyClusterSubnetGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyClusterSubnetGroup where
    type Sv ModifyClusterSubnetGroup = Redshift
    type Rs ModifyClusterSubnetGroup = ModifyClusterSubnetGroupResponse

    request = post "ModifyClusterSubnetGroup"
    response _ = xmlResponse
