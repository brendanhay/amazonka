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
    , mkModifyClusterSubnetGroupMessage
    -- ** Request lenses
    , mcsgmClusterSubnetGroupName
    , mcsgmDescription
    , mcsgmSubnetIds

    -- * Response
    , ModifyClusterSubnetGroupResponse
    -- ** Response lenses
    , csgzClusterSubnetGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyClusterSubnetGroup' request.
mkModifyClusterSubnetGroupMessage :: Text -- ^ 'mcsgmClusterSubnetGroupName'
                                  -> [Text] -- ^ 'mcsgmSubnetIds'
                                  -> ModifyClusterSubnetGroup
mkModifyClusterSubnetGroupMessage p1 p2 = ModifyClusterSubnetGroup
    { _mcsgmClusterSubnetGroupName = p1
    , _mcsgmDescription = Nothing
    , _mcsgmSubnetIds = p3
    }
{-# INLINE mkModifyClusterSubnetGroupMessage #-}

data ModifyClusterSubnetGroup = ModifyClusterSubnetGroup
    { _mcsgmClusterSubnetGroupName :: Text
      -- ^ The name of the subnet group to be modified.
    , _mcsgmDescription :: Maybe Text
      -- ^ A text description of the subnet group to be modified.
    , _mcsgmSubnetIds :: [Text]
      -- ^ An array of VPC subnet IDs. A maximum of 20 subnets can be
      -- modified in a single request.
    } deriving (Show, Generic)

-- | The name of the subnet group to be modified.
mcsgmClusterSubnetGroupName :: Lens' ModifyClusterSubnetGroup (Text)
mcsgmClusterSubnetGroupName = lens _mcsgmClusterSubnetGroupName (\s a -> s { _mcsgmClusterSubnetGroupName = a })
{-# INLINE mcsgmClusterSubnetGroupName #-}

-- | A text description of the subnet group to be modified.
mcsgmDescription :: Lens' ModifyClusterSubnetGroup (Maybe Text)
mcsgmDescription = lens _mcsgmDescription (\s a -> s { _mcsgmDescription = a })
{-# INLINE mcsgmDescription #-}

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
-- single request.
mcsgmSubnetIds :: Lens' ModifyClusterSubnetGroup ([Text])
mcsgmSubnetIds = lens _mcsgmSubnetIds (\s a -> s { _mcsgmSubnetIds = a })
{-# INLINE mcsgmSubnetIds #-}

instance ToQuery ModifyClusterSubnetGroup where
    toQuery = genericQuery def

newtype ModifyClusterSubnetGroupResponse = ModifyClusterSubnetGroupResponse
    { _csgzClusterSubnetGroup :: Maybe ClusterSubnetGroup
      -- ^ Describes a subnet group.
    } deriving (Show, Generic)

-- | Describes a subnet group.
csgzClusterSubnetGroup :: Lens' ModifyClusterSubnetGroupResponse (Maybe ClusterSubnetGroup)
csgzClusterSubnetGroup = lens _csgzClusterSubnetGroup (\s a -> s { _csgzClusterSubnetGroup = a })
{-# INLINE csgzClusterSubnetGroup #-}

instance FromXML ModifyClusterSubnetGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyClusterSubnetGroup where
    type Sv ModifyClusterSubnetGroup = Redshift
    type Rs ModifyClusterSubnetGroup = ModifyClusterSubnetGroupResponse

    request = post "ModifyClusterSubnetGroup"
    response _ = xmlResponse
