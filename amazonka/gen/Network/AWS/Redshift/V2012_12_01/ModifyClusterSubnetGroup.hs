{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.Redshift.V2012_12_01.ModifyClusterSubnetGroup where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.Redshift.V2012_12_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ModifyClusterSubnetGroup' request.
modifyClusterSubnetGroup :: Text -- ^ '_mcsgmClusterSubnetGroupName'
                         -> [Text] -- ^ '_mcsgmSubnetIds'
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

makeLenses ''ModifyClusterSubnetGroup

instance ToQuery ModifyClusterSubnetGroup where
    toQuery = genericToQuery def

data ModifyClusterSubnetGroupResponse = ModifyClusterSubnetGroupResponse
    { _csgcrClusterSubnetGroup :: Maybe ClusterSubnetGroup
      -- ^ Describes a subnet group.
    } deriving (Show, Generic)

makeLenses ''ModifyClusterSubnetGroupResponse

instance AWSRequest ModifyClusterSubnetGroup where
    type Sv ModifyClusterSubnetGroup = Redshift
    type Rs ModifyClusterSubnetGroup = ModifyClusterSubnetGroupResponse

    request = post "ModifyClusterSubnetGroup"
    response _ = cursorResponse $ \hs xml ->
        pure ModifyClusterSubnetGroupResponse
            <*> xml %|? "ClusterSubnetGroup"
