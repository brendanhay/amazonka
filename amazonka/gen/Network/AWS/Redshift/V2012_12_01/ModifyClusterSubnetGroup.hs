{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
module Network.AWS.Redshift.V2012_12_01.ModifyClusterSubnetGroup where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.Redshift.V2012_12_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

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
    } deriving (Generic)

instance ToQuery ModifyClusterSubnetGroup where
    toQuery = genericToQuery def

instance AWSRequest ModifyClusterSubnetGroup where
    type Sv ModifyClusterSubnetGroup = Redshift
    type Rs ModifyClusterSubnetGroup = ModifyClusterSubnetGroupResponse

    request = post "ModifyClusterSubnetGroup"
    response _ = xmlResponse

data ModifyClusterSubnetGroupResponse = ModifyClusterSubnetGroupResponse
    { _csgcrClusterSubnetGroup :: Maybe ClusterSubnetGroup
      -- ^ Describes a subnet group.
    } deriving (Generic)

instance FromXML ModifyClusterSubnetGroupResponse where
    fromXMLOptions = xmlOptions
