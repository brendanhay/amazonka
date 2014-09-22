{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE StandaloneDeriving          #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
    , mcsgSubnetIdentifier

    -- * Response
    , ModifyClusterSubnetGroupResponse
    -- ** Response constructor
    , modifyClusterSubnetGroupResponse
    -- ** Response lenses
    , mcsgrClusterSubnetGroup
    ) where

import Network.AWS.Request.Query
import Network.AWS.Redshift.Types
import Network.AWS.Prelude

-- | 
data ModifyClusterSubnetGroup = ModifyClusterSubnetGroup
    { _mcsgClusterSubnetGroupName :: Text
    , _mcsgDescription :: Maybe Text
    , _mcsgSubnetIdentifier :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyClusterSubnetGroup' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterSubnetGroupName ::@ @Text@
--
-- * @Description ::@ @Maybe Text@
--
-- * @SubnetIdentifier ::@ @[Text]@
--
modifyClusterSubnetGroup :: Text -- ^ 'mcsgClusterSubnetGroupName'
                         -> [Text] -- ^ 'mcsgSubnetIdentifier'
                         -> ModifyClusterSubnetGroup
modifyClusterSubnetGroup p1 p3 = ModifyClusterSubnetGroup
    { _mcsgClusterSubnetGroupName = p1
    , _mcsgDescription = Nothing
    , _mcsgSubnetIdentifier = p3
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
mcsgSubnetIdentifier :: Lens' ModifyClusterSubnetGroup [Text]
mcsgSubnetIdentifier =
    lens _mcsgSubnetIdentifier (\s a -> s { _mcsgSubnetIdentifier = a })

instance ToQuery ModifyClusterSubnetGroup where
    toQuery = genericQuery def

newtype ModifyClusterSubnetGroupResponse = ModifyClusterSubnetGroupResponse
    { _mcsgrClusterSubnetGroup :: Maybe ClusterSubnetGroup
    } deriving (Eq, Ord, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ModifyClusterSubnetGroupResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ClusterSubnetGroup ::@ @Maybe ClusterSubnetGroup@
--
modifyClusterSubnetGroupResponse :: ModifyClusterSubnetGroupResponse
modifyClusterSubnetGroupResponse = ModifyClusterSubnetGroupResponse
    { _mcsgrClusterSubnetGroup = Nothing
    }

-- | Describes a subnet group.
mcsgrClusterSubnetGroup :: Lens' ModifyClusterSubnetGroupResponse (Maybe ClusterSubnetGroup)
mcsgrClusterSubnetGroup =
    lens _mcsgrClusterSubnetGroup
         (\s a -> s { _mcsgrClusterSubnetGroup = a })

instance FromXML ModifyClusterSubnetGroupResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ModifyClusterSubnetGroup where
    type Sv ModifyClusterSubnetGroup = Redshift
    type Rs ModifyClusterSubnetGroup = ModifyClusterSubnetGroupResponse

    request = post "ModifyClusterSubnetGroup"
    response _ = xmlResponse
