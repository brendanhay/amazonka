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
      ModifyClusterSubnetGroupMessage
    -- ** Request constructor
    , modifyClusterSubnetGroupMessage
    -- ** Request lenses
    , mcsgmClusterSubnetGroupName
    , mcsgmDescription
    , mcsgmSubnetIds

    -- * Response
    , ModifyClusterSubnetGroupResult
    -- ** Response constructor
    , modifyClusterSubnetGroupResult
    -- ** Response lenses
    , mcsgrClusterSubnetGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data ModifyClusterSubnetGroupMessage = ModifyClusterSubnetGroupMessage
    { _mcsgmClusterSubnetGroupName :: Text
    , _mcsgmDescription            :: Maybe Text
    , _mcsgmSubnetIds              :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'ModifyClusterSubnetGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcsgmClusterSubnetGroupName' @::@ 'Text'
--
-- * 'mcsgmDescription' @::@ 'Maybe' 'Text'
--
-- * 'mcsgmSubnetIds' @::@ ['Text']
--
modifyClusterSubnetGroupMessage :: Text -- ^ 'mcsgmClusterSubnetGroupName'
                                -> ModifyClusterSubnetGroupMessage
modifyClusterSubnetGroupMessage p1 = ModifyClusterSubnetGroupMessage
    { _mcsgmClusterSubnetGroupName = p1
    , _mcsgmDescription            = Nothing
    , _mcsgmSubnetIds              = mempty
    }

-- | The name of the subnet group to be modified.
mcsgmClusterSubnetGroupName :: Lens' ModifyClusterSubnetGroupMessage Text
mcsgmClusterSubnetGroupName =
    lens _mcsgmClusterSubnetGroupName
        (\s a -> s { _mcsgmClusterSubnetGroupName = a })

-- | A text description of the subnet group to be modified.
mcsgmDescription :: Lens' ModifyClusterSubnetGroupMessage (Maybe Text)
mcsgmDescription = lens _mcsgmDescription (\s a -> s { _mcsgmDescription = a })

-- | An array of VPC subnet IDs. A maximum of 20 subnets can be modified in a
-- single request.
mcsgmSubnetIds :: Lens' ModifyClusterSubnetGroupMessage [Text]
mcsgmSubnetIds = lens _mcsgmSubnetIds (\s a -> s { _mcsgmSubnetIds = a })

instance ToPath ModifyClusterSubnetGroupMessage where
    toPath = const "/"

instance ToQuery ModifyClusterSubnetGroupMessage

newtype ModifyClusterSubnetGroupResult = ModifyClusterSubnetGroupResult
    { _mcsgrClusterSubnetGroup :: Maybe ClusterSubnetGroup
    } deriving (Eq, Show, Generic)

-- | 'ModifyClusterSubnetGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mcsgrClusterSubnetGroup' @::@ 'Maybe' 'ClusterSubnetGroup'
--
modifyClusterSubnetGroupResult :: ModifyClusterSubnetGroupResult
modifyClusterSubnetGroupResult = ModifyClusterSubnetGroupResult
    { _mcsgrClusterSubnetGroup = Nothing
    }

mcsgrClusterSubnetGroup :: Lens' ModifyClusterSubnetGroupResult (Maybe ClusterSubnetGroup)
mcsgrClusterSubnetGroup =
    lens _mcsgrClusterSubnetGroup (\s a -> s { _mcsgrClusterSubnetGroup = a })

instance AWSRequest ModifyClusterSubnetGroupMessage where
    type Sv ModifyClusterSubnetGroupMessage = Redshift
    type Rs ModifyClusterSubnetGroupMessage = ModifyClusterSubnetGroupResult

    request  = post "ModifyClusterSubnetGroup"
    response = const . xmlResponse $ \h x -> ModifyClusterSubnetGroupResult
newtype
