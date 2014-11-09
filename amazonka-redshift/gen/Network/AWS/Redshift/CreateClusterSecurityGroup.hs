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

-- Module      : Network.AWS.Redshift.CreateClusterSecurityGroup
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a new Amazon Redshift security group. You use security groups to
-- control access to non-VPC clusters. For information about managing security
-- groups, go to Amazon Redshift Cluster Security Groups in the Amazon
-- Redshift Management Guide.
module Network.AWS.Redshift.CreateClusterSecurityGroup
    (
    -- * Request
      CreateClusterSecurityGroupMessage
    -- ** Request constructor
    , createClusterSecurityGroupMessage
    -- ** Request lenses
    , ccsgmClusterSecurityGroupName
    , ccsgmDescription

    -- * Response
    , CreateClusterSecurityGroupResult
    -- ** Response constructor
    , createClusterSecurityGroupResult
    -- ** Response lenses
    , ccsgrClusterSecurityGroup
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

data CreateClusterSecurityGroupMessage = CreateClusterSecurityGroupMessage
    { _ccsgmClusterSecurityGroupName :: Text
    , _ccsgmDescription              :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateClusterSecurityGroupMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgmClusterSecurityGroupName' @::@ 'Text'
--
-- * 'ccsgmDescription' @::@ 'Text'
--
createClusterSecurityGroupMessage :: Text -- ^ 'ccsgmClusterSecurityGroupName'
                                  -> Text -- ^ 'ccsgmDescription'
                                  -> CreateClusterSecurityGroupMessage
createClusterSecurityGroupMessage p1 p2 = CreateClusterSecurityGroupMessage
    { _ccsgmClusterSecurityGroupName = p1
    , _ccsgmDescription              = p2
    }

-- | The name for the security group. Amazon Redshift stores the value as a
-- lowercase string. Constraints: Must contain no more than 255 alphanumeric
-- characters or hyphens. Must not be "Default". Must be unique for all
-- security groups that are created by your AWS account. Example:
-- examplesecuritygroup.
ccsgmClusterSecurityGroupName :: Lens' CreateClusterSecurityGroupMessage Text
ccsgmClusterSecurityGroupName =
    lens _ccsgmClusterSecurityGroupName
        (\s a -> s { _ccsgmClusterSecurityGroupName = a })

-- | A description for the security group.
ccsgmDescription :: Lens' CreateClusterSecurityGroupMessage Text
ccsgmDescription = lens _ccsgmDescription (\s a -> s { _ccsgmDescription = a })

instance ToPath CreateClusterSecurityGroupMessage where
    toPath = const "/"

instance ToQuery CreateClusterSecurityGroupMessage

newtype CreateClusterSecurityGroupResult = CreateClusterSecurityGroupResult
    { _ccsgrClusterSecurityGroup :: Maybe ClusterSecurityGroup
    } deriving (Eq, Show, Generic)

-- | 'CreateClusterSecurityGroupResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ccsgrClusterSecurityGroup' @::@ 'Maybe' 'ClusterSecurityGroup'
--
createClusterSecurityGroupResult :: CreateClusterSecurityGroupResult
createClusterSecurityGroupResult = CreateClusterSecurityGroupResult
    { _ccsgrClusterSecurityGroup = Nothing
    }

ccsgrClusterSecurityGroup :: Lens' CreateClusterSecurityGroupResult (Maybe ClusterSecurityGroup)
ccsgrClusterSecurityGroup =
    lens _ccsgrClusterSecurityGroup
        (\s a -> s { _ccsgrClusterSecurityGroup = a })

instance AWSRequest CreateClusterSecurityGroupMessage where
    type Sv CreateClusterSecurityGroupMessage = Redshift
    type Rs CreateClusterSecurityGroupMessage = CreateClusterSecurityGroupResult

    request  = post "CreateClusterSecurityGroup"
    response = const . xmlResponse $ \h x -> CreateClusterSecurityGroupResult
newtype
