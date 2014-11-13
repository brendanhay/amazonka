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

-- Module      : Network.AWS.EC2.ReplaceNetworkAclAssociation
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Changes which network ACL a subnet is associated with. By default when you
-- create a subnet, it's automatically associated with the default network
-- ACL. For more information about network ACLs, see Network ACLs in the
-- Amazon Virtual Private Cloud User Guide.
module Network.AWS.EC2.ReplaceNetworkAclAssociation
    (
    -- * Request
      ReplaceNetworkAclAssociation
    -- ** Request constructor
    , replaceNetworkAclAssociation
    -- ** Request lenses
    , rnaaAssociationId
    , rnaaDryRun
    , rnaaNetworkAclId

    -- * Response
    , ReplaceNetworkAclAssociationResponse
    -- ** Response constructor
    , replaceNetworkAclAssociationResponse
    -- ** Response lenses
    , rnaarNewAssociationId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ReplaceNetworkAclAssociation = ReplaceNetworkAclAssociation
    { _rnaaAssociationId :: Text
    , _rnaaDryRun        :: Maybe Bool
    , _rnaaNetworkAclId  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ReplaceNetworkAclAssociation' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rnaaAssociationId' @::@ 'Text'
--
-- * 'rnaaDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'rnaaNetworkAclId' @::@ 'Text'
--
replaceNetworkAclAssociation :: Text -- ^ 'rnaaAssociationId'
                             -> Text -- ^ 'rnaaNetworkAclId'
                             -> ReplaceNetworkAclAssociation
replaceNetworkAclAssociation p1 p2 = ReplaceNetworkAclAssociation
    { _rnaaAssociationId = p1
    , _rnaaNetworkAclId  = p2
    , _rnaaDryRun        = Nothing
    }

-- | The ID of the current association between the original network ACL and
-- the subnet.
rnaaAssociationId :: Lens' ReplaceNetworkAclAssociation Text
rnaaAssociationId =
    lens _rnaaAssociationId (\s a -> s { _rnaaAssociationId = a })

rnaaDryRun :: Lens' ReplaceNetworkAclAssociation (Maybe Bool)
rnaaDryRun = lens _rnaaDryRun (\s a -> s { _rnaaDryRun = a })

-- | The ID of the new network ACL to associate with the subnet.
rnaaNetworkAclId :: Lens' ReplaceNetworkAclAssociation Text
rnaaNetworkAclId = lens _rnaaNetworkAclId (\s a -> s { _rnaaNetworkAclId = a })

instance ToQuery ReplaceNetworkAclAssociation

instance ToPath ReplaceNetworkAclAssociation where
    toPath = const "/"

newtype ReplaceNetworkAclAssociationResponse = ReplaceNetworkAclAssociationResponse
    { _rnaarNewAssociationId :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'ReplaceNetworkAclAssociationResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rnaarNewAssociationId' @::@ 'Maybe' 'Text'
--
replaceNetworkAclAssociationResponse :: ReplaceNetworkAclAssociationResponse
replaceNetworkAclAssociationResponse = ReplaceNetworkAclAssociationResponse
    { _rnaarNewAssociationId = Nothing
    }

-- | The ID of the new association.
rnaarNewAssociationId :: Lens' ReplaceNetworkAclAssociationResponse (Maybe Text)
rnaarNewAssociationId =
    lens _rnaarNewAssociationId (\s a -> s { _rnaarNewAssociationId = a })

instance AWSRequest ReplaceNetworkAclAssociation where
    type Sv ReplaceNetworkAclAssociation = EC2
    type Rs ReplaceNetworkAclAssociation = ReplaceNetworkAclAssociationResponse

    request  = post "ReplaceNetworkAclAssociation"
    response = xmlResponse $ \h x -> ReplaceNetworkAclAssociationResponse
        <$> x %| "newAssociationId"
