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

-- Module      : Network.AWS.EC2.CreateNetworkAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates a network ACL in a VPC. Network ACLs provide an optional layer of
-- security (in addition to security groups) for the instances in your VPC.
-- For more information about network ACLs, see Network ACLs in the Amazon
-- Virtual Private Cloud User Guide.
module Network.AWS.EC2.CreateNetworkAcl
    (
    -- * Request
      CreateNetworkAcl
    -- ** Request constructor
    , createNetworkAcl
    -- ** Request lenses
    , cnaDryRun
    , cnaVpcId

    -- * Response
    , CreateNetworkAclResponse
    -- ** Response constructor
    , createNetworkAclResponse
    -- ** Response lenses
    , cnarNetworkAcl
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data CreateNetworkAcl = CreateNetworkAcl
    { _cnaDryRun :: Maybe Bool
    , _cnaVpcId  :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CreateNetworkAcl' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cnaDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'cnaVpcId' @::@ 'Text'
--
createNetworkAcl :: Text -- ^ 'cnaVpcId'
                 -> CreateNetworkAcl
createNetworkAcl p1 = CreateNetworkAcl
    { _cnaVpcId  = p1
    , _cnaDryRun = Nothing
    }

cnaDryRun :: Lens' CreateNetworkAcl (Maybe Bool)
cnaDryRun = lens _cnaDryRun (\s a -> s { _cnaDryRun = a })

-- | The ID of the VPC.
cnaVpcId :: Lens' CreateNetworkAcl Text
cnaVpcId = lens _cnaVpcId (\s a -> s { _cnaVpcId = a })

instance ToQuery CreateNetworkAcl

instance ToPath CreateNetworkAcl where
    toPath = const "/"

newtype CreateNetworkAclResponse = CreateNetworkAclResponse
    { _cnarNetworkAcl :: Maybe NetworkAcl
    } deriving (Eq, Show, Generic)

-- | 'CreateNetworkAclResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cnarNetworkAcl' @::@ 'Maybe' 'NetworkAcl'
--
createNetworkAclResponse :: CreateNetworkAclResponse
createNetworkAclResponse = CreateNetworkAclResponse
    { _cnarNetworkAcl = Nothing
    }

-- | Information about the network ACL.
cnarNetworkAcl :: Lens' CreateNetworkAclResponse (Maybe NetworkAcl)
cnarNetworkAcl = lens _cnarNetworkAcl (\s a -> s { _cnarNetworkAcl = a })

instance FromXML CreateNetworkAclResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "CreateNetworkAclResponse"

instance AWSRequest CreateNetworkAcl where
    type Sv CreateNetworkAcl = EC2
    type Rs CreateNetworkAcl = CreateNetworkAclResponse

    request  = post "CreateNetworkAcl"
    response = xmlResponse $ \h x -> CreateNetworkAclResponse
        <$> x %| "networkAcl"
