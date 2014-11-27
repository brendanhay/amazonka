{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.CreateNetworkAcl
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Creates a network ACL in a VPC. Network ACLs provide an optional layer of
-- security (in addition to security groups) for the instances in your VPC.
--
-- For more information about network ACLs, see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_ACLs.html Network ACLs> in the /AmazonVirtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CreateNetworkAcl.html>
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
import qualified GHC.Exts

data CreateNetworkAcl = CreateNetworkAcl
    { _cnaDryRun :: Maybe Bool
    , _cnaVpcId  :: Text
    } deriving (Eq, Ord, Show)

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

newtype CreateNetworkAclResponse = CreateNetworkAclResponse
    { _cnarNetworkAcl :: Maybe NetworkAcl
    } deriving (Eq, Show)

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

instance ToPath CreateNetworkAcl where
    toPath = const "/"

instance ToQuery CreateNetworkAcl where
    toQuery CreateNetworkAcl{..} = mconcat
        [ "dryRun" =? _cnaDryRun
        , "vpcId"  =? _cnaVpcId
        ]

instance ToHeaders CreateNetworkAcl

instance AWSRequest CreateNetworkAcl where
    type Sv CreateNetworkAcl = EC2
    type Rs CreateNetworkAcl = CreateNetworkAclResponse

    request  = post "CreateNetworkAcl"
    response = xmlResponse

instance FromXML CreateNetworkAclResponse where
    parseXML x = CreateNetworkAclResponse
        <$> x .@? "networkAcl"
