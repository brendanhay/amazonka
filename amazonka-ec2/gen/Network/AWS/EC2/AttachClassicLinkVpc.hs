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

-- Module      : Network.AWS.EC2.AttachClassicLinkVpc
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

-- | Links an EC2-Classic instance to a ClassicLink-enabled VPC through one or
-- more of the VPC's security groups. You cannot link an EC2-Classic instance to
-- more than one VPC at a time. You can only link an instance that's in the 'running' state. An instance is automatically unlinked from a VPC when it's stopped -
-- you can link it to the VPC again when you restart it.
--
-- After you've linked an instance, you cannot change the VPC security groups
-- that are associated with it. To change the security groups, you must first
-- unlink the instance, and then link it again.
--
-- Linking your instance to a VPC is sometimes referred to as /attaching/ your
-- instance.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-AttachClassicLinkVpc.html>
module Network.AWS.EC2.AttachClassicLinkVpc
    (
    -- * Request
      AttachClassicLinkVpc
    -- ** Request constructor
    , attachClassicLinkVpc
    -- ** Request lenses
    , aclvDryRun
    , aclvGroups
    , aclvInstanceId
    , aclvVpcId

    -- * Response
    , AttachClassicLinkVpcResponse
    -- ** Response constructor
    , attachClassicLinkVpcResponse
    -- ** Response lenses
    , aclvrReturn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data AttachClassicLinkVpc = AttachClassicLinkVpc
    { _aclvDryRun     :: Maybe Bool
    , _aclvGroups     :: List "groupId" Text
    , _aclvInstanceId :: Text
    , _aclvVpcId      :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'AttachClassicLinkVpc' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aclvDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'aclvGroups' @::@ ['Text']
--
-- * 'aclvInstanceId' @::@ 'Text'
--
-- * 'aclvVpcId' @::@ 'Text'
--
attachClassicLinkVpc :: Text -- ^ 'aclvInstanceId'
                     -> Text -- ^ 'aclvVpcId'
                     -> AttachClassicLinkVpc
attachClassicLinkVpc p1 p2 = AttachClassicLinkVpc
    { _aclvInstanceId = p1
    , _aclvVpcId      = p2
    , _aclvDryRun     = Nothing
    , _aclvGroups     = mempty
    }

aclvDryRun :: Lens' AttachClassicLinkVpc (Maybe Bool)
aclvDryRun = lens _aclvDryRun (\s a -> s { _aclvDryRun = a })

-- | The ID of one or more of the VPC's security groups. You cannot specify
-- security groups from a different VPC.
aclvGroups :: Lens' AttachClassicLinkVpc [Text]
aclvGroups = lens _aclvGroups (\s a -> s { _aclvGroups = a }) . _List

-- | The ID of an EC2-Classic instance to link to the ClassicLink-enabled VPC.
aclvInstanceId :: Lens' AttachClassicLinkVpc Text
aclvInstanceId = lens _aclvInstanceId (\s a -> s { _aclvInstanceId = a })

-- | The ID of a ClassicLink-enabled VPC.
aclvVpcId :: Lens' AttachClassicLinkVpc Text
aclvVpcId = lens _aclvVpcId (\s a -> s { _aclvVpcId = a })

newtype AttachClassicLinkVpcResponse = AttachClassicLinkVpcResponse
    { _aclvrReturn :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'AttachClassicLinkVpcResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'aclvrReturn' @::@ 'Maybe' 'Bool'
--
attachClassicLinkVpcResponse :: AttachClassicLinkVpcResponse
attachClassicLinkVpcResponse = AttachClassicLinkVpcResponse
    { _aclvrReturn = Nothing
    }

-- | Returns 'true' if the request succeeds; otherwise, it returns an error.
aclvrReturn :: Lens' AttachClassicLinkVpcResponse (Maybe Bool)
aclvrReturn = lens _aclvrReturn (\s a -> s { _aclvrReturn = a })

instance ToPath AttachClassicLinkVpc where
    toPath = const "/"

instance ToQuery AttachClassicLinkVpc where
    toQuery AttachClassicLinkVpc{..} = mconcat
        [ "dryRun"          =? _aclvDryRun
        , "SecurityGroupId" `toQueryList` _aclvGroups
        , "instanceId"      =? _aclvInstanceId
        , "vpcId"           =? _aclvVpcId
        ]

instance ToHeaders AttachClassicLinkVpc

instance AWSRequest AttachClassicLinkVpc where
    type Sv AttachClassicLinkVpc = EC2
    type Rs AttachClassicLinkVpc = AttachClassicLinkVpcResponse

    request  = post "AttachClassicLinkVpc"
    response = xmlResponse

instance FromXML AttachClassicLinkVpcResponse where
    parseXML x = AttachClassicLinkVpcResponse
        <$> x .@? "return"
