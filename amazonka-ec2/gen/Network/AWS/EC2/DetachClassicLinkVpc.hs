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

-- Module      : Network.AWS.EC2.DetachClassicLinkVpc
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

-- | Unlinks (detaches) a linked EC2-Classic instance from a VPC. After the
-- instance has been unlinked, the VPC security groups are no longer associated
-- with it. An instance is automatically unlinked from a VPC when it's stopped.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DetachClassicLinkVpc.html>
module Network.AWS.EC2.DetachClassicLinkVpc
    (
    -- * Request
      DetachClassicLinkVpc
    -- ** Request constructor
    , detachClassicLinkVpc
    -- ** Request lenses
    , dclvDryRun
    , dclvInstanceId
    , dclvVpcId

    -- * Response
    , DetachClassicLinkVpcResponse
    -- ** Response constructor
    , detachClassicLinkVpcResponse
    -- ** Response lenses
    , dclvrReturn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DetachClassicLinkVpc = DetachClassicLinkVpc
    { _dclvDryRun     :: Maybe Bool
    , _dclvInstanceId :: Text
    , _dclvVpcId      :: Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DetachClassicLinkVpc' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dclvDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dclvInstanceId' @::@ 'Text'
--
-- * 'dclvVpcId' @::@ 'Text'
--
detachClassicLinkVpc :: Text -- ^ 'dclvInstanceId'
                     -> Text -- ^ 'dclvVpcId'
                     -> DetachClassicLinkVpc
detachClassicLinkVpc p1 p2 = DetachClassicLinkVpc
    { _dclvInstanceId = p1
    , _dclvVpcId      = p2
    , _dclvDryRun     = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dclvDryRun :: Lens' DetachClassicLinkVpc (Maybe Bool)
dclvDryRun = lens _dclvDryRun (\s a -> s { _dclvDryRun = a })

-- | The ID of the instance to unlink from the VPC.
dclvInstanceId :: Lens' DetachClassicLinkVpc Text
dclvInstanceId = lens _dclvInstanceId (\s a -> s { _dclvInstanceId = a })

-- | The ID of the VPC to which the instance is linked.
dclvVpcId :: Lens' DetachClassicLinkVpc Text
dclvVpcId = lens _dclvVpcId (\s a -> s { _dclvVpcId = a })

newtype DetachClassicLinkVpcResponse = DetachClassicLinkVpcResponse
    { _dclvrReturn :: Maybe Bool
    } deriving (Eq, Ord, Read, Show)

-- | 'DetachClassicLinkVpcResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dclvrReturn' @::@ 'Maybe' 'Bool'
--
detachClassicLinkVpcResponse :: DetachClassicLinkVpcResponse
detachClassicLinkVpcResponse = DetachClassicLinkVpcResponse
    { _dclvrReturn = Nothing
    }

-- | Returns 'true' if the request succeeds; otherwise, it returns an error.
dclvrReturn :: Lens' DetachClassicLinkVpcResponse (Maybe Bool)
dclvrReturn = lens _dclvrReturn (\s a -> s { _dclvrReturn = a })

instance ToPath DetachClassicLinkVpc where
    toPath = const "/"

instance ToQuery DetachClassicLinkVpc where
    toQuery DetachClassicLinkVpc{..} = mconcat
        [ "DryRun"     =? _dclvDryRun
        , "InstanceId" =? _dclvInstanceId
        , "VpcId"      =? _dclvVpcId
        ]

instance ToHeaders DetachClassicLinkVpc

instance AWSRequest DetachClassicLinkVpc where
    type Sv DetachClassicLinkVpc = EC2
    type Rs DetachClassicLinkVpc = DetachClassicLinkVpcResponse

    request  = post "DetachClassicLinkVpc"
    response = xmlResponse

instance FromXML DetachClassicLinkVpcResponse where
    parseXML x = DetachClassicLinkVpcResponse
        <$> x .@? "return"
