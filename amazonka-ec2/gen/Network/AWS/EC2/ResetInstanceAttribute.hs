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

-- Module      : Network.AWS.EC2.ResetInstanceAttribute
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

-- | Resets an attribute of an instance to its default value. To reset the 'kernel'
-- or 'ramdisk', the instance must be in a stopped state. To reset the 'SourceDestCheck', the instance can be either running or stopped.
--
-- The 'SourceDestCheck' attribute controls whether source/destination checking
-- is enabled. The default value is 'true', which means checking is enabled. This
-- value must be 'false' for a NAT instance to perform NAT. For more information,
-- see <http://docs.aws.amazon.com/AmazonVPC/latest/UserGuide/VPC_NAT_Instance.html NAT Instances> in the /Amazon Virtual Private Cloud User Guide/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ResetInstanceAttribute.html>
module Network.AWS.EC2.ResetInstanceAttribute
    (
    -- * Request
      ResetInstanceAttribute
    -- ** Request constructor
    , resetInstanceAttribute
    -- ** Request lenses
    , riaAttribute
    , riaDryRun
    , riaInstanceId

    -- * Response
    , ResetInstanceAttributeResponse
    -- ** Response constructor
    , resetInstanceAttributeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ResetInstanceAttribute = ResetInstanceAttribute
    { _riaAttribute  :: InstanceAttributeName
    , _riaDryRun     :: Maybe Bool
    , _riaInstanceId :: Text
    } deriving (Eq, Read, Show)

-- | 'ResetInstanceAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'riaAttribute' @::@ 'InstanceAttributeName'
--
-- * 'riaDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'riaInstanceId' @::@ 'Text'
--
resetInstanceAttribute :: Text -- ^ 'riaInstanceId'
                       -> InstanceAttributeName -- ^ 'riaAttribute'
                       -> ResetInstanceAttribute
resetInstanceAttribute p1 p2 = ResetInstanceAttribute
    { _riaInstanceId = p1
    , _riaAttribute  = p2
    , _riaDryRun     = Nothing
    }

-- | The attribute to reset.
riaAttribute :: Lens' ResetInstanceAttribute InstanceAttributeName
riaAttribute = lens _riaAttribute (\s a -> s { _riaAttribute = a })

riaDryRun :: Lens' ResetInstanceAttribute (Maybe Bool)
riaDryRun = lens _riaDryRun (\s a -> s { _riaDryRun = a })

-- | The ID of the instance.
riaInstanceId :: Lens' ResetInstanceAttribute Text
riaInstanceId = lens _riaInstanceId (\s a -> s { _riaInstanceId = a })

data ResetInstanceAttributeResponse = ResetInstanceAttributeResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'ResetInstanceAttributeResponse' constructor.
resetInstanceAttributeResponse :: ResetInstanceAttributeResponse
resetInstanceAttributeResponse = ResetInstanceAttributeResponse

instance ToPath ResetInstanceAttribute where
    toPath = const "/"

instance ToQuery ResetInstanceAttribute where
    toQuery ResetInstanceAttribute{..} = mconcat
        [ "attribute"  =? _riaAttribute
        , "dryRun"     =? _riaDryRun
        , "instanceId" =? _riaInstanceId
        ]

instance ToHeaders ResetInstanceAttribute

instance AWSRequest ResetInstanceAttribute where
    type Sv ResetInstanceAttribute = EC2
    type Rs ResetInstanceAttribute = ResetInstanceAttributeResponse

    request  = post "ResetInstanceAttribute"
    response = nullResponse ResetInstanceAttributeResponse
