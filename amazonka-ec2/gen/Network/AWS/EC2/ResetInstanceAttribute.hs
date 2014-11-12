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

-- Module      : Network.AWS.EC2.ResetInstanceAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Resets an attribute of an instance to its default value. To reset the
-- kernel or ramdisk, the instance must be in a stopped state. To reset the
-- SourceDestCheck, the instance can be either running or stopped. The
-- SourceDestCheck attribute controls whether source/destination checking is
-- enabled. The default value is true, which means checking is enabled. This
-- value must be false for a NAT instance to perform NAT. For more
-- information, see NAT Instances in the Amazon Virtual Private Cloud User
-- Guide.
module Network.AWS.EC2.ResetInstanceAttribute
    (
    -- * Request
      ResetInstanceAttribute
    -- ** Request constructor
    , resetInstanceAttribute
    -- ** Request lenses
    , ria1Attribute
    , ria1DryRun
    , ria1InstanceId

    -- * Response
    , ResetInstanceAttributeResponse
    -- ** Response constructor
    , resetInstanceAttributeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data ResetInstanceAttribute = ResetInstanceAttribute
    { _ria1Attribute  :: Text
    , _ria1DryRun     :: Maybe Bool
    , _ria1InstanceId :: Text
    } (Eq, Ord, Show, Generic)

-- | 'ResetInstanceAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ria1Attribute' @::@ 'Text'
--
-- * 'ria1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'ria1InstanceId' @::@ 'Text'
--
resetInstanceAttribute :: Text -- ^ 'ria1InstanceId'
                       -> Text -- ^ 'ria1Attribute'
                       -> ResetInstanceAttribute
resetInstanceAttribute p1 p2 = ResetInstanceAttribute
    { _ria1InstanceId = p1
    , _ria1Attribute  = p2
    , _ria1DryRun     = Nothing
    }

-- | The attribute to reset.
ria1Attribute :: Lens' ResetInstanceAttribute Text
ria1Attribute = lens _ria1Attribute (\s a -> s { _ria1Attribute = a })

ria1DryRun :: Lens' ResetInstanceAttribute (Maybe Bool)
ria1DryRun = lens _ria1DryRun (\s a -> s { _ria1DryRun = a })

-- | The ID of the instance.
ria1InstanceId :: Lens' ResetInstanceAttribute Text
ria1InstanceId = lens _ria1InstanceId (\s a -> s { _ria1InstanceId = a })
instance ToQuery ResetInstanceAttribute

instance ToPath ResetInstanceAttribute where
    toPath = const "/"

data ResetInstanceAttributeResponse = ResetInstanceAttributeResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'ResetInstanceAttributeResponse' constructor.
resetInstanceAttributeResponse :: ResetInstanceAttributeResponse
resetInstanceAttributeResponse = ResetInstanceAttributeResponse

instance FromXML ResetInstanceAttributeResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ResetInstanceAttributeResponse"

instance AWSRequest ResetInstanceAttribute where
    type Sv ResetInstanceAttribute = EC2
    type Rs ResetInstanceAttribute = ResetInstanceAttributeResponse

    request  = post "ResetInstanceAttribute"
    response = nullaryResponse ResetInstanceAttributeResponse
