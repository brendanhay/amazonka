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

-- Module      : Network.AWS.EC2.ModifySubnetAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies a subnet attribute.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ModifySubnetAttribute.html>
module Network.AWS.EC2.ModifySubnetAttribute
    (
    -- * Request
      ModifySubnetAttribute
    -- ** Request constructor
    , modifySubnetAttribute
    -- ** Request lenses
    , msaMapPublicIpOnLaunch
    , msaSubnetId

    -- * Response
    , ModifySubnetAttributeResponse
    -- ** Response constructor
    , modifySubnetAttributeResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data ModifySubnetAttribute = ModifySubnetAttribute
    { _msaMapPublicIpOnLaunch :: Maybe AttributeBooleanValue
    , _msaSubnetId            :: Text
    } deriving (Eq, Show)

-- | 'ModifySubnetAttribute' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'msaMapPublicIpOnLaunch' @::@ 'Maybe' 'AttributeBooleanValue'
--
-- * 'msaSubnetId' @::@ 'Text'
--
modifySubnetAttribute :: Text -- ^ 'msaSubnetId'
                      -> ModifySubnetAttribute
modifySubnetAttribute p1 = ModifySubnetAttribute
    { _msaSubnetId            = p1
    , _msaMapPublicIpOnLaunch = Nothing
    }

msaMapPublicIpOnLaunch :: Lens' ModifySubnetAttribute (Maybe AttributeBooleanValue)
msaMapPublicIpOnLaunch =
    lens _msaMapPublicIpOnLaunch (\s a -> s { _msaMapPublicIpOnLaunch = a })

-- | The ID of the subnet.
msaSubnetId :: Lens' ModifySubnetAttribute Text
msaSubnetId = lens _msaSubnetId (\s a -> s { _msaSubnetId = a })

data ModifySubnetAttributeResponse = ModifySubnetAttributeResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'ModifySubnetAttributeResponse' constructor.
modifySubnetAttributeResponse :: ModifySubnetAttributeResponse
modifySubnetAttributeResponse = ModifySubnetAttributeResponse

instance ToPath ModifySubnetAttribute where
    toPath = const "/"

instance ToQuery ModifySubnetAttribute where
    toQuery ModifySubnetAttribute{..} = mconcat
        [ "MapPublicIpOnLaunch" =? _msaMapPublicIpOnLaunch
        , "subnetId"            =? _msaSubnetId
        ]

instance ToHeaders ModifySubnetAttribute

query

instance AWSRequest ModifySubnetAttribute where
    type Sv ModifySubnetAttribute = EC2
    type Rs ModifySubnetAttribute = ModifySubnetAttributeResponse

    request  = post "ModifySubnetAttribute"
    response = nullResponse ModifySubnetAttributeResponse
