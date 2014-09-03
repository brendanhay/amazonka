{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ModifySubnetAttribute
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Modifies a subnet attribute. Example This example modifies the attribute
-- for subnet-1a2b3c4d to specify that all instances launched into this subnet
-- are assigned a public IP address.
-- https://ec2.amazonaws.com/?Action=ModifySubnetAttribute
-- &amp;SubnetId=subnet-1a2b3c4d &amp;MapPublicIpOnLaunch.Value=true
-- &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.ModifySubnetAttribute
    (
    -- * Request
      ModifySubnetAttribute
    -- ** Default constructor
    , modifySubnetAttribute
    -- ** Accessors and lenses
    , _msasSubnetId
    , msasSubnetId
    , _msasMapPublicIpOnLaunch
    , msasMapPublicIpOnLaunch

    -- * Response
    , ModifySubnetAttributeResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ModifySubnetAttribute' request.
modifySubnetAttribute :: Text -- ^ 'msasSubnetId'
                      -> ModifySubnetAttribute
modifySubnetAttribute p1 = ModifySubnetAttribute
    { _msasSubnetId = p1
    , _msasMapPublicIpOnLaunch = Nothing
    }

data ModifySubnetAttribute = ModifySubnetAttribute

makeSiglessLenses ''ModifySubnetAttribute

instance ToQuery ModifySubnetAttribute where
    toQuery = genericQuery def

data ModifySubnetAttributeResponse = ModifySubnetAttributeResponse
    deriving (Eq, Show, Generic)

makeSiglessLenses ''ModifySubnetAttributeResponse

instance AWSRequest ModifySubnetAttribute where
    type Sv ModifySubnetAttribute = EC2
    type Rs ModifySubnetAttribute = ModifySubnetAttributeResponse

    request = post "ModifySubnetAttribute"
    response _ = nullaryResponse ModifySubnetAttributeResponse

-- | The ID of the subnet.
msasSubnetId :: Lens' ModifySubnetAttribute (Text)

-- | 
msasMapPublicIpOnLaunch :: Lens' ModifySubnetAttribute (Maybe AttributeBooleanValue)
