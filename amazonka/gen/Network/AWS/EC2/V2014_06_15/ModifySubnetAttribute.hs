{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
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
module Network.AWS.EC2.V2014_06_15.ModifySubnetAttribute where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_06_15.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'ModifySubnetAttribute' request.
modifySubnetAttribute :: Text -- ^ '_msarSubnetId'
                      -> ModifySubnetAttribute
modifySubnetAttribute p1 = ModifySubnetAttribute
    { _msarSubnetId = p1
    , _msarMapPublicIpOnLaunch = Nothing
    }

data ModifySubnetAttribute = ModifySubnetAttribute
    { _msarSubnetId :: Text
      -- ^ The ID of the subnet.
    , _msarMapPublicIpOnLaunch :: Maybe AttributeBooleanValue
      -- ^ 
    } deriving (Generic)

instance ToQuery ModifySubnetAttribute where
    toQuery = genericToQuery def

instance AWSRequest ModifySubnetAttribute where
    type Sv ModifySubnetAttribute = EC2
    type Rs ModifySubnetAttribute = ModifySubnetAttributeResponse

    request = post "ModifySubnetAttribute"
    response _ _ = return (Right ModifySubnetAttributeResponse)

data ModifySubnetAttributeResponse = ModifySubnetAttributeResponse
    deriving (Eq, Show, Generic)
