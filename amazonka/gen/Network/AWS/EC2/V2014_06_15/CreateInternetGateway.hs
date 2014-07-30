{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.CreateInternetGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates an Internet gateway for use with a VPC. After creating the Internet
-- gateway, you attach it to a VPC using AttachInternetGateway. For more
-- information about your VPC and Internet gateway, see the Amazon Virtual
-- Private Cloud User Guide. Example This example creates an Internet gateway.
-- https://ec2.amazonaws.com/?Action=CreateInternetGateway &amp;AUTHPARAMS
-- 59dbff89-35bd-4eac-99ed-be587EXAMPLE igw-eaad4883.
module Network.AWS.EC2.V2014_06_15.CreateInternetGateway where

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

-- | Minimum specification for a 'CreateInternetGateway' request.
createInternetGateway :: CreateInternetGateway
createInternetGateway = CreateInternetGateway
    { _cigrDryRun = Nothing
    }

data CreateInternetGateway = CreateInternetGateway
    { _cigrDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery CreateInternetGateway where
    toQuery = genericToQuery def

instance AWSRequest CreateInternetGateway where
    type Sv CreateInternetGateway = EC2
    type Rs CreateInternetGateway = CreateInternetGatewayResponse

    request = post "CreateInternetGateway"
    response _ = xmlResponse

data CreateInternetGatewayResponse = CreateInternetGatewayResponse
    { _cigsInternetGateway :: Maybe InternetGateway
      -- ^ Information about the Internet gateway.
    } deriving (Generic)

instance FromXML CreateInternetGatewayResponse where
    fromXMLOptions = xmlOptions
