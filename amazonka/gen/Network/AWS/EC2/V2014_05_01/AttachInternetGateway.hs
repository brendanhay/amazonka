{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.AttachInternetGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Attaches an Internet gateway to a VPC, enabling connectivity between the
-- Internet and the VPC. For more information about your VPC and Internet
-- gateway, see the Amazon Virtual Private Cloud User Guide. Example This
-- example attaches the Internet gateway with the ID igw-eaad4883 to the VPC
-- with the ID vpc-11ad4878.
-- https://ec2.amazonaws.com/?Action=AttachInternetGateway
-- &amp;InternetGatewayId=igw-eaad4883 &amp;VpcId=vpc-11ad4878 &amp;AUTHPARAMS
-- &lt;AttachInternetGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/AttachInternetGatewayResponse&gt;.
module Network.AWS.EC2.V2014_05_01.AttachInternetGateway where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Region, Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data AttachInternetGateway = AttachInternetGateway
    { _aigrVpcId :: Text
      -- ^ The ID of the VPC.
    , _aigrInternetGatewayId :: Text
      -- ^ The ID of the Internet gateway.
    , _aigrDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery AttachInternetGateway where
    toQuery = genericToQuery def

instance AWSRequest AttachInternetGateway where
    type Sv AttachInternetGateway = EC2
    type Rs AttachInternetGateway = AttachInternetGatewayResponse

    request = post "AttachInternetGateway"
    response _ _ = return (Right AttachInternetGatewayResponse)

data AttachInternetGatewayResponse = AttachInternetGatewayResponse
    deriving (Eq, Show, Generic)
