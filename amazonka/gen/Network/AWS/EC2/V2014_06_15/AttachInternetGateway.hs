{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.V2014_06_15.AttachInternetGateway
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
module Network.AWS.EC2.V2014_06_15.AttachInternetGateway where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AttachInternetGateway' request.
attachInternetGateway :: Text -- ^ '_aigrVpcId'
                      -> Text -- ^ '_aigrInternetGatewayId'
                      -> AttachInternetGateway
attachInternetGateway p1 p2 = AttachInternetGateway
    { _aigrVpcId = p1
    , _aigrInternetGatewayId = p2
    , _aigrDryRun = Nothing
    }

data AttachInternetGateway = AttachInternetGateway
    { _aigrVpcId :: Text
      -- ^ The ID of the VPC.
    , _aigrInternetGatewayId :: Text
      -- ^ The ID of the Internet gateway.
    , _aigrDryRun :: Maybe Bool
      -- ^ 
    } deriving (Show, Generic)

makeLenses ''AttachInternetGateway

instance ToQuery AttachInternetGateway where
    toQuery = genericToQuery def

data AttachInternetGatewayResponse = AttachInternetGatewayResponse
    deriving (Eq, Show, Generic)

makeLenses ''AttachInternetGatewayResponse

instance AWSRequest AttachInternetGateway where
    type Sv AttachInternetGateway = EC2
    type Rs AttachInternetGateway = AttachInternetGatewayResponse

    request = post "AttachInternetGateway"
    response _ = nullaryResponse AttachInternetGatewayResponse
