{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.DeleteCustomerGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified customer gateway. You must delete the VPN connection
-- before you can delete the customer gateway. Example This example deletes
-- the specified customer gateway.
-- https://ec2.amazonaws.com/?Action=DeleteCustomerGateway
-- &amp;CustomerGatewayId=cgw-b4dc3961 &amp;AUTHPARAMS
-- &lt;DeleteCustomerGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteCustomerGatewayResponse&gt;.
module Network.AWS.EC2.V2014_05_01.DeleteCustomerGateway where

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
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.Query
import           Network.AWS.EC2.V2014_05_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DeleteCustomerGateway = DeleteCustomerGateway
    { _dcgtCustomerGatewayId :: Text
      -- ^ The ID of the customer gateway.
    , _dcgtDryRun :: Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery DeleteCustomerGateway where
    toQuery = genericToQuery def

instance AWSRequest DeleteCustomerGateway where
    type Sv DeleteCustomerGateway = EC2
    type Rs DeleteCustomerGateway = DeleteCustomerGatewayResponse

    request = post "DeleteCustomerGateway"

    response _ _ = return (Right DeleteCustomerGatewayResponse)

data DeleteCustomerGatewayResponse = DeleteCustomerGatewayResponse
    deriving (Eq, Show, Generic)
