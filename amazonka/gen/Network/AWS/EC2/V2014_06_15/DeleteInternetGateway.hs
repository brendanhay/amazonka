{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteInternetGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified Internet gateway. You must detach the Internet
-- gateway from the VPC before you can delete it. Example This example deletes
-- the specified Internet gateway.
-- https://ec2.amazonaws.com/?Action=DeleteInternetGateway
-- &amp;InternetGatewayId=igw-eaad4883 &amp;AUTHPARAMS
-- &lt;DeleteInternetGatewayResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteInternetGatewayResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteInternetGateway where

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

-- | Minimum specification for a 'DeleteInternetGateway' request.
deleteInternetGateway :: Text -- ^ '_diguInternetGatewayId'
                      -> DeleteInternetGateway
deleteInternetGateway p1 = DeleteInternetGateway
    { _diguInternetGatewayId = p1
    , _diguDryRun = Nothing
    }

data DeleteInternetGateway = DeleteInternetGateway
    { _diguInternetGatewayId :: Text
      -- ^ The ID of the Internet gateway.
    , _diguDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery DeleteInternetGateway where
    toQuery = genericToQuery def

instance AWSRequest DeleteInternetGateway where
    type Sv DeleteInternetGateway = EC2
    type Rs DeleteInternetGateway = DeleteInternetGatewayResponse

    request = post "DeleteInternetGateway"
    response _ _ = return (Right DeleteInternetGatewayResponse)

data DeleteInternetGatewayResponse = DeleteInternetGatewayResponse
    deriving (Eq, Show, Generic)
