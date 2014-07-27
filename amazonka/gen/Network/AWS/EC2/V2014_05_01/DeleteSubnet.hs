{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_05_01.DeleteSubnet
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified subnet. You must terminate all running instances in
-- the subnet before you can delete the subnet. Example This example deletes
-- the specified subnet. https://ec2.amazonaws.com/?Action=DeleteSubnet
-- &amp;SubnetId=subnet-9d4a7b6c &amp;AUTHPARAMS &lt;DeleteSubnetResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteSubnetResponse&gt;.
module Network.AWS.EC2.V2014_05_01.DeleteSubnet where

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

data DeleteSubnet = DeleteSubnet
    { _dswSubnetId :: Text
      -- ^ The ID of the subnet.
    , _dswDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery DeleteSubnet where
    toQuery = genericToQuery def

instance AWSRequest DeleteSubnet where
    type Sv DeleteSubnet = EC2
    type Rs DeleteSubnet = DeleteSubnetResponse

    request = post "DeleteSubnet"
    response _ _ = return (Right DeleteSubnetResponse)

data DeleteSubnetResponse = DeleteSubnetResponse
    deriving (Eq, Show, Generic)
