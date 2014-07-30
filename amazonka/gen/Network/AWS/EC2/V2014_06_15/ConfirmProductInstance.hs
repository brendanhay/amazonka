{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ConfirmProductInstance
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Determines whether a product code is associated with an instance. This
-- action can only be used by the owner of the product code. It is useful when
-- a product code owner needs to verify whether another user's instance is
-- eligible for support. Example This example determines whether the specified
-- product code is associated with the specified instance.
-- https://ec2.amazonaws.com/?Action=ConfirmProductInstance
-- &amp;ProductCode=774F4FF8 &amp;InstanceId=i-10a64379 &amp;AUTHPARAMS
-- &lt;ConfirmProductInstanceResponse
-- xmlns="http://ec2.amazonaws.com/doc/2013-10-01/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;ownerId&gt;111122223333&lt;/ownerId&gt;
-- &lt;/ConfirmProductInstanceResponse&gt;.
module Network.AWS.EC2.V2014_06_15.ConfirmProductInstance where

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

-- | Minimum specification for a 'ConfirmProductInstance' request.
confirmProductInstance :: Text -- ^ '_cpirInstanceId'
                       -> Text -- ^ '_cpirProductCode'
                       -> ConfirmProductInstance
confirmProductInstance p1 p2 = ConfirmProductInstance
    { _cpirInstanceId = p1
    , _cpirProductCode = p2
    , _cpirDryRun = Nothing
    }

data ConfirmProductInstance = ConfirmProductInstance
    { _cpirInstanceId :: Text
      -- ^ The ID of the instance.
    , _cpirProductCode :: Text
      -- ^ The product code. This must be an Amazon DevPay product code that
      -- you own.
    , _cpirDryRun :: Maybe Bool
      -- ^ 
    } deriving (Generic)

instance ToQuery ConfirmProductInstance where
    toQuery = genericToQuery def

instance AWSRequest ConfirmProductInstance where
    type Sv ConfirmProductInstance = EC2
    type Rs ConfirmProductInstance = ConfirmProductInstanceResponse

    request = post "ConfirmProductInstance"
    response _ = xmlResponse

data ConfirmProductInstanceResponse = ConfirmProductInstanceResponse
    { _cpisOwnerId :: Maybe Text
      -- ^ The AWS account ID of the instance owner. This is only present if
      -- the product code is attached to the instance.
    } deriving (Generic)

instance FromXML ConfirmProductInstanceResponse where
    fromXMLOptions = xmlOptions
