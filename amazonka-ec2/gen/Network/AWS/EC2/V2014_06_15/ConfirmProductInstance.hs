{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.EC2.V2014_06_15.ConfirmProductInstance
    (
    -- * Request
      ConfirmProductInstance
    -- ** Default constructor
    , confirmProductInstance
    -- ** Accessors and lenses
    , _cpirProductCode
    , cpirProductCode
    , _cpirInstanceId
    , cpirInstanceId

    -- * Response
    , ConfirmProductInstanceResponse
    -- ** Accessors and lenses
    , _cpisOwnerId
    , cpisOwnerId
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'ConfirmProductInstance' request.
confirmProductInstance :: Text -- ^ 'cpirProductCode'
                       -> Text -- ^ 'cpirInstanceId'
                       -> ConfirmProductInstance
confirmProductInstance p1 p2 = ConfirmProductInstance
    { _cpirProductCode = p1
    , _cpirInstanceId = p2
    }

data ConfirmProductInstance = ConfirmProductInstance

makeSiglessLenses ''ConfirmProductInstance

instance ToQuery ConfirmProductInstance where
    toQuery = genericQuery def

data ConfirmProductInstanceResponse = ConfirmProductInstanceResponse
    { _cpisOwnerId :: Maybe Text
      -- ^ The AWS account ID of the instance owner. This is only present if
      -- the product code is attached to the instance.
    } deriving (Show, Generic)

makeSiglessLenses ''ConfirmProductInstanceResponse

instance FromXML ConfirmProductInstanceResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest ConfirmProductInstance where
    type Sv ConfirmProductInstance = EC2
    type Rs ConfirmProductInstance = ConfirmProductInstanceResponse

    request = post "ConfirmProductInstance"
    response _ = xmlResponse

-- | The product code. This must be an Amazon DevPay product code that you own.
cpirProductCode :: Lens' ConfirmProductInstance (Text)

-- | The ID of the instance.
cpirInstanceId :: Lens' ConfirmProductInstance (Text)

-- | The AWS account ID of the instance owner. This is only present if the
-- product code is attached to the instance.
cpisOwnerId :: Lens' ConfirmProductInstanceResponse (Maybe Text)
