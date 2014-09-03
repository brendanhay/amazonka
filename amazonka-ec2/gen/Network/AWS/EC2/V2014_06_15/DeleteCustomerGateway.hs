{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.DeleteCustomerGateway
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
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteCustomerGatewayResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteCustomerGateway
    (
    -- * Request
      DeleteCustomerGateway
    -- ** Default constructor
    , deleteCustomerGateway
    -- ** Accessors and lenses
    , _dcgrCustomerGatewayId
    , dcgrCustomerGatewayId

    -- * Response
    , DeleteCustomerGatewayResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteCustomerGateway' request.
deleteCustomerGateway :: Text -- ^ 'dcgrCustomerGatewayId'
                      -> DeleteCustomerGateway
deleteCustomerGateway p1 = DeleteCustomerGateway
    { _dcgrCustomerGatewayId = p1
    }

data DeleteCustomerGateway = DeleteCustomerGateway

makeSiglessLenses ''DeleteCustomerGateway

instance ToQuery DeleteCustomerGateway where
    toQuery = genericQuery def

data DeleteCustomerGatewayResponse = DeleteCustomerGatewayResponse
    deriving (Eq, Show, Generic)

makeSiglessLenses ''DeleteCustomerGatewayResponse

instance AWSRequest DeleteCustomerGateway where
    type Sv DeleteCustomerGateway = EC2
    type Rs DeleteCustomerGateway = DeleteCustomerGatewayResponse

    request = post "DeleteCustomerGateway"
    response _ = nullaryResponse DeleteCustomerGatewayResponse

-- | The ID of the customer gateway.
dcgrCustomerGatewayId :: Lens' DeleteCustomerGateway (Text)
