{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;59dbff89-35bd-4eac-99ed-be587EXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt; &lt;/DeleteInternetGatewayResponse&gt;.
module Network.AWS.EC2.V2014_06_15.DeleteInternetGateway
    (
    -- * Request
      DeleteInternetGateway
    -- ** Request constructor
    , mkDeleteInternetGatewayRequest
    -- ** Request lenses
    , digrInternetGatewayId

    -- * Response
    , DeleteInternetGatewayResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteInternetGateway' request.
mkDeleteInternetGatewayRequest :: Text -- ^ 'digrInternetGatewayId'
                               -> DeleteInternetGateway
mkDeleteInternetGatewayRequest p1 = DeleteInternetGateway
    { _digrInternetGatewayId = p1
    }
{-# INLINE mkDeleteInternetGatewayRequest #-}

newtype DeleteInternetGateway = DeleteInternetGateway
    { _digrInternetGatewayId :: Text
      -- ^ The ID of the Internet gateway.
    } deriving (Show, Generic)

-- | The ID of the Internet gateway.
digrInternetGatewayId :: Lens' DeleteInternetGateway (Text)
digrInternetGatewayId = lens _digrInternetGatewayId (\s a -> s { _digrInternetGatewayId = a })
{-# INLINE digrInternetGatewayId #-}

instance ToQuery DeleteInternetGateway where
    toQuery = genericQuery def

data DeleteInternetGatewayResponse = DeleteInternetGatewayResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteInternetGateway where
    type Sv DeleteInternetGateway = EC2
    type Rs DeleteInternetGateway = DeleteInternetGatewayResponse

    request = post "DeleteInternetGateway"
    response _ = nullaryResponse DeleteInternetGatewayResponse
