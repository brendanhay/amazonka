{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteInternetGateway
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
module Network.AWS.EC2.DeleteInternetGateway
    (
    -- * Request
      DeleteInternetGateway
    -- ** Request constructor
    , deleteInternetGateway
    -- ** Request lenses
    , digInternetGatewayId

    -- * Response
    , DeleteInternetGatewayResponse
    -- ** Response constructor
    , deleteInternetGatewayResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype DeleteInternetGateway = DeleteInternetGateway
    { _digInternetGatewayId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteInternetGateway' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @InternetGatewayId ::@ @Text@
--
deleteInternetGateway :: Text -- ^ 'digInternetGatewayId'
                      -> DeleteInternetGateway
deleteInternetGateway p1 = DeleteInternetGateway
    { _digInternetGatewayId = p1
    }

-- | The ID of the Internet gateway.
digInternetGatewayId :: Lens' DeleteInternetGateway Text
digInternetGatewayId =
    lens _digInternetGatewayId (\s a -> s { _digInternetGatewayId = a })

instance ToQuery DeleteInternetGateway where
    toQuery = genericQuery def

data DeleteInternetGatewayResponse = DeleteInternetGatewayResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteInternetGatewayResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
deleteInternetGatewayResponse :: DeleteInternetGatewayResponse
deleteInternetGatewayResponse = DeleteInternetGatewayResponse

instance AWSRequest DeleteInternetGateway where
    type Sv DeleteInternetGateway = EC2
    type Rs DeleteInternetGateway = DeleteInternetGatewayResponse

    request = post "DeleteInternetGateway"
    response _ = nullaryResponse DeleteInternetGatewayResponse
