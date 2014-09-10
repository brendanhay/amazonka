{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a VPC peering connection. Either the owner of the requester VPC or
-- the owner of the peer VPC can delete the VPC peering connection if it's in
-- the active state. The owner of the requester VPC can delete a VPC peering
-- connection in the pending-acceptance state. Example This example deletes
-- the specified VPC peering connection.
-- https://ec2.amazonaws.com/?Action=DeleteVpcPeeringConnection
-- &amp;vpcPeeringConnectionId=pcx-1a2b3c4d &amp;AUTHPARAMS
-- &lt;DeleteVpcPeeringConnectionResponse
-- xmlns="http://ec2.amazonaws.com/doc/2014-06-15/"&gt;
-- &lt;requestId&gt;7a62c49f-347e-4fc4-9331-6e8eEXAMPLE&lt;/requestId&gt;
-- &lt;return&gt;true&lt;/return&gt;
-- &lt;/DeleteVpcPeeringConnectionResponse&gt;.
module Network.AWS.EC2
    (
    -- * Request
      DeleteVpcPeeringConnection
    -- ** Request constructor
    , mkDeleteVpcPeeringConnection
    -- ** Request lenses
    , dvpcVpcPeeringConnectionId

    -- * Response
    , DeleteVpcPeeringConnectionResponse
    -- ** Response constructor
    , mkDeleteVpcPeeringConnectionResponse
    -- ** Response lenses
    , dvpcrReturn
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import Network.AWS.Prelude

newtype DeleteVpcPeeringConnection = DeleteVpcPeeringConnection
    { _dvpcVpcPeeringConnectionId :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteVpcPeeringConnection' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @VpcPeeringConnectionId ::@ @Text@
--
mkDeleteVpcPeeringConnection :: Text -- ^ 'dvpcVpcPeeringConnectionId'
                             -> DeleteVpcPeeringConnection
mkDeleteVpcPeeringConnection p1 = DeleteVpcPeeringConnection
    { _dvpcVpcPeeringConnectionId = p1
    }

-- | The ID of the VPC peering connection.
dvpcVpcPeeringConnectionId :: Lens' DeleteVpcPeeringConnection Text
dvpcVpcPeeringConnectionId =
    lens _dvpcVpcPeeringConnectionId
         (\s a -> s { _dvpcVpcPeeringConnectionId = a })

instance ToQuery DeleteVpcPeeringConnection where
    toQuery = genericQuery def

newtype DeleteVpcPeeringConnectionResponse = DeleteVpcPeeringConnectionResponse
    { _dvpcrReturn :: Maybe Bool
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteVpcPeeringConnectionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Return ::@ @Maybe Bool@
--
mkDeleteVpcPeeringConnectionResponse :: DeleteVpcPeeringConnectionResponse
mkDeleteVpcPeeringConnectionResponse = DeleteVpcPeeringConnectionResponse
    { _dvpcrReturn = Nothing
    }

-- | Returns true if the request succeeds; otherwise, it returns an error.
dvpcrReturn :: Lens' DeleteVpcPeeringConnectionResponse (Maybe Bool)
dvpcrReturn = lens _dvpcrReturn (\s a -> s { _dvpcrReturn = a })

instance FromXML DeleteVpcPeeringConnectionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DeleteVpcPeeringConnection where
    type Sv DeleteVpcPeeringConnection = EC2
    type Rs DeleteVpcPeeringConnection = DeleteVpcPeeringConnectionResponse

    request = post "DeleteVpcPeeringConnection"
    response _ = xmlResponse
