{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.DeleteVpcPeeringConnection
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
-- connection in the pending-acceptance state.
module Network.AWS.EC2.DeleteVpcPeeringConnection
    (
    -- * Request
      DeleteVpcPeeringConnection
    -- ** Request constructor
    , deleteVpcPeeringConnection
    -- ** Request lenses
    , dvpc1DryRun
    , dvpc1VpcPeeringConnectionId

    -- * Response
    , DeleteVpcPeeringConnectionResult
    -- ** Response constructor
    , deleteVpcPeeringConnectionResponse
    -- ** Response lenses
    , dvpcrReturn
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DeleteVpcPeeringConnection = DeleteVpcPeeringConnection
    { _dvpc1DryRun                 :: Maybe Bool
    , _dvpc1VpcPeeringConnectionId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteVpcPeeringConnection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvpc1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dvpc1VpcPeeringConnectionId' @::@ 'Text'
--
deleteVpcPeeringConnection :: Text -- ^ 'dvpc1VpcPeeringConnectionId'
                           -> DeleteVpcPeeringConnection
deleteVpcPeeringConnection p1 = DeleteVpcPeeringConnection
    { _dvpc1VpcPeeringConnectionId = p1
    , _dvpc1DryRun                 = Nothing
    }

dvpc1DryRun :: Lens' DeleteVpcPeeringConnection (Maybe Bool)
dvpc1DryRun = lens _dvpc1DryRun (\s a -> s { _dvpc1DryRun = a })

-- | The ID of the VPC peering connection.
dvpc1VpcPeeringConnectionId :: Lens' DeleteVpcPeeringConnection Text
dvpc1VpcPeeringConnectionId =
    lens _dvpc1VpcPeeringConnectionId
        (\s a -> s { _dvpc1VpcPeeringConnectionId = a })

instance ToQuery DeleteVpcPeeringConnection

instance ToPath DeleteVpcPeeringConnection where
    toPath = const "/"

newtype DeleteVpcPeeringConnectionResult = DeleteVpcPeeringConnectionResult
    { _dvpcrReturn :: Maybe Bool
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteVpcPeeringConnectionResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvpcrReturn' @::@ 'Maybe' 'Bool'
--
deleteVpcPeeringConnectionResponse :: DeleteVpcPeeringConnectionResult
deleteVpcPeeringConnectionResponse = DeleteVpcPeeringConnectionResult
    { _dvpcrReturn = Nothing
    }

-- | Returns true if the request succeeds; otherwise, it returns an error.
dvpcrReturn :: Lens' DeleteVpcPeeringConnectionResult (Maybe Bool)
dvpcrReturn = lens _dvpcrReturn (\s a -> s { _dvpcrReturn = a })

instance FromXML DeleteVpcPeeringConnectionResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteVpcPeeringConnectionResult"

instance AWSRequest DeleteVpcPeeringConnection where
    type Sv DeleteVpcPeeringConnection = EC2
    type Rs DeleteVpcPeeringConnection = DeleteVpcPeeringConnectionResult

    request  = post "DeleteVpcPeeringConnection"
    response = xmlResponse $ \h x -> DeleteVpcPeeringConnectionResult
        <$> x %| "return"
