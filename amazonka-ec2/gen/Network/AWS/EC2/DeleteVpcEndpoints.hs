{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DeleteVpcEndpoints
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes one or more specified VPC endpoints. Deleting the endpoint also
-- deletes the endpoint routes in the route tables that were associated with the
-- endpoint.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteVpcEndpoints.html>
module Network.AWS.EC2.DeleteVpcEndpoints
    (
    -- * Request
      DeleteVpcEndpoints
    -- ** Request constructor
    , deleteVpcEndpoints
    -- ** Request lenses
    , dveDryRun
    , dveVpcEndpointIds

    -- * Response
    , DeleteVpcEndpointsResponse
    -- ** Response constructor
    , deleteVpcEndpointsResponse
    -- ** Response lenses
    , dverUnsuccessful
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteVpcEndpoints = DeleteVpcEndpoints
    { _dveDryRun         :: Maybe Bool
    , _dveVpcEndpointIds :: List "item" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DeleteVpcEndpoints' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dveDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dveVpcEndpointIds' @::@ ['Text']
--
deleteVpcEndpoints :: DeleteVpcEndpoints
deleteVpcEndpoints = DeleteVpcEndpoints
    { _dveDryRun         = Nothing
    , _dveVpcEndpointIds = mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
dveDryRun :: Lens' DeleteVpcEndpoints (Maybe Bool)
dveDryRun = lens _dveDryRun (\s a -> s { _dveDryRun = a })

-- | One or more endpoint IDs.
dveVpcEndpointIds :: Lens' DeleteVpcEndpoints [Text]
dveVpcEndpointIds =
    lens _dveVpcEndpointIds (\s a -> s { _dveVpcEndpointIds = a })
        . _List

newtype DeleteVpcEndpointsResponse = DeleteVpcEndpointsResponse
    { _dverUnsuccessful :: List "item" UnsuccessfulItem
    } deriving (Eq, Read, Show, Monoid, Semigroup)

-- | 'DeleteVpcEndpointsResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dverUnsuccessful' @::@ ['UnsuccessfulItem']
--
deleteVpcEndpointsResponse :: DeleteVpcEndpointsResponse
deleteVpcEndpointsResponse = DeleteVpcEndpointsResponse
    { _dverUnsuccessful = mempty
    }

-- | Information about the endpoints that were not successfully deleted.
dverUnsuccessful :: Lens' DeleteVpcEndpointsResponse [UnsuccessfulItem]
dverUnsuccessful = lens _dverUnsuccessful (\s a -> s { _dverUnsuccessful = a }) . _List

instance ToPath DeleteVpcEndpoints where
    toPath = const "/"

instance ToQuery DeleteVpcEndpoints where
    toQuery DeleteVpcEndpoints{..} = mconcat
        [ "DryRun"        =? _dveDryRun
        , "VpcEndpointId" `toQueryList` _dveVpcEndpointIds
        ]

instance ToHeaders DeleteVpcEndpoints

instance AWSRequest DeleteVpcEndpoints where
    type Sv DeleteVpcEndpoints = EC2
    type Rs DeleteVpcEndpoints = DeleteVpcEndpointsResponse

    request  = post "DeleteVpcEndpoints"
    response = xmlResponse

instance FromXML DeleteVpcEndpointsResponse where
    parseXML x = DeleteVpcEndpointsResponse
        <$> x .@? "unsuccessful" .!@ mempty
