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

-- Module      : Network.AWS.EC2.DeleteCustomerGateway
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified customer gateway. You must delete the VPN connection
-- before you can delete the customer gateway.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DeleteCustomerGateway.html>
module Network.AWS.EC2.DeleteCustomerGateway
    (
    -- * Request
      DeleteCustomerGateway
    -- ** Request constructor
    , deleteCustomerGateway
    -- ** Request lenses
    , dcg1CustomerGatewayId
    , dcg1DryRun

    -- * Response
    , DeleteCustomerGatewayResponse
    -- ** Response constructor
    , deleteCustomerGatewayResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DeleteCustomerGateway = DeleteCustomerGateway
    { _dcg1CustomerGatewayId :: Text
    , _dcg1DryRun            :: Maybe Bool
    } deriving (Eq, Ord, Show)

-- | 'DeleteCustomerGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcg1CustomerGatewayId' @::@ 'Text'
--
-- * 'dcg1DryRun' @::@ 'Maybe' 'Bool'
--
deleteCustomerGateway :: Text -- ^ 'dcg1CustomerGatewayId'
                      -> DeleteCustomerGateway
deleteCustomerGateway p1 = DeleteCustomerGateway
    { _dcg1CustomerGatewayId = p1
    , _dcg1DryRun            = Nothing
    }

-- | The ID of the customer gateway.
dcg1CustomerGatewayId :: Lens' DeleteCustomerGateway Text
dcg1CustomerGatewayId =
    lens _dcg1CustomerGatewayId (\s a -> s { _dcg1CustomerGatewayId = a })

dcg1DryRun :: Lens' DeleteCustomerGateway (Maybe Bool)
dcg1DryRun = lens _dcg1DryRun (\s a -> s { _dcg1DryRun = a })

data DeleteCustomerGatewayResponse = DeleteCustomerGatewayResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteCustomerGatewayResponse' constructor.
deleteCustomerGatewayResponse :: DeleteCustomerGatewayResponse
deleteCustomerGatewayResponse = DeleteCustomerGatewayResponse

instance ToPath DeleteCustomerGateway where
    toPath = const "/"

instance ToQuery DeleteCustomerGateway where
    toQuery DeleteCustomerGateway{..} = mconcat
        [ "CustomerGatewayId" =? _dcg1CustomerGatewayId
        , "dryRun"            =? _dcg1DryRun
        ]

instance ToHeaders DeleteCustomerGateway

instance AWSRequest DeleteCustomerGateway where
    type Sv DeleteCustomerGateway = EC2
    type Rs DeleteCustomerGateway = DeleteCustomerGatewayResponse

    request  = post "DeleteCustomerGateway"
    response = nullResponse DeleteCustomerGatewayResponse
