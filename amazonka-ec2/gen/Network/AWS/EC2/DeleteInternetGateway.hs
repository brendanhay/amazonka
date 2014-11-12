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
-- gateway from the VPC before you can delete it.
module Network.AWS.EC2.DeleteInternetGateway
    (
    -- * Request
      DeleteInternetGateway
    -- ** Request constructor
    , deleteInternetGateway
    -- ** Request lenses
    , dig1DryRun
    , dig1InternetGatewayId

    -- * Response
    , DeleteInternetGatewayResponse
    -- ** Response constructor
    , deleteInternetGatewayResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DeleteInternetGateway = DeleteInternetGateway
    { _dig1DryRun            :: Maybe Bool
    , _dig1InternetGatewayId :: Text
    } (Eq, Ord, Show, Generic)

-- | 'DeleteInternetGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dig1DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dig1InternetGatewayId' @::@ 'Text'
--
deleteInternetGateway :: Text -- ^ 'dig1InternetGatewayId'
                      -> DeleteInternetGateway
deleteInternetGateway p1 = DeleteInternetGateway
    { _dig1InternetGatewayId = p1
    , _dig1DryRun            = Nothing
    }

dig1DryRun :: Lens' DeleteInternetGateway (Maybe Bool)
dig1DryRun = lens _dig1DryRun (\s a -> s { _dig1DryRun = a })

-- | The ID of the Internet gateway.
dig1InternetGatewayId :: Lens' DeleteInternetGateway Text
dig1InternetGatewayId =
    lens _dig1InternetGatewayId (\s a -> s { _dig1InternetGatewayId = a })
instance ToQuery DeleteInternetGateway

instance ToPath DeleteInternetGateway where
    toPath = const "/"

data DeleteInternetGatewayResponse = DeleteInternetGatewayResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteInternetGatewayResponse' constructor.
deleteInternetGatewayResponse :: DeleteInternetGatewayResponse
deleteInternetGatewayResponse = DeleteInternetGatewayResponse

instance FromXML DeleteInternetGatewayResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteInternetGatewayResponse"

instance AWSRequest DeleteInternetGateway where
    type Sv DeleteInternetGateway = EC2
    type Rs DeleteInternetGateway = DeleteInternetGatewayResponse

    request  = post "DeleteInternetGateway"
    response = nullaryResponse DeleteInternetGatewayResponse
