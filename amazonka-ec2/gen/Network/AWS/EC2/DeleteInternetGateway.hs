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
    , dig2DryRun
    , dig2InternetGatewayId

    -- * Response
    , DeleteInternetGatewayResponse
    -- ** Response constructor
    , deleteInternetGatewayResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data DeleteInternetGateway = DeleteInternetGateway
    { _dig2DryRun            :: Maybe Bool
    , _dig2InternetGatewayId :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DeleteInternetGateway' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dig2DryRun' @::@ 'Maybe' 'Bool'
--
-- * 'dig2InternetGatewayId' @::@ 'Text'
--
deleteInternetGateway :: Text -- ^ 'dig2InternetGatewayId'
                      -> DeleteInternetGateway
deleteInternetGateway p1 = DeleteInternetGateway
    { _dig2InternetGatewayId = p1
    , _dig2DryRun            = Nothing
    }

dig2DryRun :: Lens' DeleteInternetGateway (Maybe Bool)
dig2DryRun = lens _dig2DryRun (\s a -> s { _dig2DryRun = a })

-- | The ID of the Internet gateway.
dig2InternetGatewayId :: Lens' DeleteInternetGateway Text
dig2InternetGatewayId =
    lens _dig2InternetGatewayId (\s a -> s { _dig2InternetGatewayId = a })

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
