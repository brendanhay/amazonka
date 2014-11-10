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

-- Module      : Network.AWS.SNS.DeleteEndpoint
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the endpoint from Amazon SNS. This action is idempotent. For more
-- information, see Using Amazon SNS Mobile Push Notifications.
module Network.AWS.SNS.DeleteEndpoint
    (
    -- * Request
      DeleteEndpointInput
    -- ** Request constructor
    , deleteEndpoint
    -- ** Request lenses
    , deiEndpointArn

    -- * Response
    , DeleteEndpointResponse
    -- ** Response constructor
    , deleteEndpointResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

newtype DeleteEndpointInput = DeleteEndpointInput
    { _deiEndpointArn :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteEndpointInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deiEndpointArn' @::@ 'Text'
--
deleteEndpoint :: Text -- ^ 'deiEndpointArn'
               -> DeleteEndpointInput
deleteEndpoint p1 = DeleteEndpointInput
    { _deiEndpointArn = p1
    }

-- | EndpointArn of endpoint to delete.
deiEndpointArn :: Lens' DeleteEndpointInput Text
deiEndpointArn = lens _deiEndpointArn (\s a -> s { _deiEndpointArn = a })

instance ToPath DeleteEndpointInput where
    toPath = const "/"

instance ToQuery DeleteEndpointInput

data DeleteEndpointResponse = DeleteEndpointResponse

-- | 'DeleteEndpointResponse' constructor.
deleteEndpointResponse :: DeleteEndpointResponse
deleteEndpointResponse = DeleteEndpointResponse

instance AWSRequest DeleteEndpointInput where
    type Sv DeleteEndpointInput = SNS
    type Rs DeleteEndpointInput = DeleteEndpointResponse

    request  = post "DeleteEndpoint"
    response = const (nullaryResponse DeleteEndpointResponse)
