{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
      DeleteEndpoint
    -- ** Request constructor
    , deleteEndpoint
    -- ** Request lenses
    , deEndpointArn

    -- * Response
    , DeleteEndpointResponse
    -- ** Response constructor
    , deleteEndpointResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types
import qualified GHC.Exts

newtype DeleteEndpoint = DeleteEndpoint
    { _deEndpointArn :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteEndpoint' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'deEndpointArn' @::@ 'Text'
--
deleteEndpoint :: Text -- ^ 'deEndpointArn'
               -> DeleteEndpoint
deleteEndpoint p1 = DeleteEndpoint
    { _deEndpointArn = p1
    }

-- | EndpointArn of endpoint to delete.
deEndpointArn :: Lens' DeleteEndpoint Text
deEndpointArn = lens _deEndpointArn (\s a -> s { _deEndpointArn = a })

data DeleteEndpointResponse = DeleteEndpointResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteEndpointResponse' constructor.
deleteEndpointResponse :: DeleteEndpointResponse
deleteEndpointResponse = DeleteEndpointResponse

instance AWSRequest DeleteEndpoint where
    type Sv DeleteEndpoint = SNS
    type Rs DeleteEndpoint = DeleteEndpointResponse

    request  = post "DeleteEndpoint"
    response = nullResponse DeleteEndpointResponse

instance ToPath DeleteEndpoint where
    toPath = const "/"

instance ToHeaders DeleteEndpoint

instance ToQuery DeleteEndpoint
