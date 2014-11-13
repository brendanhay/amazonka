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

-- Module      : Network.AWS.SNS.DeletePlatformApplication
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes a platform application object for one of the supported push
-- notification services, such as APNS and GCM. For more information, see
-- Using Amazon SNS Mobile Push Notifications.
module Network.AWS.SNS.DeletePlatformApplication
    (
    -- * Request
      DeletePlatformApplication
    -- ** Request constructor
    , deletePlatformApplication
    -- ** Request lenses
    , dpaPlatformApplicationArn

    -- * Response
    , DeletePlatformApplicationResponse
    -- ** Response constructor
    , deletePlatformApplicationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

newtype DeletePlatformApplication = DeletePlatformApplication
    { _dpaPlatformApplicationArn :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeletePlatformApplication' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpaPlatformApplicationArn' @::@ 'Text'
--
deletePlatformApplication :: Text -- ^ 'dpaPlatformApplicationArn'
                          -> DeletePlatformApplication
deletePlatformApplication p1 = DeletePlatformApplication
    { _dpaPlatformApplicationArn = p1
    }

-- | PlatformApplicationArn of platform application object to delete.
dpaPlatformApplicationArn :: Lens' DeletePlatformApplication Text
dpaPlatformApplicationArn =
    lens _dpaPlatformApplicationArn
        (\s a -> s { _dpaPlatformApplicationArn = a })

instance ToQuery DeletePlatformApplication

instance ToPath DeletePlatformApplication where
    toPath = const "/"

data DeletePlatformApplicationResponse = DeletePlatformApplicationResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeletePlatformApplicationResponse' constructor.
deletePlatformApplicationResponse :: DeletePlatformApplicationResponse
deletePlatformApplicationResponse = DeletePlatformApplicationResponse

instance FromXML DeletePlatformApplicationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeletePlatformApplicationResponse"

instance AWSRequest DeletePlatformApplication where
    type Sv DeletePlatformApplication = SNS
    type Rs DeletePlatformApplication = DeletePlatformApplicationResponse

    request  = post "DeletePlatformApplication"
    response = nullaryResponse DeletePlatformApplicationResponse
