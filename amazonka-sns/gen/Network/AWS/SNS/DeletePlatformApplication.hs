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
      DeletePlatformApplicationInput
    -- ** Request constructor
    , deletePlatformApplicationInput
    -- ** Request lenses
    , dpaiPlatformApplicationArn

    -- * Response
    , DeletePlatformApplicationResponse
    -- ** Response constructor
    , deletePlatformApplicationResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SNS.Types

newtype DeletePlatformApplicationInput = DeletePlatformApplicationInput
    { _dpaiPlatformApplicationArn :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeletePlatformApplicationInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dpaiPlatformApplicationArn' @::@ 'Text'
--
deletePlatformApplicationInput :: Text -- ^ 'dpaiPlatformApplicationArn'
                               -> DeletePlatformApplicationInput
deletePlatformApplicationInput p1 = DeletePlatformApplicationInput
    { _dpaiPlatformApplicationArn = p1
    }

-- | PlatformApplicationArn of platform application object to delete.
dpaiPlatformApplicationArn :: Lens' DeletePlatformApplicationInput Text
dpaiPlatformApplicationArn =
    lens _dpaiPlatformApplicationArn
        (\s a -> s { _dpaiPlatformApplicationArn = a })
instance ToQuery DeletePlatformApplicationInput

instance ToPath DeletePlatformApplicationInput where
    toPath = const "/"

data DeletePlatformApplicationResponse = DeletePlatformApplicationResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeletePlatformApplicationResponse' constructor.
deletePlatformApplicationResponse :: DeletePlatformApplicationResponse
deletePlatformApplicationResponse = DeletePlatformApplicationResponse
instance FromXML DeletePlatformApplicationResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeletePlatformApplicationResponse"

instance AWSRequest DeletePlatformApplicationInput where
    type Sv DeletePlatformApplicationInput = SNS
    type Rs DeletePlatformApplicationInput = DeletePlatformApplicationResponse

    request  = post "DeletePlatformApplication"
    response = nullaryResponse DeletePlatformApplicationResponse
