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
--
-- <http://docs.aws.amazon.com/sns/latest/api/API_DeletePlatformApplication.html>
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
import qualified GHC.Exts

newtype DeletePlatformApplication = DeletePlatformApplication
    { _dpaPlatformApplicationArn :: Text
    } deriving (Eq, Ord, Show, Monoid, IsString)

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

data DeletePlatformApplicationResponse = DeletePlatformApplicationResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeletePlatformApplicationResponse' constructor.
deletePlatformApplicationResponse :: DeletePlatformApplicationResponse
deletePlatformApplicationResponse = DeletePlatformApplicationResponse

instance ToPath DeletePlatformApplication where
    toPath = const "/"

instance ToQuery DeletePlatformApplication where
    toQuery DeletePlatformApplication{..} = mconcat
        [ "PlatformApplicationArn" =? _dpaPlatformApplicationArn
        ]

instance ToHeaders DeletePlatformApplication

instance AWSRequest DeletePlatformApplication where
    type Sv DeletePlatformApplication = SNS
    type Rs DeletePlatformApplication = DeletePlatformApplicationResponse

    request  = post "DeletePlatformApplication"
    response = nullResponse DeletePlatformApplicationResponse
