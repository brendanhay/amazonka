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

-- Module      : Network.AWS.Config.DeleteDeliveryChannel
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified delivery channel. The delivery channel cannot be
-- deleted if it is the only delivery channel and the configuration recorder
-- is still running. To delete the delivery channel, stop the running
-- configuration recorder using the StopConfigurationRecorder action.
module Network.AWS.Config.DeleteDeliveryChannel
    (
    -- * Request
      DeleteDeliveryChannel
    -- ** Request constructor
    , deleteDeliveryChannel
    -- ** Request lenses
    , ddcDeliveryChannelName

    -- * Response
    , DeleteDeliveryChannelResponse
    -- ** Response constructor
    , deleteDeliveryChannelResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Config.Types

newtype DeleteDeliveryChannel = DeleteDeliveryChannel
    { _ddcDeliveryChannelName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteDeliveryChannel' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ddcDeliveryChannelName' @::@ 'Text'
--
deleteDeliveryChannel :: Text -- ^ 'ddcDeliveryChannelName'
                      -> DeleteDeliveryChannel
deleteDeliveryChannel p1 = DeleteDeliveryChannel
    { _ddcDeliveryChannelName = p1
    }

-- | The name of the delivery channel to delete.
ddcDeliveryChannelName :: Lens' DeleteDeliveryChannel Text
ddcDeliveryChannelName =
    lens _ddcDeliveryChannelName (\s a -> s { _ddcDeliveryChannelName = a })

instance ToPath DeleteDeliveryChannel where
    toPath = const "/"

instance ToQuery DeleteDeliveryChannel where
    toQuery = const mempty

instance ToHeaders DeleteDeliveryChannel

instance ToBody DeleteDeliveryChannel where
    toBody = toBody . encode . _ddcDeliveryChannelName

data DeleteDeliveryChannelResponse = DeleteDeliveryChannelResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteDeliveryChannelResponse' constructor.
deleteDeliveryChannelResponse :: DeleteDeliveryChannelResponse
deleteDeliveryChannelResponse = DeleteDeliveryChannelResponse

instance AWSRequest DeleteDeliveryChannel where
    type Sv DeleteDeliveryChannel = Config
    type Rs DeleteDeliveryChannel = DeleteDeliveryChannelResponse

    request  = post
    response = nullaryResponse DeleteDeliveryChannelResponse
