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

-- Module      : Network.AWS.RDS.DeleteEventSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes an RDS event notification subscription.
module Network.AWS.RDS.DeleteEventSubscription
    (
    -- * Request
      DeleteEventSubscriptionMessage
    -- ** Request constructor
    , deleteEventSubscriptionMessage
    -- ** Request lenses
    , desm1SubscriptionName

    -- * Response
    , DeleteEventSubscriptionResult
    -- ** Response constructor
    , deleteEventSubscriptionResult
    -- ** Response lenses
    , desrEventSubscription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types

newtype DeleteEventSubscriptionMessage = DeleteEventSubscriptionMessage
    { _desm1SubscriptionName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteEventSubscriptionMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desm1SubscriptionName' @::@ 'Text'
--
deleteEventSubscriptionMessage :: Text -- ^ 'desm1SubscriptionName'
                               -> DeleteEventSubscriptionMessage
deleteEventSubscriptionMessage p1 = DeleteEventSubscriptionMessage
    { _desm1SubscriptionName = p1
    }

-- | The name of the RDS event notification subscription you want to delete.
desm1SubscriptionName :: Lens' DeleteEventSubscriptionMessage Text
desm1SubscriptionName =
    lens _desm1SubscriptionName (\s a -> s { _desm1SubscriptionName = a })

instance ToQuery DeleteEventSubscriptionMessage

instance ToPath DeleteEventSubscriptionMessage where
    toPath = const "/"

newtype DeleteEventSubscriptionResult = DeleteEventSubscriptionResult
    { _desrEventSubscription :: Maybe EventSubscription
    } deriving (Eq, Show, Generic)

-- | 'DeleteEventSubscriptionResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desrEventSubscription' @::@ 'Maybe' 'EventSubscription'
--
deleteEventSubscriptionResult :: DeleteEventSubscriptionResult
deleteEventSubscriptionResult = DeleteEventSubscriptionResult
    { _desrEventSubscription = Nothing
    }

desrEventSubscription :: Lens' DeleteEventSubscriptionResult (Maybe EventSubscription)
desrEventSubscription =
    lens _desrEventSubscription (\s a -> s { _desrEventSubscription = a })

instance FromXML DeleteEventSubscriptionResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteEventSubscriptionResult"

instance AWSRequest DeleteEventSubscriptionMessage where
    type Sv DeleteEventSubscriptionMessage = RDS
    type Rs DeleteEventSubscriptionMessage = DeleteEventSubscriptionResult

    request  = post "DeleteEventSubscription"
    response = xmlResponse $ \h x -> DeleteEventSubscriptionResult
        <$> x %| "EventSubscription"
