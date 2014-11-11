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

-- Module      : Network.AWS.Redshift.DeleteEventSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes an Amazon Redshift event notification subscription.
module Network.AWS.Redshift.DeleteEventSubscription
    (
    -- * Request
      DeleteEventSubscriptionMessage
    -- ** Request constructor
    , deleteEventSubscriptionMessage
    -- ** Request lenses
    , desm1SubscriptionName

    -- * Response
    , DeleteEventSubscriptionResponse
    -- ** Response constructor
    , deleteEventSubscriptionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.Redshift.Types

newtype DeleteEventSubscriptionMessage = DeleteEventSubscriptionMessage
    { _desm1SubscriptionName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

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

-- | The name of the Amazon Redshift event notification subscription to be
-- deleted.
desm1SubscriptionName :: Lens' DeleteEventSubscriptionMessage Text
desm1SubscriptionName =
    lens _desm1SubscriptionName (\s a -> s { _desm1SubscriptionName = a })
instance ToQuery DeleteEventSubscriptionMessage

instance ToPath DeleteEventSubscriptionMessage where
    toPath = const "/"

data DeleteEventSubscriptionResponse = DeleteEventSubscriptionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DeleteEventSubscriptionResponse' constructor.
deleteEventSubscriptionResponse :: DeleteEventSubscriptionResponse
deleteEventSubscriptionResponse = DeleteEventSubscriptionResponse
instance FromXML DeleteEventSubscriptionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DeleteEventSubscriptionResponse"

instance AWSRequest DeleteEventSubscriptionMessage where
    type Sv DeleteEventSubscriptionMessage = Redshift
    type Rs DeleteEventSubscriptionMessage = DeleteEventSubscriptionResponse

    request  = post "DeleteEventSubscription"
    response = nullaryResponse DeleteEventSubscriptionResponse
