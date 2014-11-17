{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes a source identifier from an existing RDS event notification
-- subscription.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_RemoveSourceIdentifierFromSubscription.html>
module Network.AWS.RDS.RemoveSourceIdentifierFromSubscription
    (
    -- * Request
      RemoveSourceIdentifierFromSubscription
    -- ** Request constructor
    , removeSourceIdentifierFromSubscription
    -- ** Request lenses
    , rsifsSourceIdentifier
    , rsifsSubscriptionName

    -- * Response
    , RemoveSourceIdentifierFromSubscriptionResponse
    -- ** Response constructor
    , removeSourceIdentifierFromSubscriptionResponse
    -- ** Response lenses
    , rsifsrEventSubscription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data RemoveSourceIdentifierFromSubscription = RemoveSourceIdentifierFromSubscription
    { _rsifsSourceIdentifier :: Text
    , _rsifsSubscriptionName :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'RemoveSourceIdentifierFromSubscription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsifsSourceIdentifier' @::@ 'Text'
--
-- * 'rsifsSubscriptionName' @::@ 'Text'
--
removeSourceIdentifierFromSubscription :: Text -- ^ 'rsifsSubscriptionName'
                                       -> Text -- ^ 'rsifsSourceIdentifier'
                                       -> RemoveSourceIdentifierFromSubscription
removeSourceIdentifierFromSubscription p1 p2 = RemoveSourceIdentifierFromSubscription
    { _rsifsSubscriptionName = p1
    , _rsifsSourceIdentifier = p2
    }

-- | The source identifier to be removed from the subscription, such as the DB
-- instance identifier for a DB instance or the name of a security group.
rsifsSourceIdentifier :: Lens' RemoveSourceIdentifierFromSubscription Text
rsifsSourceIdentifier =
    lens _rsifsSourceIdentifier (\s a -> s { _rsifsSourceIdentifier = a })

-- | The name of the RDS event notification subscription you want to remove a
-- source identifier from.
rsifsSubscriptionName :: Lens' RemoveSourceIdentifierFromSubscription Text
rsifsSubscriptionName =
    lens _rsifsSubscriptionName (\s a -> s { _rsifsSubscriptionName = a })

newtype RemoveSourceIdentifierFromSubscriptionResponse = RemoveSourceIdentifierFromSubscriptionResponse
    { _rsifsrEventSubscription :: Maybe EventSubscription
    } deriving (Eq, Show, Generic)

-- | 'RemoveSourceIdentifierFromSubscriptionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rsifsrEventSubscription' @::@ 'Maybe' 'EventSubscription'
--
removeSourceIdentifierFromSubscriptionResponse :: RemoveSourceIdentifierFromSubscriptionResponse
removeSourceIdentifierFromSubscriptionResponse = RemoveSourceIdentifierFromSubscriptionResponse
    { _rsifsrEventSubscription = Nothing
    }

rsifsrEventSubscription :: Lens' RemoveSourceIdentifierFromSubscriptionResponse (Maybe EventSubscription)
rsifsrEventSubscription =
    lens _rsifsrEventSubscription (\s a -> s { _rsifsrEventSubscription = a })

instance ToPath RemoveSourceIdentifierFromSubscription where
    toPath = const "/"

instance ToQuery RemoveSourceIdentifierFromSubscription

instance ToHeaders RemoveSourceIdentifierFromSubscription

instance AWSRequest RemoveSourceIdentifierFromSubscription where
    type Sv RemoveSourceIdentifierFromSubscription = RDS
    type Rs RemoveSourceIdentifierFromSubscription = RemoveSourceIdentifierFromSubscriptionResponse

    request  = post "RemoveSourceIdentifierFromSubscription"
    response = xmlResponse

instance FromXML RemoveSourceIdentifierFromSubscriptionResponse where
    parseXML c = RemoveSourceIdentifierFromSubscriptionResponse
        <$> c .:? "EventSubscription"
