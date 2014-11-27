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

-- Module      : Network.AWS.RDS.AddSourceIdentifierToSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Adds a source identifier to an existing RDS event notification subscription.
--
-- <http://docs.aws.amazon.com/AmazonRDS/latest/APIReference/API_AddSourceIdentifierToSubscription.html>
module Network.AWS.RDS.AddSourceIdentifierToSubscription
    (
    -- * Request
      AddSourceIdentifierToSubscription
    -- ** Request constructor
    , addSourceIdentifierToSubscription
    -- ** Request lenses
    , asitsSourceIdentifier
    , asitsSubscriptionName

    -- * Response
    , AddSourceIdentifierToSubscriptionResponse
    -- ** Response constructor
    , addSourceIdentifierToSubscriptionResponse
    -- ** Response lenses
    , asitsrEventSubscription
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.RDS.Types
import qualified GHC.Exts

data AddSourceIdentifierToSubscription = AddSourceIdentifierToSubscription
    { _asitsSourceIdentifier :: Text
    , _asitsSubscriptionName :: Text
    } deriving (Eq, Ord, Show)

-- | 'AddSourceIdentifierToSubscription' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asitsSourceIdentifier' @::@ 'Text'
--
-- * 'asitsSubscriptionName' @::@ 'Text'
--
addSourceIdentifierToSubscription :: Text -- ^ 'asitsSubscriptionName'
                                  -> Text -- ^ 'asitsSourceIdentifier'
                                  -> AddSourceIdentifierToSubscription
addSourceIdentifierToSubscription p1 p2 = AddSourceIdentifierToSubscription
    { _asitsSubscriptionName = p1
    , _asitsSourceIdentifier = p2
    }

-- | The identifier of the event source to be added. An identifier must begin
-- with a letter and must contain only ASCII letters, digits, and hyphens; it
-- cannot end with a hyphen or contain two consecutive hyphens.
--
-- Constraints:
--
-- If the source type is a DB instance, then a 'DBInstanceIdentifier' must be
-- supplied. If the source type is a DB security group, a 'DBSecurityGroupName'
-- must be supplied. If the source type is a DB parameter group, a 'DBParameterGroupName' must be supplied. If the source type is a DB snapshot, a 'DBSnapshotIdentifier'
-- must be supplied.
asitsSourceIdentifier :: Lens' AddSourceIdentifierToSubscription Text
asitsSourceIdentifier =
    lens _asitsSourceIdentifier (\s a -> s { _asitsSourceIdentifier = a })

-- | The name of the RDS event notification subscription you want to add a source
-- identifier to.
asitsSubscriptionName :: Lens' AddSourceIdentifierToSubscription Text
asitsSubscriptionName =
    lens _asitsSubscriptionName (\s a -> s { _asitsSubscriptionName = a })

newtype AddSourceIdentifierToSubscriptionResponse = AddSourceIdentifierToSubscriptionResponse
    { _asitsrEventSubscription :: Maybe EventSubscription
    } deriving (Eq, Show)

-- | 'AddSourceIdentifierToSubscriptionResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'asitsrEventSubscription' @::@ 'Maybe' 'EventSubscription'
--
addSourceIdentifierToSubscriptionResponse :: AddSourceIdentifierToSubscriptionResponse
addSourceIdentifierToSubscriptionResponse = AddSourceIdentifierToSubscriptionResponse
    { _asitsrEventSubscription = Nothing
    }

asitsrEventSubscription :: Lens' AddSourceIdentifierToSubscriptionResponse (Maybe EventSubscription)
asitsrEventSubscription =
    lens _asitsrEventSubscription (\s a -> s { _asitsrEventSubscription = a })

instance ToPath AddSourceIdentifierToSubscription where
    toPath = const "/"

instance ToQuery AddSourceIdentifierToSubscription where
    toQuery AddSourceIdentifierToSubscription{..} = mconcat
        [ "SourceIdentifier" =? _asitsSourceIdentifier
        , "SubscriptionName" =? _asitsSubscriptionName
        ]

instance ToHeaders AddSourceIdentifierToSubscription

instance AWSRequest AddSourceIdentifierToSubscription where
    type Sv AddSourceIdentifierToSubscription = RDS
    type Rs AddSourceIdentifierToSubscription = AddSourceIdentifierToSubscriptionResponse

    request  = post "AddSourceIdentifierToSubscription"
    response = xmlResponse

instance FromXML AddSourceIdentifierToSubscriptionResponse where
    parseXML = withElement "AddSourceIdentifierToSubscriptionResult" $ \x -> AddSourceIdentifierToSubscriptionResponse
        <$> x .@? "EventSubscription"
