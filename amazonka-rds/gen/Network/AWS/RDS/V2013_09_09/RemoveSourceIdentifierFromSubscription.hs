{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.RemoveSourceIdentifierFromSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Removes a source identifier from an existing RDS event notification
-- subscription. https://rds.us-east-1.amazonaws.com/
-- ?Action=RemoveSourceIdentifierFromSubscription
-- &SubscriptionName=EventSubscription01 &SourceIdentifier=dbinstance01
-- &Version=2013-01-10 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20130128T012415Z &AWSAccessKeyId= &Signature= true 012345678901
-- db-instance modifying 2013-01-28 00:29:23.736 creation deletion
-- EventSubscription01 arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- 6f0b82bf-68e9-11e2-b97b-43c6362ec60d.
module Network.AWS.RDS.V2013_09_09.RemoveSourceIdentifierFromSubscription
    (
    -- * Request
      RemoveSourceIdentifierFromSubscription
    -- ** Request constructor
    , removeSourceIdentifierFromSubscription
    -- ** Request lenses
    , rsifsmSubscriptionName
    , rsifsmSourceIdentifier

    -- * Response
    , RemoveSourceIdentifierFromSubscriptionResponse
    -- ** Response lenses
    , eserEventSubscription
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'RemoveSourceIdentifierFromSubscription' request.
removeSourceIdentifierFromSubscription :: Text -- ^ 'rsifsmSubscriptionName'
                                       -> Text -- ^ 'rsifsmSourceIdentifier'
                                       -> RemoveSourceIdentifierFromSubscription
removeSourceIdentifierFromSubscription p1 p2 = RemoveSourceIdentifierFromSubscription
    { _rsifsmSubscriptionName = p1
    , _rsifsmSourceIdentifier = p2
    }

data RemoveSourceIdentifierFromSubscription = RemoveSourceIdentifierFromSubscription
    { _rsifsmSubscriptionName :: Text
      -- ^ The name of the RDS event notification subscription you want to
      -- remove a source identifier from.
    , _rsifsmSourceIdentifier :: Text
      -- ^ The source identifier to be removed from the subscription, such
      -- as the DB instance identifier for a DB instance or the name of a
      -- security group.
    } deriving (Show, Generic)

-- | The name of the RDS event notification subscription you want to remove a
-- source identifier from.
rsifsmSubscriptionName
    :: Functor f
    => (Text
    -> f (Text))
    -> RemoveSourceIdentifierFromSubscription
    -> f RemoveSourceIdentifierFromSubscription
rsifsmSubscriptionName f x =
    (\y -> x { _rsifsmSubscriptionName = y })
       <$> f (_rsifsmSubscriptionName x)
{-# INLINE rsifsmSubscriptionName #-}

-- | The source identifier to be removed from the subscription, such as the DB
-- instance identifier for a DB instance or the name of a security group.
rsifsmSourceIdentifier
    :: Functor f
    => (Text
    -> f (Text))
    -> RemoveSourceIdentifierFromSubscription
    -> f RemoveSourceIdentifierFromSubscription
rsifsmSourceIdentifier f x =
    (\y -> x { _rsifsmSourceIdentifier = y })
       <$> f (_rsifsmSourceIdentifier x)
{-# INLINE rsifsmSourceIdentifier #-}

instance ToQuery RemoveSourceIdentifierFromSubscription where
    toQuery = genericQuery def

data RemoveSourceIdentifierFromSubscriptionResponse = RemoveSourceIdentifierFromSubscriptionResponse
    { _eserEventSubscription :: Maybe EventSubscription
      -- ^ Contains the results of a successful invocation of the
      -- DescribeEventSubscriptions action.
    } deriving (Show, Generic)

-- | Contains the results of a successful invocation of the
-- DescribeEventSubscriptions action.
eserEventSubscription
    :: Functor f
    => (Maybe EventSubscription
    -> f (Maybe EventSubscription))
    -> RemoveSourceIdentifierFromSubscriptionResponse
    -> f RemoveSourceIdentifierFromSubscriptionResponse
eserEventSubscription f x =
    (\y -> x { _eserEventSubscription = y })
       <$> f (_eserEventSubscription x)
{-# INLINE eserEventSubscription #-}

instance FromXML RemoveSourceIdentifierFromSubscriptionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest RemoveSourceIdentifierFromSubscription where
    type Sv RemoveSourceIdentifierFromSubscription = RDS
    type Rs RemoveSourceIdentifierFromSubscription = RemoveSourceIdentifierFromSubscriptionResponse

    request = post "RemoveSourceIdentifierFromSubscription"
    response _ = xmlResponse
