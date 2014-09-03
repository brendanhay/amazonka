{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.RDS.V2013_09_09.AddSourceIdentifierToSubscription
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Adds a source identifier to an existing RDS event notification
-- subscription. https://rds.us-east-1.amazonaws.com/
-- ?Action=AddSourceIdentifierToSubscription
-- ?SubscriptionName=EventSubscription01 &SourceIdentifier=dbinstance01
-- &Version=2013-01-10 &SignatureVersion=4 &SignatureMethod=HmacSHA256
-- &Timestamp=20130128T011410Z &AWSAccessKeyId= &Signature= true 012345678901
-- db-instance modifying dbinstance01 2013-01-28 00:29:23.736 creation
-- deletion EventSubscription01
-- arn:aws:sns:us-east-1:012345678901:EventSubscription01
-- 05d0fd8a-68e8-11e2-8e4d-31f087d822e1.
module Network.AWS.RDS.V2013_09_09.AddSourceIdentifierToSubscription
    (
    -- * Request
      AddSourceIdentifierToSubscription
    -- ** Request constructor
    , addSourceIdentifierToSubscription
    -- ** Request lenses
    , asitsmSubscriptionName
    , asitsmSourceIdentifier

    -- * Response
    , AddSourceIdentifierToSubscriptionResponse
    -- ** Response lenses
    , eswEventSubscription
    ) where

import Network.AWS.Request.Query
import Network.AWS.RDS.V2013_09_09.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'AddSourceIdentifierToSubscription' request.
addSourceIdentifierToSubscription :: Text -- ^ 'asitsmSubscriptionName'
                                  -> Text -- ^ 'asitsmSourceIdentifier'
                                  -> AddSourceIdentifierToSubscription
addSourceIdentifierToSubscription p1 p2 = AddSourceIdentifierToSubscription
    { _asitsmSubscriptionName = p1
    , _asitsmSourceIdentifier = p2
    }

data AddSourceIdentifierToSubscription = AddSourceIdentifierToSubscription
    { _asitsmSubscriptionName :: Text
      -- ^ The name of the RDS event notification subscription you want to
      -- add a source identifier to.
    , _asitsmSourceIdentifier :: Text
      -- ^ The identifier of the event source to be added. An identifier
      -- must begin with a letter and must contain only ASCII letters,
      -- digits, and hyphens; it cannot end with a hyphen or contain two
      -- consecutive hyphens. Constraints: If the source type is a DB
      -- instance, then a DBInstanceIdentifier must be supplied. If the
      -- source type is a DB security group, a DBSecurityGroupName must be
      -- supplied. If the source type is a DB parameter group, a
      -- DBParameterGroupName must be supplied. If the source type is a DB
      -- snapshot, a DBSnapshotIdentifier must be supplied.
    } deriving (Show, Generic)

-- | The name of the RDS event notification subscription you want to add a
-- source identifier to.
asitsmSubscriptionName
    :: Functor f
    => (Text
    -> f (Text))
    -> AddSourceIdentifierToSubscription
    -> f AddSourceIdentifierToSubscription
asitsmSubscriptionName f x =
    (\y -> x { _asitsmSubscriptionName = y })
       <$> f (_asitsmSubscriptionName x)
{-# INLINE asitsmSubscriptionName #-}

-- | The identifier of the event source to be added. An identifier must begin
-- with a letter and must contain only ASCII letters, digits, and hyphens; it
-- cannot end with a hyphen or contain two consecutive hyphens. Constraints:
-- If the source type is a DB instance, then a DBInstanceIdentifier must be
-- supplied. If the source type is a DB security group, a DBSecurityGroupName
-- must be supplied. If the source type is a DB parameter group, a
-- DBParameterGroupName must be supplied. If the source type is a DB snapshot,
-- a DBSnapshotIdentifier must be supplied.
asitsmSourceIdentifier
    :: Functor f
    => (Text
    -> f (Text))
    -> AddSourceIdentifierToSubscription
    -> f AddSourceIdentifierToSubscription
asitsmSourceIdentifier f x =
    (\y -> x { _asitsmSourceIdentifier = y })
       <$> f (_asitsmSourceIdentifier x)
{-# INLINE asitsmSourceIdentifier #-}

instance ToQuery AddSourceIdentifierToSubscription where
    toQuery = genericQuery def

data AddSourceIdentifierToSubscriptionResponse = AddSourceIdentifierToSubscriptionResponse
    { _eswEventSubscription :: Maybe EventSubscription
      -- ^ Contains the results of a successful invocation of the
      -- DescribeEventSubscriptions action.
    } deriving (Show, Generic)

-- | Contains the results of a successful invocation of the
-- DescribeEventSubscriptions action.
eswEventSubscription
    :: Functor f
    => (Maybe EventSubscription
    -> f (Maybe EventSubscription))
    -> AddSourceIdentifierToSubscriptionResponse
    -> f AddSourceIdentifierToSubscriptionResponse
eswEventSubscription f x =
    (\y -> x { _eswEventSubscription = y })
       <$> f (_eswEventSubscription x)
{-# INLINE eswEventSubscription #-}

instance FromXML AddSourceIdentifierToSubscriptionResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest AddSourceIdentifierToSubscription where
    type Sv AddSourceIdentifierToSubscription = RDS
    type Rs AddSourceIdentifierToSubscription = AddSourceIdentifierToSubscriptionResponse

    request = post "AddSourceIdentifierToSubscription"
    response _ = xmlResponse
