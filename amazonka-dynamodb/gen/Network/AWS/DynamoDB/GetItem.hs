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

-- Module      : Network.AWS.DynamoDB.GetItem
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The GetItem operation returns a set of attributes for the item with the
-- given primary key. If there is no matching item, GetItem does not return
-- any data. GetItem provides an eventually consistent read by default. If
-- your application requires a strongly consistent read, set ConsistentRead to
-- true. Although a strongly consistent read might take more time than an
-- eventually consistent read, it always returns the last updated value.
module Network.AWS.DynamoDB.GetItem
    (
    -- * Request
      GetItem
    -- ** Request constructor
    , getItem
    -- ** Request lenses
    , giAttributesToGet
    , giConsistentRead
    , giExpressionAttributeNames
    , giKey
    , giProjectionExpression
    , giReturnConsumedCapacity
    , giTableName

    -- * Response
    , GetItemResponse
    -- ** Response constructor
    , getItemResponse
    -- ** Response lenses
    , girConsumedCapacity
    , girItem
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.DynamoDB.Types

data GetItem = GetItem
    { _giAttributesToGet          :: List1 Text
    , _giConsistentRead           :: Maybe Bool
    , _giExpressionAttributeNames :: Map Text Text
    , _giKey                      :: Map Text AttributeValue
    , _giProjectionExpression     :: Maybe Text
    , _giReturnConsumedCapacity   :: Maybe Text
    , _giTableName                :: Text
    } deriving (Eq, Show, Generic)

-- | 'GetItem' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'giAttributesToGet' @::@ 'NonEmpty' 'Text'
--
-- * 'giConsistentRead' @::@ 'Maybe' 'Bool'
--
-- * 'giExpressionAttributeNames' @::@ 'HashMap' 'Text' 'Text'
--
-- * 'giKey' @::@ 'HashMap' 'Text' 'AttributeValue'
--
-- * 'giProjectionExpression' @::@ 'Maybe' 'Text'
--
-- * 'giReturnConsumedCapacity' @::@ 'Maybe' 'Text'
--
-- * 'giTableName' @::@ 'Text'
--
getItem :: Text -- ^ 'giTableName'
        -> NonEmpty Text -- ^ 'giAttributesToGet'
        -> GetItem
getItem p1 p2 = GetItem
    { _giTableName                = p1
    , _giAttributesToGet          = withIso _List1 (const id) p2
    , _giKey                      = mempty
    , _giConsistentRead           = Nothing
    , _giReturnConsumedCapacity   = Nothing
    , _giProjectionExpression     = Nothing
    , _giExpressionAttributeNames = mempty
    }

-- | There is a newer parameter available. Use ProjectionExpression instead.
-- Note that if you use AttributesToGet and ProjectionExpression at the same
-- time, DynamoDB will return a ValidationException exception. This
-- parameter allows you to retrieve lists or maps; however, it cannot
-- retrieve individual list or map elements. The names of one or more
-- attributes to retrieve. If no attribute names are specified, then all
-- attributes will be returned. If any of the requested attributes are not
-- found, they will not appear in the result. Note that AttributesToGet has
-- no effect on provisioned throughput consumption. DynamoDB determines
-- capacity units consumed based on item size, not on the amount of data
-- that is returned to an application.
giAttributesToGet :: Lens' GetItem (NonEmpty Text)
giAttributesToGet =
    lens _giAttributesToGet (\s a -> s { _giAttributesToGet = a })
        . _List1

-- | A value that if set to true, then the operation uses strongly consistent
-- reads; otherwise, eventually consistent reads are used.
giConsistentRead :: Lens' GetItem (Maybe Bool)
giConsistentRead = lens _giConsistentRead (\s a -> s { _giConsistentRead = a })

-- | One or more substitution tokens for simplifying complex expressions. The
-- following are some use cases for an ExpressionAttributeNames value: To
-- shorten an attribute name that is very long or unwieldy in an expression.
-- To create a placeholder for repeating occurrences of an attribute name in
-- an expression. To prevent special characters in an attribute name from
-- being misinterpreted in an expression. Use the # character in an
-- expression to dereference an attribute name. For example, consider the
-- following expression: order.customerInfo.LastName = "Smith" OR
-- order.customerInfo.LastName = "Jones" Now suppose that you specified the
-- following for ExpressionAttributeNames:
-- {"n":"order.customerInfo.LastName"} The expression can now be simplified
-- as follows: #n = "Smith" OR #n = "Jones".
giExpressionAttributeNames :: Lens' GetItem (HashMap Text Text)
giExpressionAttributeNames =
    lens _giExpressionAttributeNames
        (\s a -> s { _giExpressionAttributeNames = a })
            . _Map

-- | A map of attribute names to AttributeValue objects, representing the
-- primary key of the item to retrieve. For the primary key, you must
-- provide all of the attributes. For example, with a hash type primary key,
-- you only need to specify the hash attribute. For a hash-and-range type
-- primary key, you must specify both the hash attribute and the range
-- attribute.
giKey :: Lens' GetItem (HashMap Text AttributeValue)
giKey = lens _giKey (\s a -> s { _giKey = a })
    . _Map

-- | One or more attributes to retrieve from the table. These attributes can
-- include scalars, sets, or elements of a JSON document. The attributes in
-- the expression must be separated by commas. If no attribute names are
-- specified, then all attributes will be returned. If any of the requested
-- attributes are not found, they will not appear in the result.
giProjectionExpression :: Lens' GetItem (Maybe Text)
giProjectionExpression =
    lens _giProjectionExpression (\s a -> s { _giProjectionExpression = a })

giReturnConsumedCapacity :: Lens' GetItem (Maybe Text)
giReturnConsumedCapacity =
    lens _giReturnConsumedCapacity
        (\s a -> s { _giReturnConsumedCapacity = a })

-- | The name of the table containing the requested item.
giTableName :: Lens' GetItem Text
giTableName = lens _giTableName (\s a -> s { _giTableName = a })

instance ToPath GetItem where
    toPath = const "/"

instance ToQuery GetItem where
    toQuery = const mempty

instance ToHeaders GetItem

instance ToBody GetItem where
    toBody = toBody . encode . _giTableName

data GetItemResponse = GetItemResponse
    { _girConsumedCapacity :: Maybe ConsumedCapacity
    , _girItem             :: Map Text AttributeValue
    } deriving (Eq, Show, Generic)

-- | 'GetItemResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'girConsumedCapacity' @::@ 'Maybe' 'ConsumedCapacity'
--
-- * 'girItem' @::@ 'HashMap' 'Text' 'AttributeValue'
--
getItemResponse :: GetItemResponse
getItemResponse = GetItemResponse
    { _girItem             = mempty
    , _girConsumedCapacity = Nothing
    }

girConsumedCapacity :: Lens' GetItemResponse (Maybe ConsumedCapacity)
girConsumedCapacity =
    lens _girConsumedCapacity (\s a -> s { _girConsumedCapacity = a })

-- | A map of attribute names to AttributeValue objects, as specified by
-- AttributesToGet.
girItem :: Lens' GetItemResponse (HashMap Text AttributeValue)
girItem = lens _girItem (\s a -> s { _girItem = a })
    . _Map

-- FromJSON

instance AWSRequest GetItem where
    type Sv GetItem = DynamoDB
    type Rs GetItem = GetItemResponse

    request  = post'
    response = jsonResponse $ \h o -> GetItemResponse
        <$> o .: "ConsumedCapacity"
        <*> o .: "Item"
