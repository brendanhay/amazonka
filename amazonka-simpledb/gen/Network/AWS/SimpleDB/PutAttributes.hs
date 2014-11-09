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

-- Module      : Network.AWS.SimpleDB.PutAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The PutAttributes operation creates or replaces attributes in an item. The
-- client may specify new attributes using a combination of the
-- Attribute.X.Name and Attribute.X.Value parameters. The client specifies the
-- first attribute by the parameters Attribute.0.Name and Attribute.0.Value,
-- the second attribute by the parameters Attribute.1.Name and
-- Attribute.1.Value, and so on. Attributes are uniquely identified in an item
-- by their name/value combination. For example, a single item can have the
-- attributes { "first_name", "first_value" } and { "first_name",
-- second_value" }. However, it cannot have two attribute instances where both
-- the Attribute.X.Name and Attribute.X.Value are the same. Optionally, the
-- requestor can supply the Replace parameter for each individual attribute.
-- Setting this value to true causes the new attribute value to replace the
-- existing attribute value(s). For example, if an item has the attributes {
-- 'a', '1' }, { 'b', '2'} and { 'b', '3' } and the requestor calls
-- PutAttributes using the attributes { 'b', '4' } with the Replace parameter
-- set to true, the final attributes of the item are changed to { 'a', '1' }
-- and { 'b', '4' }, which replaces the previous values of the 'b' attribute
-- with the new value. You cannot specify an empty string as an attribute
-- name. Because Amazon SimpleDB makes multiple copies of client data and uses
-- an eventual consistency update model, an immediate GetAttributes or Select
-- operation (read) immediately after a PutAttributes or DeleteAttributes
-- operation (write) might not return the updated data. The following
-- limitations are enforced for this operation: 256 total attribute name-value
-- pairs per item One billion attributes per domain 10 GB of total user data
-- storage per domain.
module Network.AWS.SimpleDB.PutAttributes
    (
    -- * Request
      PutAttributes
    -- ** Request constructor
    , putAttributes
    -- ** Request lenses
    , paAttributes
    , paDomainName
    , paExpected
    , paItemName

    -- * Response
    , PutAttributesResponse
    -- ** Response constructor
    , putAttributesResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SimpleDB.Types

data PutAttributes = PutAttributes
    { _paAttributes :: [ReplaceableAttribute]
    , _paDomainName :: Text
    , _paExpected   :: Maybe UpdateCondition
    , _paItemName   :: Text
    } deriving (Eq, Show, Generic)

-- | 'PutAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'paAttributes' @::@ ['ReplaceableAttribute']
--
-- * 'paDomainName' @::@ 'Text'
--
-- * 'paExpected' @::@ 'Maybe' 'UpdateCondition'
--
-- * 'paItemName' @::@ 'Text'
--
putAttributes :: Text -- ^ 'paDomainName'
              -> Text -- ^ 'paItemName'
              -> PutAttributes
putAttributes p1 p2 = PutAttributes
    { _paDomainName = p1
    , _paItemName   = p2
    , _paAttributes = mempty
    , _paExpected   = Nothing
    }

-- | The list of attributes.
paAttributes :: Lens' PutAttributes [ReplaceableAttribute]
paAttributes = lens _paAttributes (\s a -> s { _paAttributes = a })

-- | The name of the domain in which to perform the operation.
paDomainName :: Lens' PutAttributes Text
paDomainName = lens _paDomainName (\s a -> s { _paDomainName = a })

-- | The update condition which, if specified, determines whether the
-- specified attributes will be updated or not. The update condition must be
-- satisfied in order for this request to be processed and the attributes to
-- be updated.
paExpected :: Lens' PutAttributes (Maybe UpdateCondition)
paExpected = lens _paExpected (\s a -> s { _paExpected = a })

-- | The name of the item.
paItemName :: Lens' PutAttributes Text
paItemName = lens _paItemName (\s a -> s { _paItemName = a })

instance ToPath PutAttributes where
    toPath = const "/"

instance ToQuery PutAttributes

data PutAttributesResponse = PutAttributesResponse

-- | 'PutAttributesResponse' constructor.
putAttributesResponse :: PutAttributesResponse
putAttributesResponse = PutAttributesResponse

instance AWSRequest PutAttributes where
    type Sv PutAttributes = SimpleDB
    type Rs PutAttributes = PutAttributesResponse

    request  = post "PutAttributes"
    response = const (nullaryResponse PutAttributesResponse)
