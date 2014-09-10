{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SimpleDB.DeleteAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes one or more attributes associated with an item. If all attributes
-- of the item are deleted, the item is deleted. If DeleteAttributes is called
-- without being passed any attributes or values specified, all the attributes
-- for the item are deleted. DeleteAttributes is an idempotent operation;
-- running it multiple times on the same item or attribute does not result in
-- an error response. Because Amazon SimpleDB makes multiple copies of item
-- data and uses an eventual consistency update model, performing a
-- GetAttributes or Select operation (read) immediately after a
-- DeleteAttributes or PutAttributes operation (write) might not return
-- updated item data.
module Network.AWS.SimpleDB.DeleteAttributes
    (
    -- * Request
      DeleteAttributes
    -- ** Request constructor
    , mkDeleteAttributes
    -- ** Request lenses
    , daDomainName
    , daItemName
    , daAttributes
    , daExpected

    -- * Response
    , DeleteAttributesResponse
    -- ** Response constructor
    , mkDeleteAttributesResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SimpleDB.Types
import Network.AWS.Prelude

data DeleteAttributes = DeleteAttributes
    { _daDomainName :: !Text
    , _daItemName :: !Text
    , _daAttributes :: [Attribute]
    , _daExpected :: Maybe UpdateCondition
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteAttributes' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @ItemName ::@ @Text@
--
-- * @Attributes ::@ @[Attribute]@
--
-- * @Expected ::@ @Maybe UpdateCondition@
--
mkDeleteAttributes :: Text -- ^ 'daDomainName'
                   -> Text -- ^ 'daItemName'
                   -> DeleteAttributes
mkDeleteAttributes p1 p2 = DeleteAttributes
    { _daDomainName = p1
    , _daItemName = p2
    , _daAttributes = mempty
    , _daExpected = Nothing
    }

-- | The name of the domain in which to perform the operation.
daDomainName :: Lens' DeleteAttributes Text
daDomainName = lens _daDomainName (\s a -> s { _daDomainName = a })

-- | The name of the item. Similar to rows on a spreadsheet, items represent
-- individual objects that contain one or more value-attribute pairs.
daItemName :: Lens' DeleteAttributes Text
daItemName = lens _daItemName (\s a -> s { _daItemName = a })

-- | A list of Attributes. Similar to columns on a spreadsheet, attributes
-- represent categories of data that can be assigned to items.
daAttributes :: Lens' DeleteAttributes [Attribute]
daAttributes = lens _daAttributes (\s a -> s { _daAttributes = a })

-- | The update condition which, if specified, determines whether the specified
-- attributes will be deleted or not. The update condition must be satisfied
-- in order for this request to be processed and the attributes to be deleted.
daExpected :: Lens' DeleteAttributes (Maybe UpdateCondition)
daExpected = lens _daExpected (\s a -> s { _daExpected = a })

instance ToQuery DeleteAttributes where
    toQuery = genericQuery def

data DeleteAttributesResponse = DeleteAttributesResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteAttributesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteAttributesResponse :: DeleteAttributesResponse
mkDeleteAttributesResponse = DeleteAttributesResponse

instance AWSRequest DeleteAttributes where
    type Sv DeleteAttributes = SimpleDB
    type Rs DeleteAttributes = DeleteAttributesResponse

    request = post "DeleteAttributes"
    response _ = nullaryResponse DeleteAttributesResponse
