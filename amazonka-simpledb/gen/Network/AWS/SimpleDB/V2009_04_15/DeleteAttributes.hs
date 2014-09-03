{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SimpleDB.V2009_04_15.DeleteAttributes
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
module Network.AWS.SimpleDB.V2009_04_15.DeleteAttributes
    (
    -- * Request
      DeleteAttributes
    -- ** Request constructor
    , deleteAttributes
    -- ** Request lenses
    , darDomainName
    , darItemName
    , darAttributes
    , darExpected

    -- * Response
    , DeleteAttributesResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SimpleDB.V2009_04_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DeleteAttributes' request.
deleteAttributes :: Text -- ^ 'darDomainName'
                 -> Text -- ^ 'darItemName'
                 -> DeleteAttributes
deleteAttributes p1 p2 = DeleteAttributes
    { _darDomainName = p1
    , _darItemName = p2
    , _darAttributes = mempty
    , _darExpected = Nothing
    }

data DeleteAttributes = DeleteAttributes
    { _darDomainName :: Text
      -- ^ The name of the domain in which to perform the operation.
    , _darItemName :: Text
      -- ^ The name of the item. Similar to rows on a spreadsheet, items
      -- represent individual objects that contain one or more
      -- value-attribute pairs.
    , _darAttributes :: [Attribute]
      -- ^ A list of Attributes. Similar to columns on a spreadsheet,
      -- attributes represent categories of data that can be assigned to
      -- items.
    , _darExpected :: Maybe UpdateCondition
      -- ^ The update condition which, if specified, determines whether the
      -- specified attributes will be deleted or not. The update condition
      -- must be satisfied in order for this request to be processed and
      -- the attributes to be deleted.
    } deriving (Show, Generic)

-- | The name of the domain in which to perform the operation.
darDomainName
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteAttributes
    -> f DeleteAttributes
darDomainName f x =
    (\y -> x { _darDomainName = y })
       <$> f (_darDomainName x)
{-# INLINE darDomainName #-}

-- | The name of the item. Similar to rows on a spreadsheet, items represent
-- individual objects that contain one or more value-attribute pairs.
darItemName
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteAttributes
    -> f DeleteAttributes
darItemName f x =
    (\y -> x { _darItemName = y })
       <$> f (_darItemName x)
{-# INLINE darItemName #-}

-- | A list of Attributes. Similar to columns on a spreadsheet, attributes
-- represent categories of data that can be assigned to items.
darAttributes
    :: Functor f
    => ([Attribute]
    -> f ([Attribute]))
    -> DeleteAttributes
    -> f DeleteAttributes
darAttributes f x =
    (\y -> x { _darAttributes = y })
       <$> f (_darAttributes x)
{-# INLINE darAttributes #-}

-- | The update condition which, if specified, determines whether the specified
-- attributes will be deleted or not. The update condition must be satisfied
-- in order for this request to be processed and the attributes to be deleted.
darExpected
    :: Functor f
    => (Maybe UpdateCondition
    -> f (Maybe UpdateCondition))
    -> DeleteAttributes
    -> f DeleteAttributes
darExpected f x =
    (\y -> x { _darExpected = y })
       <$> f (_darExpected x)
{-# INLINE darExpected #-}

instance ToQuery DeleteAttributes where
    toQuery = genericQuery def

data DeleteAttributesResponse = DeleteAttributesResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DeleteAttributes where
    type Sv DeleteAttributes = SimpleDB
    type Rs DeleteAttributes = DeleteAttributesResponse

    request = post "DeleteAttributes"
    response _ = nullaryResponse DeleteAttributesResponse
