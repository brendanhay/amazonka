{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SDB.DeleteAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes one or more attributes associated with an item. If all
-- attributes of the item are deleted, the item is deleted.
--
-- @DeleteAttributes@ is an idempotent operation; running it multiple times
-- on the same item or attribute does not result in an error response.
--
-- Because Amazon SimpleDB makes multiple copies of item data and uses an
-- eventual consistency update model, performing a GetAttributes or Select
-- operation (read) immediately after a @DeleteAttributes@ or PutAttributes
-- operation (write) might not return updated item data.
--
-- <http://docs.aws.amazon.com/AmazonSimpleDB/latest/DeveloperGuide/SDB_API_DeleteAttributes.html>
module Network.AWS.SDB.DeleteAttributes
    (
    -- * Request
      DeleteAttributes
    -- ** Request constructor
    , deleteAttributes
    -- ** Request lenses
    , daAttributes
    , daExpected
    , daDomainName
    , daItemName

    -- * Response
    , DeleteAttributesResponse
    -- ** Response constructor
    , deleteAttributesResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SDB.Types

-- | /See:/ 'deleteAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'daAttributes'
--
-- * 'daExpected'
--
-- * 'daDomainName'
--
-- * 'daItemName'
data DeleteAttributes = DeleteAttributes'{_daAttributes :: Maybe [Attribute], _daExpected :: Maybe UpdateCondition, _daDomainName :: Text, _daItemName :: Text} deriving (Eq, Read, Show)

-- | 'DeleteAttributes' smart constructor.
deleteAttributes :: Text -> Text -> DeleteAttributes
deleteAttributes pDomainName pItemName = DeleteAttributes'{_daAttributes = Nothing, _daExpected = Nothing, _daDomainName = pDomainName, _daItemName = pItemName};

-- | A list of Attributes. Similar to columns on a spreadsheet, attributes
-- represent categories of data that can be assigned to items.
daAttributes :: Lens' DeleteAttributes [Attribute]
daAttributes = lens _daAttributes (\ s a -> s{_daAttributes = a}) . _Default;

-- | The update condition which, if specified, determines whether the
-- specified attributes will be deleted or not. The update condition must
-- be satisfied in order for this request to be processed and the
-- attributes to be deleted.
daExpected :: Lens' DeleteAttributes (Maybe UpdateCondition)
daExpected = lens _daExpected (\ s a -> s{_daExpected = a});

-- | The name of the domain in which to perform the operation.
daDomainName :: Lens' DeleteAttributes Text
daDomainName = lens _daDomainName (\ s a -> s{_daDomainName = a});

-- | The name of the item. Similar to rows on a spreadsheet, items represent
-- individual objects that contain one or more value-attribute pairs.
daItemName :: Lens' DeleteAttributes Text
daItemName = lens _daItemName (\ s a -> s{_daItemName = a});

instance AWSRequest DeleteAttributes where
        type Sv DeleteAttributes = SDB
        type Rs DeleteAttributes = DeleteAttributesResponse
        request = post
        response = receiveNull DeleteAttributesResponse'

instance ToHeaders DeleteAttributes where
        toHeaders = const mempty

instance ToPath DeleteAttributes where
        toPath = const "/"

instance ToQuery DeleteAttributes where
        toQuery DeleteAttributes'{..}
          = mconcat
              ["Action" =: ("DeleteAttributes" :: ByteString),
               "Version" =: ("2009-04-15" :: ByteString),
               toQuery (toQueryList "Attribute" <$> _daAttributes),
               "Expected" =: _daExpected,
               "DomainName" =: _daDomainName,
               "ItemName" =: _daItemName]

-- | /See:/ 'deleteAttributesResponse' smart constructor.
data DeleteAttributesResponse = DeleteAttributesResponse' deriving (Eq, Read, Show)

-- | 'DeleteAttributesResponse' smart constructor.
deleteAttributesResponse :: DeleteAttributesResponse
deleteAttributesResponse = DeleteAttributesResponse';
