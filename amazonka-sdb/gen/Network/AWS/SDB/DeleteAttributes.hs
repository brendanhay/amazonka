{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.DeleteAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more attributes associated with an item. If all
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
    , darqAttributes
    , darqExpected
    , darqDomainName
    , darqItemName

    -- * Response
    , DeleteAttributesResponse
    -- ** Response constructor
    , deleteAttributesResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SDB.Types

-- | /See:/ 'deleteAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'darqAttributes'
--
-- * 'darqExpected'
--
-- * 'darqDomainName'
--
-- * 'darqItemName'
data DeleteAttributes = DeleteAttributes'
    { _darqAttributes :: !(Maybe [Attribute])
    , _darqExpected   :: !(Maybe UpdateCondition)
    , _darqDomainName :: !Text
    , _darqItemName   :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAttributes' smart constructor.
deleteAttributes :: Text -> Text -> DeleteAttributes
deleteAttributes pDomainName pItemName =
    DeleteAttributes'
    { _darqAttributes = Nothing
    , _darqExpected = Nothing
    , _darqDomainName = pDomainName
    , _darqItemName = pItemName
    }

-- | A list of Attributes. Similar to columns on a spreadsheet, attributes
-- represent categories of data that can be assigned to items.
darqAttributes :: Lens' DeleteAttributes [Attribute]
darqAttributes = lens _darqAttributes (\ s a -> s{_darqAttributes = a}) . _Default;

-- | The update condition which, if specified, determines whether the
-- specified attributes will be deleted or not. The update condition must
-- be satisfied in order for this request to be processed and the
-- attributes to be deleted.
darqExpected :: Lens' DeleteAttributes (Maybe UpdateCondition)
darqExpected = lens _darqExpected (\ s a -> s{_darqExpected = a});

-- | The name of the domain in which to perform the operation.
darqDomainName :: Lens' DeleteAttributes Text
darqDomainName = lens _darqDomainName (\ s a -> s{_darqDomainName = a});

-- | The name of the item. Similar to rows on a spreadsheet, items represent
-- individual objects that contain one or more value-attribute pairs.
darqItemName :: Lens' DeleteAttributes Text
darqItemName = lens _darqItemName (\ s a -> s{_darqItemName = a});

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
               toQuery
                 (toQueryList "Attribute" <$> _darqAttributes),
               "Expected" =: _darqExpected,
               "DomainName" =: _darqDomainName,
               "ItemName" =: _darqItemName]

-- | /See:/ 'deleteAttributesResponse' smart constructor.
data DeleteAttributesResponse =
    DeleteAttributesResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteAttributesResponse' smart constructor.
deleteAttributesResponse :: DeleteAttributesResponse
deleteAttributesResponse = DeleteAttributesResponse'
