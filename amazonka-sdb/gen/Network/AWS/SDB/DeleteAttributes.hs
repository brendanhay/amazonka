{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.DeleteAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more attributes associated with an item. If all attributes of the item are deleted, the item is deleted.
--
--
-- @DeleteAttributes@ is an idempotent operation; running it multiple times on the same item or attribute does not result in an error response.
--
-- Because Amazon SimpleDB makes multiple copies of item data and uses an eventual consistency update model, performing a 'GetAttributes' or 'Select' operation (read) immediately after a @DeleteAttributes@ or 'PutAttributes' operation (write) might not return updated item data.
--
module Network.AWS.SDB.DeleteAttributes
    (
    -- * Creating a Request
      deleteAttributes
    , DeleteAttributes
    -- * Request Lenses
    , daAttributes
    , daExpected
    , daDomainName
    , daItemName

    -- * Destructuring the Response
    , deleteAttributesResponse
    , DeleteAttributesResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SDB.Types
import Network.AWS.SDB.Types.Product

-- | /See:/ 'deleteAttributes' smart constructor.
data DeleteAttributes = DeleteAttributes'
  { _daAttributes :: !(Maybe [Attribute])
  , _daExpected   :: !(Maybe UpdateCondition)
  , _daDomainName :: !Text
  , _daItemName   :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'daAttributes' - A list of Attributes. Similar to columns on a spreadsheet, attributes represent categories of data that can be assigned to items.
--
-- * 'daExpected' - The update condition which, if specified, determines whether the specified attributes will be deleted or not. The update condition must be satisfied in order for this request to be processed and the attributes to be deleted.
--
-- * 'daDomainName' - The name of the domain in which to perform the operation.
--
-- * 'daItemName' - The name of the item. Similar to rows on a spreadsheet, items represent individual objects that contain one or more value-attribute pairs.
deleteAttributes
    :: Text -- ^ 'daDomainName'
    -> Text -- ^ 'daItemName'
    -> DeleteAttributes
deleteAttributes pDomainName_ pItemName_ =
  DeleteAttributes'
    { _daAttributes = Nothing
    , _daExpected = Nothing
    , _daDomainName = pDomainName_
    , _daItemName = pItemName_
    }


-- | A list of Attributes. Similar to columns on a spreadsheet, attributes represent categories of data that can be assigned to items.
daAttributes :: Lens' DeleteAttributes [Attribute]
daAttributes = lens _daAttributes (\ s a -> s{_daAttributes = a}) . _Default . _Coerce

-- | The update condition which, if specified, determines whether the specified attributes will be deleted or not. The update condition must be satisfied in order for this request to be processed and the attributes to be deleted.
daExpected :: Lens' DeleteAttributes (Maybe UpdateCondition)
daExpected = lens _daExpected (\ s a -> s{_daExpected = a})

-- | The name of the domain in which to perform the operation.
daDomainName :: Lens' DeleteAttributes Text
daDomainName = lens _daDomainName (\ s a -> s{_daDomainName = a})

-- | The name of the item. Similar to rows on a spreadsheet, items represent individual objects that contain one or more value-attribute pairs.
daItemName :: Lens' DeleteAttributes Text
daItemName = lens _daItemName (\ s a -> s{_daItemName = a})

instance AWSRequest DeleteAttributes where
        type Rs DeleteAttributes = DeleteAttributesResponse
        request = postQuery sdb
        response = receiveNull DeleteAttributesResponse'

instance Hashable DeleteAttributes where

instance NFData DeleteAttributes where

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
data DeleteAttributesResponse =
  DeleteAttributesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteAttributesResponse' with the minimum fields required to make a request.
--
deleteAttributesResponse
    :: DeleteAttributesResponse
deleteAttributesResponse = DeleteAttributesResponse'


instance NFData DeleteAttributesResponse where
