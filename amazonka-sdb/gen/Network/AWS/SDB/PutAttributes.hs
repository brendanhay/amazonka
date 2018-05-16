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
-- Module      : Network.AWS.SDB.PutAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The PutAttributes operation creates or replaces attributes in an item. The client may specify new attributes using a combination of the @Attribute.X.Name@ and @Attribute.X.Value@ parameters. The client specifies the first attribute by the parameters @Attribute.0.Name@ and @Attribute.0.Value@ , the second attribute by the parameters @Attribute.1.Name@ and @Attribute.1.Value@ , and so on.
--
--
-- Attributes are uniquely identified in an item by their name/value combination. For example, a single item can have the attributes @{ "first_name", "first_value" }@ and @{ "first_name", second_value" }@ . However, it cannot have two attribute instances where both the @Attribute.X.Name@ and @Attribute.X.Value@ are the same.
--
-- Optionally, the requestor can supply the @Replace@ parameter for each individual attribute. Setting this value to @true@ causes the new attribute value to replace the existing attribute value(s). For example, if an item has the attributes @{ 'a', '1' }@ , @{ 'b', '2'}@ and @{ 'b', '3' }@ and the requestor calls @PutAttributes@ using the attributes @{ 'b', '4' }@ with the @Replace@ parameter set to true, the final attributes of the item are changed to @{ 'a', '1' }@ and @{ 'b', '4' }@ , which replaces the previous values of the 'b' attribute with the new value.
--
-- You cannot specify an empty string as an attribute name.
--
-- Because Amazon SimpleDB makes multiple copies of client data and uses an eventual consistency update model, an immediate 'GetAttributes' or 'Select' operation (read) immediately after a 'PutAttributes' or 'DeleteAttributes' operation (write) might not return the updated data.
--
-- The following limitations are enforced for this operation:     * 256 total attribute name-value pairs per item    * One billion attributes per domain    * 10 GB of total user data storage per domain
--
--
--
module Network.AWS.SDB.PutAttributes
    (
    -- * Creating a Request
      putAttributes
    , PutAttributes
    -- * Request Lenses
    , paExpected
    , paDomainName
    , paItemName
    , paAttributes

    -- * Destructuring the Response
    , putAttributesResponse
    , PutAttributesResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SDB.Types
import Network.AWS.SDB.Types.Product

-- | /See:/ 'putAttributes' smart constructor.
data PutAttributes = PutAttributes'
  { _paExpected   :: !(Maybe UpdateCondition)
  , _paDomainName :: !Text
  , _paItemName   :: !Text
  , _paAttributes :: ![ReplaceableAttribute]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'paExpected' - The update condition which, if specified, determines whether the specified attributes will be updated or not. The update condition must be satisfied in order for this request to be processed and the attributes to be updated.
--
-- * 'paDomainName' - The name of the domain in which to perform the operation.
--
-- * 'paItemName' - The name of the item.
--
-- * 'paAttributes' - The list of attributes.
putAttributes
    :: Text -- ^ 'paDomainName'
    -> Text -- ^ 'paItemName'
    -> PutAttributes
putAttributes pDomainName_ pItemName_ =
  PutAttributes'
    { _paExpected = Nothing
    , _paDomainName = pDomainName_
    , _paItemName = pItemName_
    , _paAttributes = mempty
    }


-- | The update condition which, if specified, determines whether the specified attributes will be updated or not. The update condition must be satisfied in order for this request to be processed and the attributes to be updated.
paExpected :: Lens' PutAttributes (Maybe UpdateCondition)
paExpected = lens _paExpected (\ s a -> s{_paExpected = a})

-- | The name of the domain in which to perform the operation.
paDomainName :: Lens' PutAttributes Text
paDomainName = lens _paDomainName (\ s a -> s{_paDomainName = a})

-- | The name of the item.
paItemName :: Lens' PutAttributes Text
paItemName = lens _paItemName (\ s a -> s{_paItemName = a})

-- | The list of attributes.
paAttributes :: Lens' PutAttributes [ReplaceableAttribute]
paAttributes = lens _paAttributes (\ s a -> s{_paAttributes = a}) . _Coerce

instance AWSRequest PutAttributes where
        type Rs PutAttributes = PutAttributesResponse
        request = postQuery sdb
        response = receiveNull PutAttributesResponse'

instance Hashable PutAttributes where

instance NFData PutAttributes where

instance ToHeaders PutAttributes where
        toHeaders = const mempty

instance ToPath PutAttributes where
        toPath = const "/"

instance ToQuery PutAttributes where
        toQuery PutAttributes'{..}
          = mconcat
              ["Action" =: ("PutAttributes" :: ByteString),
               "Version" =: ("2009-04-15" :: ByteString),
               "Expected" =: _paExpected,
               "DomainName" =: _paDomainName,
               "ItemName" =: _paItemName,
               toQueryList "Attribute" _paAttributes]

-- | /See:/ 'putAttributesResponse' smart constructor.
data PutAttributesResponse =
  PutAttributesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'PutAttributesResponse' with the minimum fields required to make a request.
--
putAttributesResponse
    :: PutAttributesResponse
putAttributesResponse = PutAttributesResponse'


instance NFData PutAttributesResponse where
