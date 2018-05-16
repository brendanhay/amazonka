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
-- Module      : Network.AWS.SDB.BatchPutAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @BatchPutAttributes@ operation creates or replaces attributes within one or more items. By using this operation, the client can perform multiple 'PutAttribute' operation with a single call. This helps yield savings in round trips and latencies, enabling Amazon SimpleDB to optimize requests and generally produce better throughput.
--
--
-- The client may specify the item name with the @Item.X.ItemName@ parameter. The client may specify new attributes using a combination of the @Item.X.Attribute.Y.Name@ and @Item.X.Attribute.Y.Value@ parameters. The client may specify the first attribute for the first item using the parameters @Item.0.Attribute.0.Name@ and @Item.0.Attribute.0.Value@ , and for the second attribute for the first item by the parameters @Item.0.Attribute.1.Name@ and @Item.0.Attribute.1.Value@ , and so on.
--
-- Attributes are uniquely identified within an item by their name/value combination. For example, a single item can have the attributes @{ "first_name", "first_value" }@ and @{ "first_name", "second_value" }@ . However, it cannot have two attribute instances where both the @Item.X.Attribute.Y.Name@ and @Item.X.Attribute.Y.Value@ are the same.
--
-- Optionally, the requester can supply the @Replace@ parameter for each individual value. Setting this value to @true@ will cause the new attribute values to replace the existing attribute values. For example, if an item @I@ has the attributes @{ 'a', '1' }, { 'b', '2'}@ and @{ 'b', '3' }@ and the requester does a BatchPutAttributes of @{'I', 'b', '4' }@ with the Replace parameter set to true, the final attributes of the item will be @{ 'a', '1' }@ and @{ 'b', '4' }@ , replacing the previous values of the 'b' attribute with the new value.
--
-- /Important:/ This operation is vulnerable to exceeding the maximum URL size when making a REST request using the HTTP GET method. This operation does not support conditions using @Expected.X.Name@ , @Expected.X.Value@ , or @Expected.X.Exists@ . You can execute multiple @BatchPutAttributes@ operations and other operations in parallel. However, large numbers of concurrent @BatchPutAttributes@ calls can result in Service Unavailable (503) responses.
--
-- The following limitations are enforced for this operation:     * 256 attribute name-value pairs per item    * 1 MB request size    * 1 billion attributes per domain    * 10 GB of total user data storage per domain    * 25 item limit per @BatchPutAttributes@ operation
--
--
--
module Network.AWS.SDB.BatchPutAttributes
    (
    -- * Creating a Request
      batchPutAttributes
    , BatchPutAttributes
    -- * Request Lenses
    , bpaDomainName
    , bpaItems

    -- * Destructuring the Response
    , batchPutAttributesResponse
    , BatchPutAttributesResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SDB.Types
import Network.AWS.SDB.Types.Product

-- | /See:/ 'batchPutAttributes' smart constructor.
data BatchPutAttributes = BatchPutAttributes'
  { _bpaDomainName :: !Text
  , _bpaItems      :: ![ReplaceableItem]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchPutAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bpaDomainName' - The name of the domain in which the attributes are being stored.
--
-- * 'bpaItems' - A list of items on which to perform the operation.
batchPutAttributes
    :: Text -- ^ 'bpaDomainName'
    -> BatchPutAttributes
batchPutAttributes pDomainName_ =
  BatchPutAttributes' {_bpaDomainName = pDomainName_, _bpaItems = mempty}


-- | The name of the domain in which the attributes are being stored.
bpaDomainName :: Lens' BatchPutAttributes Text
bpaDomainName = lens _bpaDomainName (\ s a -> s{_bpaDomainName = a})

-- | A list of items on which to perform the operation.
bpaItems :: Lens' BatchPutAttributes [ReplaceableItem]
bpaItems = lens _bpaItems (\ s a -> s{_bpaItems = a}) . _Coerce

instance AWSRequest BatchPutAttributes where
        type Rs BatchPutAttributes =
             BatchPutAttributesResponse
        request = postQuery sdb
        response = receiveNull BatchPutAttributesResponse'

instance Hashable BatchPutAttributes where

instance NFData BatchPutAttributes where

instance ToHeaders BatchPutAttributes where
        toHeaders = const mempty

instance ToPath BatchPutAttributes where
        toPath = const "/"

instance ToQuery BatchPutAttributes where
        toQuery BatchPutAttributes'{..}
          = mconcat
              ["Action" =: ("BatchPutAttributes" :: ByteString),
               "Version" =: ("2009-04-15" :: ByteString),
               "DomainName" =: _bpaDomainName,
               toQueryList "Item" _bpaItems]

-- | /See:/ 'batchPutAttributesResponse' smart constructor.
data BatchPutAttributesResponse =
  BatchPutAttributesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchPutAttributesResponse' with the minimum fields required to make a request.
--
batchPutAttributesResponse
    :: BatchPutAttributesResponse
batchPutAttributesResponse = BatchPutAttributesResponse'


instance NFData BatchPutAttributesResponse where
