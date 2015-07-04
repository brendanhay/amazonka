{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Module      : Network.AWS.SDB.BatchPutAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The @BatchPutAttributes@ operation creates or replaces attributes within
-- one or more items. By using this operation, the client can perform
-- multiple PutAttribute operation with a single call. This helps yield
-- savings in round trips and latencies, enabling Amazon SimpleDB to
-- optimize requests and generally produce better throughput.
--
-- The client may specify the item name with the @Item.X.ItemName@
-- parameter. The client may specify new attributes using a combination of
-- the @Item.X.Attribute.Y.Name@ and @Item.X.Attribute.Y.Value@ parameters.
-- The client may specify the first attribute for the first item using the
-- parameters @Item.0.Attribute.0.Name@ and @Item.0.Attribute.0.Value@, and
-- for the second attribute for the first item by the parameters
-- @Item.0.Attribute.1.Name@ and @Item.0.Attribute.1.Value@, and so on.
--
-- Attributes are uniquely identified within an item by their name\/value
-- combination. For example, a single item can have the attributes
-- @{ \"first_name\", \"first_value\" }@ and
-- @{ \"first_name\", \"second_value\" }@. However, it cannot have two
-- attribute instances where both the @Item.X.Attribute.Y.Name@ and
-- @Item.X.Attribute.Y.Value@ are the same.
--
-- Optionally, the requester can supply the @Replace@ parameter for each
-- individual value. Setting this value to @true@ will cause the new
-- attribute values to replace the existing attribute values. For example,
-- if an item @I@ has the attributes @{ \'a\', \'1\' }, { \'b\', \'2\'}@
-- and @{ \'b\', \'3\' }@ and the requester does a BatchPutAttributes of
-- @{\'I\', \'b\', \'4\' }@ with the Replace parameter set to true, the
-- final attributes of the item will be @{ \'a\', \'1\' }@ and
-- @{ \'b\', \'4\' }@, replacing the previous values of the \'b\' attribute
-- with the new value.
--
-- This operation is vulnerable to exceeding the maximum URL size when
-- making a REST request using the HTTP GET method. This operation does not
-- support conditions using @Expected.X.Name@, @Expected.X.Value@, or
-- @Expected.X.Exists@.
--
-- You can execute multiple @BatchPutAttributes@ operations and other
-- operations in parallel. However, large numbers of concurrent
-- @BatchPutAttributes@ calls can result in Service Unavailable (503)
-- responses.
--
-- The following limitations are enforced for this operation:
--
-- -   256 attribute name-value pairs per item
-- -   1 MB request size
-- -   1 billion attributes per domain
-- -   10 GB of total user data storage per domain
-- -   25 item limit per @BatchPutAttributes@ operation
--
-- <http://docs.aws.amazon.com/AmazonSimpleDB/latest/DeveloperGuide/SDB_API_BatchPutAttributes.html>
module Network.AWS.SDB.BatchPutAttributes
    (
    -- * Request
      BatchPutAttributes
    -- ** Request constructor
    , batchPutAttributes
    -- ** Request lenses
    , bpaDomainName
    , bpaItems

    -- * Response
    , BatchPutAttributesResponse
    -- ** Response constructor
    , batchPutAttributesResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SDB.Types

-- | /See:/ 'batchPutAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bpaDomainName'
--
-- * 'bpaItems'
data BatchPutAttributes = BatchPutAttributes'
    { _bpaDomainName :: !Text
    , _bpaItems      :: ![ReplaceableItem]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BatchPutAttributes' smart constructor.
batchPutAttributes :: Text -> BatchPutAttributes
batchPutAttributes pDomainName =
    BatchPutAttributes'
    { _bpaDomainName = pDomainName
    , _bpaItems = mempty
    }

-- | The name of the domain in which the attributes are being stored.
bpaDomainName :: Lens' BatchPutAttributes Text
bpaDomainName = lens _bpaDomainName (\ s a -> s{_bpaDomainName = a});

-- | A list of items on which to perform the operation.
bpaItems :: Lens' BatchPutAttributes [ReplaceableItem]
bpaItems = lens _bpaItems (\ s a -> s{_bpaItems = a});

instance AWSRequest BatchPutAttributes where
        type Sv BatchPutAttributes = SDB
        type Rs BatchPutAttributes =
             BatchPutAttributesResponse
        request = post
        response = receiveNull BatchPutAttributesResponse'

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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BatchPutAttributesResponse' smart constructor.
batchPutAttributesResponse :: BatchPutAttributesResponse
batchPutAttributesResponse = BatchPutAttributesResponse'
