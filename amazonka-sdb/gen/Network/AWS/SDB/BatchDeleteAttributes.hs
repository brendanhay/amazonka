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
-- Module      : Network.AWS.SDB.BatchDeleteAttributes
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs multiple DeleteAttributes operations in a single call, which reduces round trips and latencies. This enables Amazon SimpleDB to optimize requests, which generally yields better throughput.
--
--
-- The following limitations are enforced for this operation:     * 1 MB request size    * 25 item limit per BatchDeleteAttributes operation
--
--
--
module Network.AWS.SDB.BatchDeleteAttributes
    (
    -- * Creating a Request
      batchDeleteAttributes
    , BatchDeleteAttributes
    -- * Request Lenses
    , bdaDomainName
    , bdaItems

    -- * Destructuring the Response
    , batchDeleteAttributesResponse
    , BatchDeleteAttributesResponse
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SDB.Types
import Network.AWS.SDB.Types.Product

-- | /See:/ 'batchDeleteAttributes' smart constructor.
data BatchDeleteAttributes = BatchDeleteAttributes'
  { _bdaDomainName :: !Text
  , _bdaItems      :: ![DeletableItem]
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDeleteAttributes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'bdaDomainName' - The name of the domain in which the attributes are being deleted.
--
-- * 'bdaItems' - A list of items on which to perform the operation.
batchDeleteAttributes
    :: Text -- ^ 'bdaDomainName'
    -> BatchDeleteAttributes
batchDeleteAttributes pDomainName_ =
  BatchDeleteAttributes' {_bdaDomainName = pDomainName_, _bdaItems = mempty}


-- | The name of the domain in which the attributes are being deleted.
bdaDomainName :: Lens' BatchDeleteAttributes Text
bdaDomainName = lens _bdaDomainName (\ s a -> s{_bdaDomainName = a})

-- | A list of items on which to perform the operation.
bdaItems :: Lens' BatchDeleteAttributes [DeletableItem]
bdaItems = lens _bdaItems (\ s a -> s{_bdaItems = a}) . _Coerce

instance AWSRequest BatchDeleteAttributes where
        type Rs BatchDeleteAttributes =
             BatchDeleteAttributesResponse
        request = postQuery sdb
        response = receiveNull BatchDeleteAttributesResponse'

instance Hashable BatchDeleteAttributes where

instance NFData BatchDeleteAttributes where

instance ToHeaders BatchDeleteAttributes where
        toHeaders = const mempty

instance ToPath BatchDeleteAttributes where
        toPath = const "/"

instance ToQuery BatchDeleteAttributes where
        toQuery BatchDeleteAttributes'{..}
          = mconcat
              ["Action" =: ("BatchDeleteAttributes" :: ByteString),
               "Version" =: ("2009-04-15" :: ByteString),
               "DomainName" =: _bdaDomainName,
               toQueryList "Item" _bdaItems]

-- | /See:/ 'batchDeleteAttributesResponse' smart constructor.
data BatchDeleteAttributesResponse =
  BatchDeleteAttributesResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'BatchDeleteAttributesResponse' with the minimum fields required to make a request.
--
batchDeleteAttributesResponse
    :: BatchDeleteAttributesResponse
batchDeleteAttributesResponse = BatchDeleteAttributesResponse'


instance NFData BatchDeleteAttributesResponse where
