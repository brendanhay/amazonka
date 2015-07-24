{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.BatchDeleteAttributes
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Performs multiple DeleteAttributes operations in a single call, which
-- reduces round trips and latencies. This enables Amazon SimpleDB to
-- optimize requests, which generally yields better throughput.
--
-- The following limitations are enforced for this operation:
--
-- -   1 MB request size
-- -   25 item limit per BatchDeleteAttributes operation
--
-- <http://docs.aws.amazon.com/AmazonSimpleDB/latest/DeveloperGuide/SDB_API_BatchDeleteAttributes.html>
module Network.AWS.SDB.BatchDeleteAttributes
    (
    -- * Request
      BatchDeleteAttributes
    -- ** Request constructor
    , batchDeleteAttributes
    -- ** Request lenses
    , bdaDomainName
    , bdaItems

    -- * Response
    , BatchDeleteAttributesResponse
    -- ** Response constructor
    , batchDeleteAttributesResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SDB.Types

-- | /See:/ 'batchDeleteAttributes' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bdaDomainName'
--
-- * 'bdaItems'
data BatchDeleteAttributes = BatchDeleteAttributes'
    { _bdaDomainName :: !Text
    , _bdaItems      :: ![DeletableItem]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BatchDeleteAttributes' smart constructor.
batchDeleteAttributes :: Text -> BatchDeleteAttributes
batchDeleteAttributes pDomainName_ =
    BatchDeleteAttributes'
    { _bdaDomainName = pDomainName_
    , _bdaItems = mempty
    }

-- | The name of the domain in which the attributes are being deleted.
bdaDomainName :: Lens' BatchDeleteAttributes Text
bdaDomainName = lens _bdaDomainName (\ s a -> s{_bdaDomainName = a});

-- | A list of items on which to perform the operation.
bdaItems :: Lens' BatchDeleteAttributes [DeletableItem]
bdaItems = lens _bdaItems (\ s a -> s{_bdaItems = a});

instance AWSRequest BatchDeleteAttributes where
        type Sv BatchDeleteAttributes = SDB
        type Rs BatchDeleteAttributes =
             BatchDeleteAttributesResponse
        request = post "BatchDeleteAttributes"
        response = receiveNull BatchDeleteAttributesResponse'

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
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'BatchDeleteAttributesResponse' smart constructor.
batchDeleteAttributesResponse :: BatchDeleteAttributesResponse
batchDeleteAttributesResponse = BatchDeleteAttributesResponse'
