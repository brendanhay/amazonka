{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SimpleDB.BatchDeleteAttributes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Performs multiple DeleteAttributes operations in a single call, which
-- reduces round trips and latencies. This enables Amazon SimpleDB to optimize
-- requests, which generally yields better throughput. If you specify
-- BatchDeleteAttributes without attributes or values, all the attributes for
-- the item are deleted. BatchDeleteAttributes is an idempotent operation;
-- running it multiple times on the same item or attribute doesn't result in
-- an error. The BatchDeleteAttributes operation succeeds or fails in its
-- entirety. There are no partial deletes. You can execute multiple
-- BatchDeleteAttributes operations and other operations in parallel. However,
-- large numbers of concurrent BatchDeleteAttributes calls can result in
-- Service Unavailable (503) responses. This operation is vulnerable to
-- exceeding the maximum URL size when making a REST request using the HTTP
-- GET method. This operation does not support conditions using
-- Expected.X.Name, Expected.X.Value, or Expected.X.Exists. The following
-- limitations are enforced for this operation: 1 MB request size 25 item
-- limit per BatchDeleteAttributes operation.
module Network.AWS.SimpleDB.BatchDeleteAttributes
    (
    -- * Request
      BatchDeleteAttributes
    -- ** Request constructor
    , mkBatchDeleteAttributes
    -- ** Request lenses
    , bdaDomainName
    , bdaItems

    -- * Response
    , BatchDeleteAttributesResponse
    -- ** Response constructor
    , mkBatchDeleteAttributesResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SimpleDB.Types
import Network.AWS.Prelude

data BatchDeleteAttributes = BatchDeleteAttributes
    { _bdaDomainName :: Text
    , _bdaItems :: [DeletableItem]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'BatchDeleteAttributes' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
-- * @Items ::@ @[DeletableItem]@
--
mkBatchDeleteAttributes :: Text -- ^ 'bdaDomainName'
                        -> [DeletableItem] -- ^ 'bdaItems'
                        -> BatchDeleteAttributes
mkBatchDeleteAttributes p1 p2 = BatchDeleteAttributes
    { _bdaDomainName = p1
    , _bdaItems = p2
    }

-- | The name of the domain in which the attributes are being deleted.
bdaDomainName :: Lens' BatchDeleteAttributes Text
bdaDomainName = lens _bdaDomainName (\s a -> s { _bdaDomainName = a })

-- | A list of items on which to perform the operation.
bdaItems :: Lens' BatchDeleteAttributes [DeletableItem]
bdaItems = lens _bdaItems (\s a -> s { _bdaItems = a })

instance ToQuery BatchDeleteAttributes where
    toQuery = genericQuery def

data BatchDeleteAttributesResponse = BatchDeleteAttributesResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'BatchDeleteAttributesResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkBatchDeleteAttributesResponse :: BatchDeleteAttributesResponse
mkBatchDeleteAttributesResponse = BatchDeleteAttributesResponse

instance AWSRequest BatchDeleteAttributes where
    type Sv BatchDeleteAttributes = SimpleDB
    type Rs BatchDeleteAttributes = BatchDeleteAttributesResponse

    request = post "BatchDeleteAttributes"
    response _ = nullaryResponse BatchDeleteAttributesResponse
