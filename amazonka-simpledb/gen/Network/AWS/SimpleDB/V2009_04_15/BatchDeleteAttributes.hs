{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SimpleDB.V2009_04_15.BatchDeleteAttributes
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
module Network.AWS.SimpleDB.V2009_04_15.BatchDeleteAttributes
    (
    -- * Request
      BatchDeleteAttributes
    -- ** Request constructor
    , batchDeleteAttributes
    -- ** Request lenses
    , bdarItems
    , bdarDomainName

    -- * Response
    , BatchDeleteAttributesResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.SimpleDB.V2009_04_15.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'BatchDeleteAttributes' request.
batchDeleteAttributes :: [DeletableItem] -- ^ 'bdarItems'
                      -> Text -- ^ 'bdarDomainName'
                      -> BatchDeleteAttributes
batchDeleteAttributes p1 p2 = BatchDeleteAttributes
    { _bdarItems = p1
    , _bdarDomainName = p2
    }
{-# INLINE batchDeleteAttributes #-}

data BatchDeleteAttributes = BatchDeleteAttributes
    { _bdarItems :: [DeletableItem]
      -- ^ A list of items on which to perform the operation.
    , _bdarDomainName :: Text
      -- ^ The name of the domain in which the attributes are being deleted.
    } deriving (Show, Generic)

-- | A list of items on which to perform the operation.
bdarItems :: Lens' BatchDeleteAttributes ([DeletableItem])
bdarItems f x =
    f (_bdarItems x)
        <&> \y -> x { _bdarItems = y }
{-# INLINE bdarItems #-}

-- | The name of the domain in which the attributes are being deleted.
bdarDomainName :: Lens' BatchDeleteAttributes (Text)
bdarDomainName f x =
    f (_bdarDomainName x)
        <&> \y -> x { _bdarDomainName = y }
{-# INLINE bdarDomainName #-}

instance ToQuery BatchDeleteAttributes where
    toQuery = genericQuery def

data BatchDeleteAttributesResponse = BatchDeleteAttributesResponse
    deriving (Eq, Show, Generic)

instance AWSRequest BatchDeleteAttributes where
    type Sv BatchDeleteAttributes = SimpleDB
    type Rs BatchDeleteAttributes = BatchDeleteAttributesResponse

    request = post "BatchDeleteAttributes"
    response _ = nullaryResponse BatchDeleteAttributesResponse
