{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.SDB.BatchDeleteAttributes
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
-- requests, which generally yields better throughput. The following
-- limitations are enforced for this operation: 1 MB request size 25 item
-- limit per BatchDeleteAttributes operation.
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

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SDB.Types
import qualified GHC.Exts

data BatchDeleteAttributes = BatchDeleteAttributes
    { _bdaDomainName :: Text
    , _bdaItems      :: [DeletableItem]
    } deriving (Eq, Show, Generic)

-- | 'BatchDeleteAttributes' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'bdaDomainName' @::@ 'Text'
--
-- * 'bdaItems' @::@ ['DeletableItem']
--
batchDeleteAttributes :: Text -- ^ 'bdaDomainName'
                      -> BatchDeleteAttributes
batchDeleteAttributes p1 = BatchDeleteAttributes
    { _bdaDomainName = p1
    , _bdaItems      = mempty
    }

-- | The name of the domain in which the attributes are being deleted.
bdaDomainName :: Lens' BatchDeleteAttributes Text
bdaDomainName = lens _bdaDomainName (\s a -> s { _bdaDomainName = a })

-- | A list of items on which to perform the operation.
bdaItems :: Lens' BatchDeleteAttributes [DeletableItem]
bdaItems = lens _bdaItems (\s a -> s { _bdaItems = a })

instance ToQuery BatchDeleteAttributes

instance ToPath BatchDeleteAttributes where
    toPath = const "/"

data BatchDeleteAttributesResponse = BatchDeleteAttributesResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'BatchDeleteAttributesResponse' constructor.
batchDeleteAttributesResponse :: BatchDeleteAttributesResponse
batchDeleteAttributesResponse = BatchDeleteAttributesResponse

instance AWSRequest BatchDeleteAttributes where
    type Sv BatchDeleteAttributes = SDB
    type Rs BatchDeleteAttributes = BatchDeleteAttributesResponse

    request  = post "BatchDeleteAttributes"
    response = nullaryResponse BatchDeleteAttributesResponse
