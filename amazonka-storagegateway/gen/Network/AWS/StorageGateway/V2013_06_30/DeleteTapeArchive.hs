{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.V2013_06_30.DeleteTapeArchive
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.StorageGateway.V2013_06_30.DeleteTapeArchive
    (
    -- * Request
      DeleteTapeArchive
    -- ** Request constructor
    , deleteTapeArchive
    -- ** Request lenses
    , dtaiTapeARN

    -- * Response
    , DeleteTapeArchiveResponse
    -- ** Response lenses
    , dtaoTapeARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'DeleteTapeArchive' request.
deleteTapeArchive :: Text -- ^ 'dtaiTapeARN'
                  -> DeleteTapeArchive
deleteTapeArchive p1 = DeleteTapeArchive
    { _dtaiTapeARN = p1
    }

data DeleteTapeArchive = DeleteTapeArchive
    { _dtaiTapeARN :: Text
    } deriving (Show, Generic)

dtaiTapeARN
    :: Functor f
    => (Text
    -> f (Text))
    -> DeleteTapeArchive
    -> f DeleteTapeArchive
dtaiTapeARN f x =
    (\y -> x { _dtaiTapeARN = y })
       <$> f (_dtaiTapeARN x)
{-# INLINE dtaiTapeARN #-}

instance ToPath DeleteTapeArchive

instance ToQuery DeleteTapeArchive

instance ToHeaders DeleteTapeArchive

instance ToJSON DeleteTapeArchive

data DeleteTapeArchiveResponse = DeleteTapeArchiveResponse
    { _dtaoTapeARN :: Maybe Text
    } deriving (Show, Generic)

dtaoTapeARN
    :: Functor f
    => (Maybe Text
    -> f (Maybe Text))
    -> DeleteTapeArchiveResponse
    -> f DeleteTapeArchiveResponse
dtaoTapeARN f x =
    (\y -> x { _dtaoTapeARN = y })
       <$> f (_dtaoTapeARN x)
{-# INLINE dtaoTapeARN #-}

instance FromJSON DeleteTapeArchiveResponse

instance AWSRequest DeleteTapeArchive where
    type Sv DeleteTapeArchive = StorageGateway
    type Rs DeleteTapeArchive = DeleteTapeArchiveResponse

    request = get
    response _ = jsonResponse
