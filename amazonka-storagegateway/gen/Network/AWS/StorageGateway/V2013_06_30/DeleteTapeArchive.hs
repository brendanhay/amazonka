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
    , mkDeleteTapeArchive
    -- ** Request lenses
    , dtaTapeARN

    -- * Response
    , DeleteTapeArchiveResponse
    -- ** Response lenses
    , dtarsTapeARN
    ) where

import           Network.AWS.StorageGateway.V2013_06_30.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

newtype DeleteTapeArchive = DeleteTapeArchive
    { _dtaTapeARN :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteTapeArchive' request.
mkDeleteTapeArchive :: Text -- ^ 'dtaTapeARN'
                    -> DeleteTapeArchive
mkDeleteTapeArchive p1 = DeleteTapeArchive
    { _dtaTapeARN = p1
    }
{-# INLINE mkDeleteTapeArchive #-}

dtaTapeARN :: Lens' DeleteTapeArchive Text
dtaTapeARN = lens _dtaTapeARN (\s a -> s { _dtaTapeARN = a })
{-# INLINE dtaTapeARN #-}

instance ToPath DeleteTapeArchive

instance ToQuery DeleteTapeArchive

instance ToHeaders DeleteTapeArchive

instance ToJSON DeleteTapeArchive

newtype DeleteTapeArchiveResponse = DeleteTapeArchiveResponse
    { _dtarsTapeARN :: Maybe Text
    } deriving (Show, Generic)

dtarsTapeARN :: Lens' DeleteTapeArchiveResponse (Maybe Text)
dtarsTapeARN = lens _dtarsTapeARN (\s a -> s { _dtarsTapeARN = a })
{-# INLINE dtarsTapeARN #-}

instance FromJSON DeleteTapeArchiveResponse

instance AWSRequest DeleteTapeArchive where
    type Sv DeleteTapeArchive = StorageGateway
    type Rs DeleteTapeArchive = DeleteTapeArchiveResponse

    request = get
    response _ = jsonResponse
