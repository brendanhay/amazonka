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
    , mkDeleteTapeArchiveInput
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

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteTapeArchive' request.
mkDeleteTapeArchiveInput :: Text -- ^ 'dtaiTapeARN'
                         -> DeleteTapeArchive
mkDeleteTapeArchiveInput p1 = DeleteTapeArchive
    { _dtaiTapeARN = p1
    }
{-# INLINE mkDeleteTapeArchiveInput #-}

newtype DeleteTapeArchive = DeleteTapeArchive
    { _dtaiTapeARN :: Text
    } deriving (Show, Generic)

dtaiTapeARN :: Lens' DeleteTapeArchive (Text)
dtaiTapeARN = lens _dtaiTapeARN (\s a -> s { _dtaiTapeARN = a })
{-# INLINE dtaiTapeARN #-}

instance ToPath DeleteTapeArchive

instance ToQuery DeleteTapeArchive

instance ToHeaders DeleteTapeArchive

instance ToJSON DeleteTapeArchive

newtype DeleteTapeArchiveResponse = DeleteTapeArchiveResponse
    { _dtaoTapeARN :: Maybe Text
    } deriving (Show, Generic)

dtaoTapeARN :: Lens' DeleteTapeArchiveResponse (Maybe Text)
dtaoTapeARN = lens _dtaoTapeARN (\s a -> s { _dtaoTapeARN = a })
{-# INLINE dtaoTapeARN #-}

instance FromJSON DeleteTapeArchiveResponse

instance AWSRequest DeleteTapeArchive where
    type Sv DeleteTapeArchive = StorageGateway
    type Rs DeleteTapeArchive = DeleteTapeArchiveResponse

    request = get
    response _ = jsonResponse
