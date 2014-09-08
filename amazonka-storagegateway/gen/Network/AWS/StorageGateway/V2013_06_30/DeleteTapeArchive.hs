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
    -- ** Response constructor
    , mkDeleteTapeArchiveResponse
    -- ** Response lenses
    , dtarTapeARN
    ) where

import Network.AWS.StorageGateway.V2013_06_30.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

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

dtaTapeARN :: Lens' DeleteTapeArchive Text
dtaTapeARN = lens _dtaTapeARN (\s a -> s { _dtaTapeARN = a })

instance ToPath DeleteTapeArchive

instance ToQuery DeleteTapeArchive

instance ToHeaders DeleteTapeArchive

instance ToJSON DeleteTapeArchive

newtype DeleteTapeArchiveResponse = DeleteTapeArchiveResponse
    { _dtarTapeARN :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DeleteTapeArchiveResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDeleteTapeArchiveResponse :: DeleteTapeArchiveResponse
mkDeleteTapeArchiveResponse = DeleteTapeArchiveResponse
    { _dtarTapeARN = Nothing
    }

dtarTapeARN :: Lens' DeleteTapeArchiveResponse (Maybe Text)
dtarTapeARN = lens _dtarTapeARN (\s a -> s { _dtarTapeARN = a })

instance FromJSON DeleteTapeArchiveResponse

instance AWSRequest DeleteTapeArchive where
    type Sv DeleteTapeArchive = StorageGateway
    type Rs DeleteTapeArchive = DeleteTapeArchiveResponse

    request = get
    response _ = jsonResponse
