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

-- Module      : Network.AWS.StorageGateway.DeleteTapeArchive
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Deletes the specified virtual tape from the virtual tape shelf (VTS).
module Network.AWS.StorageGateway.DeleteTapeArchive
    (
    -- * Request
      DeleteTapeArchive
    -- ** Request constructor
    , deleteTapeArchive
    -- ** Request lenses
    , dtaTapeARN

    -- * Response
    , DeleteTapeArchiveResponse
    -- ** Response constructor
    , deleteTapeArchiveResponse
    -- ** Response lenses
    , dtarTapeARN
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.StorageGateway.Types

newtype DeleteTapeArchive = DeleteTapeArchive
    { _dtaTapeARN :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DeleteTapeArchive' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtaTapeARN' @::@ 'Text'
--
deleteTapeArchive :: Text -- ^ 'dtaTapeARN'
                  -> DeleteTapeArchive
deleteTapeArchive p1 = DeleteTapeArchive
    { _dtaTapeARN = p1
    }

-- | The Amazon Resource Name (ARN) of the virtual tape to delete from the
-- virtual tape shelf (VTS).
dtaTapeARN :: Lens' DeleteTapeArchive Text
dtaTapeARN = lens _dtaTapeARN (\s a -> s { _dtaTapeARN = a })

instance ToPath DeleteTapeArchive where
    toPath = const "/"

instance ToQuery DeleteTapeArchive where
    toQuery = const mempty

instance ToHeaders DeleteTapeArchive

instance ToBody DeleteTapeArchive where
    toBody = toBody . encode . _dtaTapeARN

newtype DeleteTapeArchiveResponse = DeleteTapeArchiveResponse
    { _dtarTapeARN :: Maybe Text
    } deriving (Eq, Ord, Show, Generic, Monoid)

-- | 'DeleteTapeArchiveResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtarTapeARN' @::@ 'Maybe' 'Text'
--
deleteTapeArchiveResponse :: DeleteTapeArchiveResponse
deleteTapeArchiveResponse = DeleteTapeArchiveResponse
    { _dtarTapeARN = Nothing
    }

-- | The Amazon Resource Name (ARN) of the virtual tape that was deleted from
-- the virtual tape shelf (VTS).
dtarTapeARN :: Lens' DeleteTapeArchiveResponse (Maybe Text)
dtarTapeARN = lens _dtarTapeARN (\s a -> s { _dtarTapeARN = a })

-- FromJSON

instance AWSRequest DeleteTapeArchive where
    type Sv DeleteTapeArchive = StorageGateway
    type Rs DeleteTapeArchive = DeleteTapeArchiveResponse

    request  = post'
    response = jsonResponse $ \h o -> DeleteTapeArchiveResponse
        <$> o .: "TapeARN"
