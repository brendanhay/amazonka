{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.StorageGateway.DeleteTapeArchive
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Deletes the specified virtual tape from the virtual tape shelf (VTS).
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DeleteTapeArchive.html>
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
import Network.AWS.Request.JSON
import Network.AWS.StorageGateway.Types
import qualified GHC.Exts

newtype DeleteTapeArchive = DeleteTapeArchive
    { _dtaTapeARN :: Text
    } deriving (Eq, Ord, Read, Show, Monoid, IsString)

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

-- | The Amazon Resource Name (ARN) of the virtual tape to delete from the virtual
-- tape shelf (VTS).
dtaTapeARN :: Lens' DeleteTapeArchive Text
dtaTapeARN = lens _dtaTapeARN (\s a -> s { _dtaTapeARN = a })

newtype DeleteTapeArchiveResponse = DeleteTapeArchiveResponse
    { _dtarTapeARN :: Maybe Text
    } deriving (Eq, Ord, Read, Show, Monoid)

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

-- | The Amazon Resource Name (ARN) of the virtual tape that was deleted from the
-- virtual tape shelf (VTS).
dtarTapeARN :: Lens' DeleteTapeArchiveResponse (Maybe Text)
dtarTapeARN = lens _dtarTapeARN (\s a -> s { _dtarTapeARN = a })

instance ToPath DeleteTapeArchive where
    toPath = const "/"

instance ToQuery DeleteTapeArchive where
    toQuery = const mempty

instance ToHeaders DeleteTapeArchive

instance ToJSON DeleteTapeArchive where
    toJSON DeleteTapeArchive{..} = object
        [ "TapeARN" .= _dtaTapeARN
        ]

instance AWSRequest DeleteTapeArchive where
    type Sv DeleteTapeArchive = StorageGateway
    type Rs DeleteTapeArchive = DeleteTapeArchiveResponse

    request  = post "DeleteTapeArchive"
    response = jsonResponse

instance FromJSON DeleteTapeArchiveResponse where
    parseJSON = withObject "DeleteTapeArchiveResponse" $ \o -> DeleteTapeArchiveResponse
        <$> o .:? "TapeARN"
