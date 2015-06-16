{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

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

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.StorageGateway.Types

-- | /See:/ 'deleteTapeArchive' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtaTapeARN'
newtype DeleteTapeArchive = DeleteTapeArchive'{_dtaTapeARN :: Text} deriving (Eq, Read, Show)

-- | 'DeleteTapeArchive' smart constructor.
deleteTapeArchive :: Text -> DeleteTapeArchive
deleteTapeArchive pTapeARN = DeleteTapeArchive'{_dtaTapeARN = pTapeARN};

-- | The Amazon Resource Name (ARN) of the virtual tape to delete from the
-- virtual tape shelf (VTS).
dtaTapeARN :: Lens' DeleteTapeArchive Text
dtaTapeARN = lens _dtaTapeARN (\ s a -> s{_dtaTapeARN = a});

instance AWSRequest DeleteTapeArchive where
        type Sv DeleteTapeArchive = StorageGateway
        type Rs DeleteTapeArchive = DeleteTapeArchiveResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteTapeArchiveResponse' <$> (x .?> "TapeARN"))

instance ToHeaders DeleteTapeArchive where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DeleteTapeArchive" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteTapeArchive where
        toJSON DeleteTapeArchive'{..}
          = object ["TapeARN" .= _dtaTapeARN]

instance ToPath DeleteTapeArchive where
        toPath = const "/"

instance ToQuery DeleteTapeArchive where
        toQuery = const mempty

-- | /See:/ 'deleteTapeArchiveResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtarTapeARN'
newtype DeleteTapeArchiveResponse = DeleteTapeArchiveResponse'{_dtarTapeARN :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DeleteTapeArchiveResponse' smart constructor.
deleteTapeArchiveResponse :: DeleteTapeArchiveResponse
deleteTapeArchiveResponse = DeleteTapeArchiveResponse'{_dtarTapeARN = Nothing};

-- | The Amazon Resource Name (ARN) of the virtual tape that was deleted from
-- the virtual tape shelf (VTS).
dtarTapeARN :: Lens' DeleteTapeArchiveResponse (Maybe Text)
dtarTapeARN = lens _dtarTapeARN (\ s a -> s{_dtarTapeARN = a});
