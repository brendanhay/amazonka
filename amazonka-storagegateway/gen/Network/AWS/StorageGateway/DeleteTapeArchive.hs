{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

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
    , dtar1TapeARN
    , dtar1Status
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | DeleteTapeArchiveInput
--
-- /See:/ 'deleteTapeArchive' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtaTapeARN'
newtype DeleteTapeArchive = DeleteTapeArchive'
    { _dtaTapeARN :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteTapeArchive' smart constructor.
deleteTapeArchive :: Text -> DeleteTapeArchive
deleteTapeArchive pTapeARN =
    DeleteTapeArchive'
    { _dtaTapeARN = pTapeARN
    }

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
                 DeleteTapeArchiveResponse' <$>
                   (x .?> "TapeARN") <*> (pure s))

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

-- | DeleteTapeArchiveOutput
--
-- /See:/ 'deleteTapeArchiveResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dtar1TapeARN'
--
-- * 'dtar1Status'
data DeleteTapeArchiveResponse = DeleteTapeArchiveResponse'
    { _dtar1TapeARN :: !(Maybe Text)
    , _dtar1Status  :: !Status
    } deriving (Eq,Read,Show)

-- | 'DeleteTapeArchiveResponse' smart constructor.
deleteTapeArchiveResponse :: Status -> DeleteTapeArchiveResponse
deleteTapeArchiveResponse pStatus =
    DeleteTapeArchiveResponse'
    { _dtar1TapeARN = Nothing
    , _dtar1Status = pStatus
    }

-- | The Amazon Resource Name (ARN) of the virtual tape that was deleted from
-- the virtual tape shelf (VTS).
dtar1TapeARN :: Lens' DeleteTapeArchiveResponse (Maybe Text)
dtar1TapeARN = lens _dtar1TapeARN (\ s a -> s{_dtar1TapeARN = a});

-- | FIXME: Undocumented member.
dtar1Status :: Lens' DeleteTapeArchiveResponse Status
dtar1Status = lens _dtar1Status (\ s a -> s{_dtar1Status = a});
