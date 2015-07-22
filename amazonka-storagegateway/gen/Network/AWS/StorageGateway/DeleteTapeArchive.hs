{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteTapeArchive
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified virtual tape from the virtual tape shelf (VTS).
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DeleteTapeArchive.html>
module Network.AWS.StorageGateway.DeleteTapeArchive
    (
    -- * Request
      DeleteTapeArchive
    -- ** Request constructor
    , deleteTapeArchive
    -- ** Request lenses
    , dtarqTapeARN

    -- * Response
    , DeleteTapeArchiveResponse
    -- ** Response constructor
    , deleteTapeArchiveResponse
    -- ** Response lenses
    , dtaarsTapeARN
    , dtaarsStatus
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
-- * 'dtarqTapeARN'
newtype DeleteTapeArchive = DeleteTapeArchive'
    { _dtarqTapeARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTapeArchive' smart constructor.
deleteTapeArchive :: Text -> DeleteTapeArchive
deleteTapeArchive pTapeARN =
    DeleteTapeArchive'
    { _dtarqTapeARN = pTapeARN
    }

-- | The Amazon Resource Name (ARN) of the virtual tape to delete from the
-- virtual tape shelf (VTS).
dtarqTapeARN :: Lens' DeleteTapeArchive Text
dtarqTapeARN = lens _dtarqTapeARN (\ s a -> s{_dtarqTapeARN = a});

instance AWSRequest DeleteTapeArchive where
        type Sv DeleteTapeArchive = StorageGateway
        type Rs DeleteTapeArchive = DeleteTapeArchiveResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteTapeArchiveResponse' <$>
                   (x .?> "TapeARN") <*> (pure (fromEnum s)))

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
          = object ["TapeARN" .= _dtarqTapeARN]

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
-- * 'dtaarsTapeARN'
--
-- * 'dtaarsStatus'
data DeleteTapeArchiveResponse = DeleteTapeArchiveResponse'
    { _dtaarsTapeARN :: !(Maybe Text)
    , _dtaarsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTapeArchiveResponse' smart constructor.
deleteTapeArchiveResponse :: Int -> DeleteTapeArchiveResponse
deleteTapeArchiveResponse pStatus =
    DeleteTapeArchiveResponse'
    { _dtaarsTapeARN = Nothing
    , _dtaarsStatus = pStatus
    }

-- | The Amazon Resource Name (ARN) of the virtual tape that was deleted from
-- the virtual tape shelf (VTS).
dtaarsTapeARN :: Lens' DeleteTapeArchiveResponse (Maybe Text)
dtaarsTapeARN = lens _dtaarsTapeARN (\ s a -> s{_dtaarsTapeARN = a});

-- | FIXME: Undocumented member.
dtaarsStatus :: Lens' DeleteTapeArchiveResponse Int
dtaarsStatus = lens _dtaarsStatus (\ s a -> s{_dtaarsStatus = a});
