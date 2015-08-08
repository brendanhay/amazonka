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
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DeleteTapeArchive.html AWS API Reference> for DeleteTapeArchive.
module Network.AWS.StorageGateway.DeleteTapeArchive
    (
    -- * Creating a Request
      DeleteTapeArchive
    , deleteTapeArchive
    -- * Request Lenses
    , dtaTapeARN

    -- * Destructuring the Response
    , DeleteTapeArchiveResponse
    , deleteTapeArchiveResponse
    -- * Response Lenses
    , dtatrsTapeARN
    , dtatrsStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTapeArchive' smart constructor.
deleteTapeArchive :: Text -> DeleteTapeArchive
deleteTapeArchive pTapeARN_ =
    DeleteTapeArchive'
    { _dtaTapeARN = pTapeARN_
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
-- * 'dtatrsTapeARN'
--
-- * 'dtatrsStatus'
data DeleteTapeArchiveResponse = DeleteTapeArchiveResponse'
    { _dtatrsTapeARN :: !(Maybe Text)
    , _dtatrsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteTapeArchiveResponse' smart constructor.
deleteTapeArchiveResponse :: Int -> DeleteTapeArchiveResponse
deleteTapeArchiveResponse pStatus_ =
    DeleteTapeArchiveResponse'
    { _dtatrsTapeARN = Nothing
    , _dtatrsStatus = pStatus_
    }

-- | The Amazon Resource Name (ARN) of the virtual tape that was deleted from
-- the virtual tape shelf (VTS).
dtatrsTapeARN :: Lens' DeleteTapeArchiveResponse (Maybe Text)
dtatrsTapeARN = lens _dtatrsTapeARN (\ s a -> s{_dtatrsTapeARN = a});

-- | Undocumented member.
dtatrsStatus :: Lens' DeleteTapeArchiveResponse Int
dtatrsStatus = lens _dtatrsStatus (\ s a -> s{_dtatrsStatus = a});
