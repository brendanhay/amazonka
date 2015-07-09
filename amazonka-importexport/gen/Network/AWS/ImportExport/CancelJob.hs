{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.CancelJob
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation cancels a specified job. Only the job owner can cancel
-- it. The operation fails if the job has already started or is complete.
--
-- <http://docs.aws.amazon.com/AWSImportExport/latest/DG/WebCancelJob.html>
module Network.AWS.ImportExport.CancelJob
    (
    -- * Request
      CancelJob
    -- ** Request constructor
    , cancelJob
    -- ** Request lenses
    , canAPIVersion
    , canJobId

    -- * Response
    , CancelJobResponse
    -- ** Response constructor
    , cancelJobResponse
    -- ** Response lenses
    , canSuccess
    , canStatus
    ) where

import           Network.AWS.ImportExport.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input structure for the CancelJob operation.
--
-- /See:/ 'cancelJob' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'canAPIVersion'
--
-- * 'canJobId'
data CancelJob = CancelJob'
    { _canAPIVersion :: !(Maybe Text)
    , _canJobId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelJob' smart constructor.
cancelJob :: Text -> CancelJob
cancelJob pJobId =
    CancelJob'
    { _canAPIVersion = Nothing
    , _canJobId = pJobId
    }

-- | FIXME: Undocumented member.
canAPIVersion :: Lens' CancelJob (Maybe Text)
canAPIVersion = lens _canAPIVersion (\ s a -> s{_canAPIVersion = a});

-- | FIXME: Undocumented member.
canJobId :: Lens' CancelJob Text
canJobId = lens _canJobId (\ s a -> s{_canJobId = a});

instance AWSRequest CancelJob where
        type Sv CancelJob = ImportExport
        type Rs CancelJob = CancelJobResponse
        request = post
        response
          = receiveXMLWrapper "CancelJobResult"
              (\ s h x ->
                 CancelJobResponse' <$>
                   (x .@? "Success") <*> (pure (fromEnum s)))

instance ToHeaders CancelJob where
        toHeaders = const mempty

instance ToPath CancelJob where
        toPath = const "/"

instance ToQuery CancelJob where
        toQuery CancelJob'{..}
          = mconcat
              ["Operation=CancelJob",
               "Action" =: ("CancelJob" :: ByteString),
               "Version" =: ("2010-06-01" :: ByteString),
               "APIVersion" =: _canAPIVersion, "JobId" =: _canJobId]

-- | Output structure for the CancelJob operation.
--
-- /See:/ 'cancelJobResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'canSuccess'
--
-- * 'canStatus'
data CancelJobResponse = CancelJobResponse'
    { _canSuccess :: !(Maybe Bool)
    , _canStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelJobResponse' smart constructor.
cancelJobResponse :: Int -> CancelJobResponse
cancelJobResponse pStatus =
    CancelJobResponse'
    { _canSuccess = Nothing
    , _canStatus = pStatus
    }

-- | FIXME: Undocumented member.
canSuccess :: Lens' CancelJobResponse (Maybe Bool)
canSuccess = lens _canSuccess (\ s a -> s{_canSuccess = a});

-- | FIXME: Undocumented member.
canStatus :: Lens' CancelJobResponse Int
canStatus = lens _canStatus (\ s a -> s{_canStatus = a});
