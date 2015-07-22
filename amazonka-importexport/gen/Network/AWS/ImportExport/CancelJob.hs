{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    , crqAPIVersion
    , crqJobId

    -- * Response
    , CancelJobResponse
    -- ** Response constructor
    , cancelJobResponse
    -- ** Response lenses
    , crsSuccess
    , crsStatus
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
-- * 'crqAPIVersion'
--
-- * 'crqJobId'
data CancelJob = CancelJob'
    { _crqAPIVersion :: !(Maybe Text)
    , _crqJobId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelJob' smart constructor.
cancelJob :: Text -> CancelJob
cancelJob pJobId =
    CancelJob'
    { _crqAPIVersion = Nothing
    , _crqJobId = pJobId
    }

-- | FIXME: Undocumented member.
crqAPIVersion :: Lens' CancelJob (Maybe Text)
crqAPIVersion = lens _crqAPIVersion (\ s a -> s{_crqAPIVersion = a});

-- | FIXME: Undocumented member.
crqJobId :: Lens' CancelJob Text
crqJobId = lens _crqJobId (\ s a -> s{_crqJobId = a});

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
               "APIVersion" =: _crqAPIVersion, "JobId" =: _crqJobId]

-- | Output structure for the CancelJob operation.
--
-- /See:/ 'cancelJobResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'crsSuccess'
--
-- * 'crsStatus'
data CancelJobResponse = CancelJobResponse'
    { _crsSuccess :: !(Maybe Bool)
    , _crsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelJobResponse' smart constructor.
cancelJobResponse :: Int -> CancelJobResponse
cancelJobResponse pStatus =
    CancelJobResponse'
    { _crsSuccess = Nothing
    , _crsStatus = pStatus
    }

-- | FIXME: Undocumented member.
crsSuccess :: Lens' CancelJobResponse (Maybe Bool)
crsSuccess = lens _crsSuccess (\ s a -> s{_crsSuccess = a});

-- | FIXME: Undocumented member.
crsStatus :: Lens' CancelJobResponse Int
crsStatus = lens _crsStatus (\ s a -> s{_crsStatus = a});
