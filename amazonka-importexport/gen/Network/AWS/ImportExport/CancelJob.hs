{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ImportExport.CancelJob
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation cancels a specified job. Only the job owner can cancel
-- it. The operation fails if the job has already started or is complete.
--
-- /See:/ <http://docs.aws.amazon.com/AWSImportExport/latest/DG/WebCancelJob.html AWS API Reference> for CancelJob.
module Network.AWS.ImportExport.CancelJob
    (
    -- * Creating a Request
      cancelJob
    , CancelJob
    -- * Request Lenses
    , cAPIVersion
    , cJobId

    -- * Destructuring the Response
    , cancelJobResponse
    , CancelJobResponse
    -- * Response Lenses
    , crsSuccess
    , crsStatus
    ) where

import           Network.AWS.ImportExport.Types
import           Network.AWS.ImportExport.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Input structure for the CancelJob operation.
--
-- /See:/ 'cancelJob' smart constructor.
data CancelJob = CancelJob'
    { _cAPIVersion :: !(Maybe Text)
    , _cJobId      :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CancelJob' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cAPIVersion'
--
-- * 'cJobId'
cancelJob
    :: Text -- ^ 'cJobId'
    -> CancelJob
cancelJob pJobId_ =
    CancelJob'
    { _cAPIVersion = Nothing
    , _cJobId = pJobId_
    }

-- | Undocumented member.
cAPIVersion :: Lens' CancelJob (Maybe Text)
cAPIVersion = lens _cAPIVersion (\ s a -> s{_cAPIVersion = a});

-- | Undocumented member.
cJobId :: Lens' CancelJob Text
cJobId = lens _cJobId (\ s a -> s{_cJobId = a});

instance AWSRequest CancelJob where
        type Rs CancelJob = CancelJobResponse
        request = postQuery importExport
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
               "APIVersion" =: _cAPIVersion, "JobId" =: _cJobId]

-- | Output structure for the CancelJob operation.
--
-- /See:/ 'cancelJobResponse' smart constructor.
data CancelJobResponse = CancelJobResponse'
    { _crsSuccess :: !(Maybe Bool)
    , _crsStatus  :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'CancelJobResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'crsSuccess'
--
-- * 'crsStatus'
cancelJobResponse
    :: Int -- ^ 'crsStatus'
    -> CancelJobResponse
cancelJobResponse pStatus_ =
    CancelJobResponse'
    { _crsSuccess = Nothing
    , _crsStatus = pStatus_
    }

-- | Undocumented member.
crsSuccess :: Lens' CancelJobResponse (Maybe Bool)
crsSuccess = lens _crsSuccess (\ s a -> s{_crsSuccess = a});

-- | The response status code.
crsStatus :: Lens' CancelJobResponse Int
crsStatus = lens _crsStatus (\ s a -> s{_crsStatus = a});
